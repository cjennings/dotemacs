;;; ai-vterm.el --- In-Emacs Claude launcher with vertical-split vterm -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Picks a Claude-template project (a dir under ~/.emacs.d, ~/code/*, or
;; ~/projects/* containing .ai/protocols.org), opens or reuses a vterm
;; buffer named "claude [<basename>]", sends Claude Code's startup
;; instruction to it, and routes the buffer to a right-side window via
;; display-buffer-alist.  Multiple projects produce multiple coexisting
;; buffers that share the same right-side slot; switching among them is a
;; buffer-switch, not a kill-and-recreate.
;;
;; Existing windmove (Shift-arrows) handles code <-> Claude focus
;; toggling.  Buffer-move (C-M-arrows) handles side-swap.  Neither
;; needs anything new from this module.

;;; Code:

(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function vterm-send-return "vterm" ())

(defgroup ai-vterm nil
  "In-Emacs Claude launcher with vertical-split vterm."
  :group 'tools)

(defcustom cj/ai-vterm-claude-command
  "claude \"Read .ai/protocols.org and follow all instructions.\""
  "Shell command sent to a fresh AI-vterm to start Claude Code."
  :type 'string
  :group 'ai-vterm)

(defvar cj/--ai-vterm-suppress-tmux nil
  "When non-nil, the generic vterm tmux-launch hook skips its auto-tmux step.

ai-vterm dynamically binds this around `(vterm)' so the hook in
eshell-vterm-config.el doesn't send a bare \"tmux\\n\" before the named
session launch command runs.  The hook reads the variable via
`bound-and-true-p' so loading order between the two modules doesn't
matter.")

(defcustom cj/ai-vterm-project-roots
  (list (expand-file-name "~/.emacs.d"))
  "Directories that are themselves Claude-template projects.
Each entry is included as a candidate when it exists and contains
.ai/protocols.org.  Use this for single-project roots like ~/.emacs.d."
  :type '(repeat directory)
  :group 'ai-vterm)

(defcustom cj/ai-vterm-container-roots
  (list (expand-file-name "~/code")
        (expand-file-name "~/projects"))
  "Directories whose immediate children are scanned for Claude projects.
Each entry's child directories are included as candidates when they
contain .ai/protocols.org.  Use this for container dirs like ~/code."
  :type '(repeat directory)
  :group 'ai-vterm)

(defun cj/--ai-vterm-buffer-name (dir)
  "Return the AI-vterm buffer name for project directory DIR.

The name pattern is \"claude [<basename>]\".  The display-buffer-alist
rule keys on the literal prefix \"claude [\", so changing the format
breaks routing to the right-side window."
  (format "claude [%s]"
          (file-name-nondirectory (directory-file-name dir))))

(defun cj/--ai-vterm-tmux-session-name (dir)
  "Return the tmux name derived from project directory DIR.

The basename of DIR, with any run of whitespace collapsed to a single
hyphen so the result is safe to pass on a tmux command line."
  (replace-regexp-in-string
   "[[:space:]]+" "-"
   (file-name-nondirectory (directory-file-name dir))))

(defun cj/--ai-vterm-launch-command (dir)
  "Return the shell command line that runs Claude in a project tmux session.

Uses `tmux new-session -A' so a second F9 on the same project reattaches
to the running session instead of spawning a new one.  The session name
is the project's basename via `cj/--ai-vterm-tmux-session-name'.

The shell command run on first creation is
  <claude-command>; exec bash
so the tmux window survives Claude exiting -- the session stays alive
with a bare bash prompt for recovery, and reattach works the same way."
  (let ((session (cj/--ai-vterm-tmux-session-name dir))
        (start-dir (expand-file-name dir)))
    (format "tmux new-session -A -s %s -c %s '%s'"
            (shell-quote-argument session)
            (shell-quote-argument start-dir)
            (concat cj/ai-vterm-claude-command "; exec bash"))))

(defun cj/--ai-vterm-has-marker-p (dir)
  "Return non-nil when DIR contains .ai/protocols.org."
  (file-exists-p (expand-file-name ".ai/protocols.org" dir)))

(defun cj/--ai-vterm-candidates ()
  "Return the list of Claude-template project paths.

Each entry of `cj/ai-vterm-project-roots' contributes itself when it
exists and contains .ai/protocols.org.  Each entry of
`cj/ai-vterm-container-roots' contributes its immediate child
directories that contain .ai/protocols.org.

Returns absolute paths.  Nonexistent roots are skipped silently."
  (let (result)
    (dolist (root cj/ai-vterm-project-roots)
      (let ((expanded (expand-file-name root)))
        (when (and (file-directory-p expanded)
                   (cj/--ai-vterm-has-marker-p expanded))
          (push expanded result))))
    (dolist (root cj/ai-vterm-container-roots)
      (let ((expanded (expand-file-name root)))
        (when (file-directory-p expanded)
          (dolist (child (directory-files
                          expanded t directory-files-no-dot-files-regexp t))
            (when (and (file-directory-p child)
                       (cj/--ai-vterm-has-marker-p child))
              (push child result))))))
    (nreverse result)))

(defun cj/--ai-vterm-process-live-p (buffer)
  "Return non-nil when BUFFER has a live process attached."
  (let ((proc (get-buffer-process buffer)))
    (and proc (process-live-p proc))))

(defcustom cj/ai-vterm-window-width 0.5
  "Fraction of frame width allocated to the AI-vterm side window."
  :type 'number
  :group 'ai-vterm)

(defun cj/--ai-vterm-display-rule-list ()
  "Return the `display-buffer-alist' entry list installed by this module.

The single rule routes any buffer whose name starts with \"claude [\"
through three actions in order:

1. `display-buffer-reuse-window' -- if the buffer is already visible
   in any window, focus that one.
2. `display-buffer-use-some-window' -- otherwise, reuse an existing
   non-selected window (the right window of a left/right split, in
   the typical layout).
3. `display-buffer-in-direction' -- otherwise, split the selected
   window to the right at width `cj/ai-vterm-window-width'.

`display-buffer-in-side-window' is avoided deliberately.  Side windows
enforce dedication, which breaks `buffer-move' (C-M-arrows) and
`switch-to-buffer' replacement.  The chain above keeps the resulting
window an ordinary window so all the standard window commands work."
  `(("\\`claude \\["
     (display-buffer-reuse-window
      display-buffer-use-some-window
      display-buffer-in-direction)
     (direction . right)
     (window-width . ,cj/ai-vterm-window-width)
     (inhibit-same-window . t))))

(dolist (entry (cj/--ai-vterm-display-rule-list))
  (add-to-list 'display-buffer-alist entry))

(defun cj/--ai-vterm-show-or-create (dir name)
  "Show or create the AI-vterm buffer for project DIR with buffer NAME.

If a buffer named NAME exists with a live process, display it.  If
the buffer exists but its process is dead, kill it and recreate.  If
no such buffer exists, create a new vterm in DIR and send the
project's tmux launch command (see `cj/--ai-vterm-launch-command') so
the same project basename reattaches across Emacs restarts.

The dynamic binding of `cj/--ai-vterm-suppress-tmux' around `(vterm)'
suppresses the generic tmux-launch hook in eshell-vterm-config.el so
it doesn't fire a bare \"tmux\\n\" before the project-named launch
command runs.

Returns the buffer."
  (let ((existing (get-buffer name)))
    (cond
     ((and existing (cj/--ai-vterm-process-live-p existing))
      (display-buffer existing)
      existing)
     (t
      (when existing
        (kill-buffer existing))
      (let ((default-directory dir)
            (cj/--ai-vterm-suppress-tmux t))
        (vterm name))
      (let ((buf (get-buffer name)))
        (with-current-buffer buf
          (vterm-send-string (cj/--ai-vterm-launch-command dir))
          (vterm-send-return))
        (display-buffer buf)
        buf)))))

(defun cj/--ai-vterm-pick-project ()
  "Prompt for a Claude-template project; return its absolute path.

Candidates come from `cj/--ai-vterm-candidates'.  Display uses
`abbreviate-file-name' so paths read as ~/code/foo instead of the
full home-dir form.  Signals `user-error' when no candidates exist."
  (let ((candidates (cj/--ai-vterm-candidates)))
    (unless candidates
      (user-error "No Claude-template projects found under %s"
                  (mapconcat #'identity
                             (append cj/ai-vterm-project-roots
                                     cj/ai-vterm-container-roots)
                             ", ")))
    (let* ((display-alist
            (mapcar (lambda (p) (cons (abbreviate-file-name p) p))
                    candidates))
           (chosen (completing-read "AI vterm project: "
                                    display-alist nil t)))
      (or (cdr (assoc chosen display-alist))
          (expand-file-name chosen)))))

(defun cj/ai-vterm (&optional arg)
  "Open or reuse a Claude-running vterm for a chosen project.

The project is picked from a filtered completing-read list of dirs
that contain .ai/protocols.org.  The vterm buffer is named
\"claude [<basename>]\" and is routed to a right-side window via
`display-buffer-alist'.  Multiple projects coexist as separate
buffers; reinvoking on the same project reuses its existing vterm.

With prefix ARG, display the buffer without selecting its window."
  (interactive "P")
  (let* ((dir (cj/--ai-vterm-pick-project))
         (name (cj/--ai-vterm-buffer-name dir))
         (buf (cj/--ai-vterm-show-or-create dir name)))
    (unless arg
      (let ((win (get-buffer-window buf)))
        (when win (select-window win))))
    buf))

(keymap-global-set "<f9>" #'cj/ai-vterm)

(provide 'ai-vterm)
;;; ai-vterm.el ends here
