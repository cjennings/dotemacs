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
;; Three F-key entry points:
;;
;; - F9   `cj/ai-vterm' -- DWIM dispatch.  If a claude buffer is
;;        currently displayed in this frame, F9 quits its window
;;        (toggle off).  Otherwise, if exactly one claude buffer is
;;        alive, F9 re-displays it; if zero or two-plus are alive, F9
;;        falls through to the project picker.
;; - C-F9 `cj/ai-vterm-pick-project' -- always show the project
;;        picker, even when a claude buffer is currently displayed.
;;        Used when the user wants to start a new project session
;;        instead of toggling the current one.
;; - M-F9 `cj/ai-vterm-pick-buffer' -- pick from the alive claude
;;        buffers (no project candidates, no creation).  When a claude
;;        buffer is currently displayed, the picked buffer replaces it
;;        in that window so orientation and size are preserved.
;;
;; Existing windmove (Shift-arrows) handles code <-> Claude focus
;; toggling.  Buffer-move (C-M-arrows) handles side-swap.  Neither
;; needs anything new from this module.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'cj-window-geometry-lib)
(require 'cj-window-toggle-lib)

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

(defconst cj/--ai-vterm-name-prefix "claude ["
  "Buffer-name prefix shared by all AI-vterm buffers.

Single source of truth for both buffer construction in
`cj/--ai-vterm-buffer-name' and detection in
`cj/--ai-vterm-buffer-p'.  The display-buffer-alist rule keys on the
escaped form \"\\\\`claude \\\\[\" -- they must stay in sync.")

(defun cj/--ai-vterm-buffer-name (dir)
  "Return the AI-vterm buffer name for project directory DIR.

The name pattern is \"claude [<basename>]\".  The display-buffer-alist
rule keys on the literal prefix \"claude [\", so changing the format
breaks routing to the right-side window."
  (format "%s%s]"
          cj/--ai-vterm-name-prefix
          (file-name-nondirectory (directory-file-name dir))))

(defun cj/--ai-vterm-buffer-p (buffer)
  "Return non-nil when BUFFER is an AI-vterm buffer.

A buffer qualifies when its name starts with the literal prefix in
`cj/--ai-vterm-name-prefix' (\"claude [\").  The check is anchored at
the start so names like \"foo claude [bar]\" do not match."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (string-prefix-p cj/--ai-vterm-name-prefix (buffer-name buffer))))

(defun cj/--ai-vterm-claude-buffers ()
  "Return the live AI-vterm buffers in `buffer-list' order.

Order matches `buffer-list' on the selected frame, which is most-
recently-selected first.  Non-AI-vterm buffers are filtered out via
`cj/--ai-vterm-buffer-p'."
  (seq-filter #'cj/--ai-vterm-buffer-p (buffer-list)))

(defun cj/--ai-vterm-displayed-claude-window (&optional frame)
  "Return a window in FRAME currently displaying an AI-vterm buffer, or nil.

FRAME defaults to the selected frame.  When more than one window in
the frame shows a claude buffer, the first one in `window-list' order
is returned.  The minibuffer is excluded from the search."
  (seq-find (lambda (w)
              (cj/--ai-vterm-buffer-p (window-buffer w)))
            (window-list (or frame (selected-frame)) 'never)))

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
  "Default fraction of frame allocated to the AI-vterm window.

Used by `cj/--ai-vterm-display-saved' as the size fallback when
`cj/--ai-vterm-last-size' is nil (i.e. the user hasn't yet toggled
off a claude window in this session).  Applies to both width and
height axes -- the same fallback fraction is used for either default
direction."
  :type 'number
  :group 'ai-vterm)

(defvar cj/--ai-vterm-last-direction nil
  "Last user-chosen direction for the AI-vterm display.

Symbol: right, below, left, or above.  nil means no claude window
has been toggled off yet this session, so the default direction
applies.  Captured at toggle-off by `cj/--ai-vterm-capture-state'
and consumed by `cj/--ai-vterm-display-saved'.")

(defvar cj/--ai-vterm-last-size nil
  "Last user-chosen body size for the AI-vterm display.

Positive integer: body-columns when `cj/--ai-vterm-last-direction'
is right or left, body-lines when below or above.  nil means use
the customizable default `cj/ai-vterm-window-width' (a float
fraction).

Body size, not total size, because total-width includes the
right-edge divider when the window has a right sibling but excludes
it when the window is at the frame edge.  Capturing total-width
from a rightmost claude (no divider) and replaying into a middle
position (with divider) leaves the body 1 column short -- visible
as 1 col of the sibling buffer peeking through where claude should
have ended.  Body-width is divider-independent and matches what the
user actually sees.

Absolute values rather than fractions because
`display-buffer-in-direction' interprets a float `window-width' /
`window-height' as a fraction of the new window's parent in the
window tree.  In a 3+ window layout the parent may be a sub-tree,
and a fraction-of-frame produces the wrong size on replay
(squeezes the other windows).  An integer is unambiguous, at the
cost of not auto-scaling if the frame itself resizes.")

(defun cj/--ai-vterm-capture-state (window)
  "Capture WINDOW's direction and size into module-level state.

Sets `cj/--ai-vterm-last-direction' and `cj/--ai-vterm-last-size'
so a subsequent F9 display can restore the user's chosen orientation
and size.  Called at toggle-off (just before the window is torn
down).  The default direction is `right' -- the module's side-panel
default.  Does nothing when WINDOW is not live."
  (cj/window-toggle-capture-state
   window 'right
   'cj/--ai-vterm-last-direction
   'cj/--ai-vterm-last-size))

(defun cj/--ai-vterm-reuse-existing-claude (buffer _alist)
  "Display-buffer action: reuse any window in this frame already showing
a claude buffer.

Looks up `cj/--ai-vterm-displayed-claude-window' on the selected
frame.  When a claude window exists, replaces its buffer with BUFFER
and returns the window.  When none exists, returns nil so the next
action in the chain runs.

This is more specific than `display-buffer-use-some-window', which
would happily steal any non-selected window (e.g. a code window
above the claude split) when the user is focused in claude and
swaps projects via C-F9.  The selective lookup here keeps non-claude
windows undisturbed and preserves the user's split geometry across
project changes."
  (let ((win (cj/--ai-vterm-displayed-claude-window)))
    (when win
      (set-window-buffer win buffer)
      win)))

(defun cj/--ai-vterm-display-saved (buffer alist)
  "Display-buffer action: split per saved direction and size.

Delegates to `cj/window-toggle-display-saved' against the F9 state
vars, falling back to `right' and `cj/ai-vterm-window-width'."
  (cj/window-toggle-display-saved
   buffer alist
   'cj/--ai-vterm-last-direction 'right
   'cj/--ai-vterm-last-size cj/ai-vterm-window-width))

(defun cj/--ai-vterm-display-rule-list ()
  "Return the `display-buffer-alist' entry list installed by this module.

The single rule routes any buffer whose name starts with \"claude [\"
through three actions in order:

1. `display-buffer-reuse-window' -- if the same buffer is already
   visible in any window, focus that one.
2. `cj/--ai-vterm-reuse-existing-claude' -- otherwise, if any
   window in this frame already shows a claude-prefixed buffer,
   swap its buffer for the new one (preserves geometry across
   project changes via C-F9).
3. `cj/--ai-vterm-display-saved' -- otherwise, split per the saved
   direction + size from the last toggle-off (or defaults when no
   capture has happened this session).

`display-buffer-in-side-window' is avoided deliberately.  Side
windows enforce dedication, which breaks `buffer-move' (C-M-arrows)
and `switch-to-buffer' replacement.  The chain above keeps the
resulting window an ordinary window so all standard window commands
work.

`display-buffer-use-some-window' is also avoided -- it would happily
steal any non-selected window (e.g. a code window above a claude
split) when the user is focused in claude and switches projects."
  '(("\\`claude \\["
     (display-buffer-reuse-window
      cj/--ai-vterm-reuse-existing-claude
      cj/--ai-vterm-display-saved)
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
      ;; `vterm' calls pop-to-buffer-same-window internally, which
      ;; replaces the selected window's buffer (e.g. the dashboard at
      ;; fresh startup) before our display-buffer-alist rule has a
      ;; chance to route it.  `save-window-excursion' reverts that
      ;; side-effect; the explicit display-buffer call below then
      ;; routes the buffer through the alist into a right-side split.
      (save-window-excursion
        (let ((default-directory dir)
              (cj/--ai-vterm-suppress-tmux t))
          (vterm name)))
      (let ((buf (get-buffer name)))
        (with-current-buffer buf
          (vterm-send-string (cj/--ai-vterm-launch-command dir))
          (vterm-send-return))
        (display-buffer buf)
        buf)))))

(defun cj/--ai-vterm-format-candidate (path)
  "Return the display name for PATH in the AI-vterm project picker.

Appends \" [running]\" when the project's claude buffer exists with
a live process, so the user sees at a glance which projects already
have a session.  Path is abbreviated via `abbreviate-file-name' so
it reads as ~/code/foo rather than the full home-dir form."
  (let* ((name (cj/--ai-vterm-buffer-name path))
         (buf (get-buffer name))
         (running (and buf (cj/--ai-vterm-process-live-p buf)))
         (display-path (abbreviate-file-name path)))
    (if running
        (format "%s [running]" display-path)
      display-path)))

(defun cj/--ai-vterm-pick-project ()
  "Prompt for a Claude-template project; return its absolute path.

Candidates come from `cj/--ai-vterm-candidates'.  Display uses
`cj/--ai-vterm-format-candidate', which abbreviates the path and
flags projects with a live session via a \" [running]\" suffix.
Signals `user-error' when no candidates exist."
  (let ((candidates (cj/--ai-vterm-candidates)))
    (unless candidates
      (user-error "No Claude-template projects found under %s"
                  (mapconcat #'identity
                             (append cj/ai-vterm-project-roots
                                     cj/ai-vterm-container-roots)
                             ", ")))
    (let* ((display-alist
            (mapcar (lambda (p) (cons (cj/--ai-vterm-format-candidate p) p))
                    candidates))
           (chosen (completing-read "AI vterm project: "
                                    display-alist nil t)))
      (or (cdr (assoc chosen display-alist))
          (expand-file-name chosen)))))

(defun cj/--ai-vterm-dispatch ()
  "Compute the F9 (`cj/ai-vterm') action without performing it.

Returns one of:
- (toggle-off . WINDOW)        -- claude is displayed in WINDOW; quit it.
- (redisplay-recent . BUFFER)  -- 1+ alive claude buffers; show MRU.
- (pick-project)               -- zero alive claude buffers; prompt.

When 2+ claude buffers are alive, F9 redisplays the most-recently-
selected one rather than opening the project picker.  C-F9 is the
explicit \"start a different project\" surface; M-F9 is the explicit
\"switch among existing claudes\" surface.  F9 keeps a single, simple
job: toggle whichever claude was last in use.

A pure-decision helper so the dispatch logic is exercisable in tests
without firing real `display-buffer' or `quit-window' calls."
  (let ((win (cj/--ai-vterm-displayed-claude-window)))
    (cond
     (win (cons 'toggle-off win))
     (t
      (let ((buffers (cj/--ai-vterm-claude-buffers)))
        (cond
         (buffers (cons 'redisplay-recent (car buffers)))
         (t '(pick-project))))))))

(defun cj/--ai-vterm-pick-buffer-candidates (buffers shown-buffer)
  "Build the M-F9 picker alist.

BUFFERS is an MRU-ordered list of alive AI-vterm buffers.
SHOWN-BUFFER is the AI-vterm buffer currently displayed in this frame,
or nil.

When SHOWN-BUFFER is one of BUFFERS, it sorts last with a
\" [shown]\" suffix so the default `completing-read' selection lands
on a non-shown candidate (i.e. RET picks \"the other one\").  When
SHOWN-BUFFER is not in BUFFERS (a stale window state), every entry is
treated as non-shown.

Each cell is (DISPLAY-NAME . BUFFER)."
  (let ((non-shown (seq-remove (lambda (b) (eq b shown-buffer)) buffers))
        (shown (when (memq shown-buffer buffers) shown-buffer)))
    (append
     (mapcar (lambda (b) (cons (buffer-name b) b)) non-shown)
     (when shown
       (list (cons (format "%s [shown]" (buffer-name shown)) shown))))))

(defun cj/ai-vterm-pick-project (&optional arg)
  "Pick a Claude-template project and open or reuse its vterm.

The project is picked from a filtered completing-read list of dirs
that contain .ai/protocols.org.  The vterm buffer is named
\"claude [<basename>]\" and is routed to a right-side window via
`display-buffer-alist'.  Multiple projects coexist as separate
buffers; reinvoking on the same project reuses its existing vterm.

With prefix ARG, display the buffer without selecting its window.

Bound to C-F9 -- always shows the project picker, even when a claude
buffer is currently displayed."
  (interactive "P")
  (let* ((dir (cj/--ai-vterm-pick-project))
         (name (cj/--ai-vterm-buffer-name dir))
         (buf (cj/--ai-vterm-show-or-create dir name)))
    (unless arg
      (let ((win (get-buffer-window buf)))
        (when win (select-window win))))
    buf))

(defun cj/ai-vterm-pick-buffer ()
  "Pick from the alive AI-vterm buffers; switch to the chosen one.

When an AI-vterm buffer is currently displayed in this frame, the
picked buffer replaces it in the same window via `set-window-buffer'.
Orientation and size are preserved so the user's split layout doesn't
change.  When no AI-vterm buffer is displayed, default placement
applies via `display-buffer'.

The currently-displayed buffer (if any) is sorted last in the picker
with a \" [shown]\" suffix; the default selection lands on a
non-shown candidate so RET means \"give me the other one\".

Signals `user-error' when no AI-vterm buffers exist.

Bound to M-F9."
  (interactive)
  (let ((buffers (cj/--ai-vterm-claude-buffers)))
    (unless buffers
      (user-error "No Claude buffers"))
    (let* ((shown-win (cj/--ai-vterm-displayed-claude-window))
           (shown-buf (and shown-win (window-buffer shown-win)))
           (alist (cj/--ai-vterm-pick-buffer-candidates buffers shown-buf))
           (chosen (completing-read "AI vterm buffer: " alist nil t))
           (buf (cdr (assoc chosen alist))))
      (cond
       ((and shown-win (window-live-p shown-win))
        (set-window-buffer shown-win buf)
        (select-window shown-win))
       (t
        (display-buffer buf)
        (let ((w (get-buffer-window buf)))
          (when w (select-window w)))))
      buf)))

(defun cj/ai-vterm (&optional arg)
  "Smart F9 dispatch for the AI-vterm launcher.

Behavior depends on the current state:

- If an AI-vterm buffer is currently displayed in this frame, F9
  quits its window (toggle off, buffer stays alive).
- Else, if exactly one alive AI-vterm buffer exists, F9 re-displays
  it (DWIM -- the obvious next step is to look at it).
- Else (zero or 2+), F9 falls through to `cj/ai-vterm-pick-project'.

With prefix ARG, display the buffer without selecting its window
when a buffer is being shown (no effect on the toggle-off branch).

See `cj/ai-vterm-pick-project' (C-F9) to force the project picker
and `cj/ai-vterm-pick-buffer' (M-F9) to switch among existing
AI-vterm buffers without touching the project list."
  (interactive "P")
  (pcase (cj/--ai-vterm-dispatch)
    (`(toggle-off . ,win)
     (cj/--ai-vterm-capture-state win)
     ;; `delete-window' rather than `quit-window' so the toggle-off
     ;; semantics are unconditional.  `quit-window' only deletes the
     ;; window when its `quit-restore' parameter records that it was
     ;; created for the buffer.  Buffer-move (C-M-arrows) leaves the
     ;; claude buffer in a window without that history, so
     ;; `quit-window' would just bury -- the window stays with some
     ;; other buffer in it, and the next toggle-on then creates a
     ;; fresh side window for a count of N+1.  Skip the deletion
     ;; only when claude is the lone window in the frame (delete
     ;; would leave none); bury in that case.
     (if (one-window-p)
         (bury-buffer (window-buffer win))
       (delete-window win))
     nil)
    (`(redisplay-recent . ,buf)
     (display-buffer buf)
     (unless arg
       (let ((w (get-buffer-window buf)))
         (when w (select-window w))))
     buf)
    (`(pick-project)
     (cj/ai-vterm-pick-project arg))))

(keymap-global-set "<f9>"   #'cj/ai-vterm)
(keymap-global-set "C-<f9>" #'cj/ai-vterm-pick-project)
(keymap-global-set "M-<f9>" #'cj/ai-vterm-pick-buffer)

(provide 'ai-vterm)
;;; ai-vterm.el ends here
