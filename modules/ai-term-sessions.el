;;; ai-term-sessions.el --- AI-term project discovery and tmux sessions -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: library.
;; Top-level side effects: none (defuns, the name-prefix defconst, and the
;;   in-memory MRU state defvar).
;; Runtime requires: seq.
;; Direct test load: yes.
;;
;; Session/project layer of ai-term: agent buffer naming and detection,
;; project-candidate discovery and picker ordering, tmux session naming,
;; live-session discovery, launch-command building, and the active-agent
;; rotation inputs.  No EAT, no window operations -- everything here is
;; exercisable without a terminal backend or a display.
;;
;; The user options this layer reads (`cj/ai-term-project-roots',
;; `cj/ai-term-container-roots', `cj/ai-term-tmux-session-prefix',
;; `cj/ai-term-tmux-window-name', `cj/ai-term-agent-command') are owned by
;; ai-term.el, the public face, which requires this module before defining
;; them; they are forward-declared here so this module compiles and reads
;; them without a cycle.

;;; Code:

(require 'seq)

;; Owned by ai-term.el (the public face's defcustoms); forward-declared so
;; this module compiles and reads them without a cycle.
(defvar cj/ai-term-project-roots)
(defvar cj/ai-term-container-roots)
(defvar cj/ai-term-tmux-session-prefix)
(defvar cj/ai-term-tmux-window-name)
(defvar cj/ai-term-agent-command)

(defconst cj/--ai-term-name-prefix "agent ["
  "Buffer-name prefix shared by all AI-term buffers.

Single source of truth for both buffer construction in
`cj/--ai-term-buffer-name' and detection in
`cj/--ai-term-buffer-p'.  The display-buffer-alist rule keys on the
escaped form \"\\\\`agent \\\\[\" -- they must stay in sync.")

(defun cj/--ai-term-buffer-name (dir)
  "Return the AI-term buffer name for project directory DIR.

The name pattern is \"agent [<basename>]\".  The display-buffer-alist
rule keys on the literal prefix \"agent [\", so changing the format
breaks routing to the right-side window."
  (format "%s%s]"
          cj/--ai-term-name-prefix
          (file-name-nondirectory (directory-file-name dir))))

(defun cj/--ai-term-buffer-p (buffer)
  "Return non-nil when BUFFER is an AI-term buffer.

A buffer qualifies when its name starts with the literal prefix in
`cj/--ai-term-name-prefix' (\"agent [\").  The check is anchored at
the start so names like \"foo agent [bar]\" do not match."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (string-prefix-p cj/--ai-term-name-prefix (buffer-name buffer))))

(defun cj/--ai-term-buffer-basename (buffer)
  "Return the project basename embedded in BUFFER's AI-term name, or nil.

The buffer name is \"agent [<basename>]\" (see
`cj/--ai-term-buffer-name') and never changes for the buffer's life,
unlike `default-directory', which ghostel retargets via OSC 7 every time
the shell cds.  Teardown paths must key tmux-session lookups off this,
not the directory, or a close after a cd kills the wrong aiv- session.
Returns nil when BUFFER is not a live AI-term buffer."
  (when (cj/--ai-term-buffer-p buffer)
    (let ((name (buffer-name buffer)))
      (when (string-suffix-p "]" name)
        (substring name (length cj/--ai-term-name-prefix) -1)))))

(defun cj/--ai-term-agent-buffers ()
  "Return the live AI-term buffers in `buffer-list' order.

Order matches `buffer-list' on the selected frame, which is most-
recently-selected first.  Non-AI-term buffers are filtered out via
`cj/--ai-term-buffer-p'."
  (seq-filter #'cj/--ai-term-buffer-p (buffer-list)))

(defun cj/--ai-term-next-agent-dir (current dirs)
  "Return the project dir after CURRENT in DIRS, wrapping to the first.

DIRS is an ordered list of active-agent project dirs.  When CURRENT is
the last element, wrap to the first.  When CURRENT is nil or not a member
of DIRS, return the first dir.  Returns nil when DIRS is empty.  Matches
with `member' (string equality) since dirs are paths.

Pure decision helper (no buffer or window side effects) so the cycle
order driving `cj/ai-term-next' is exercisable in tests."
  (when dirs
    (if (member current dirs)
        (or (cadr (member current dirs))
            (car dirs))
      (car dirs))))

(defun cj/--ai-term-active-agent-dirs ()
  "Return project dirs that have a live agent buffer or a live tmux session.

Sorted by the agent buffer name, so the rotation is stable and matches
what the picker shows.  This is the queue `cj/ai-term-next' steps through:
it includes detached sessions (alive in tmux but with no Emacs buffer),
which the step materializes by attaching."
  (let* ((sessions (cj/--ai-term-live-tmux-sessions))
         (live-names (mapcar #'buffer-name (cj/--ai-term-agent-buffers))))
    (sort
     (seq-filter
      (lambda (dir)
        (or (member (cj/--ai-term-buffer-name dir) live-names)
            (cj/--ai-term-session-active-p dir sessions)))
      (cj/--ai-term-candidates))
     (lambda (a b)
       (string< (cj/--ai-term-buffer-name a) (cj/--ai-term-buffer-name b))))))

(defun cj/--ai-term-tmux-session-name (dir)
  "Return the tmux session name for project directory DIR.

`cj/ai-term-tmux-session-prefix' followed by DIR's basename, sanitized
to a form tmux won't re-mangle: runs of whitespace become a single
hyphen, and `.' / `:' become `_'.  tmux disallows `.' and `:' in
session names and silently rewrites them to `_', so a project like
`.emacs.d' really runs in session `aiv-_emacs_d', not `aiv-.emacs.d' --
sanitizing up front keeps the computed name matching the live one (and
keeps `cj/--ai-term-session-active-p' and the crash-recovery picker
from missing such projects).  The prefix lets `tmux ls' output be
filtered to AI-term's own sessions (see
`cj/--ai-term-live-tmux-sessions')."
  (concat cj/ai-term-tmux-session-prefix
          (replace-regexp-in-string
           "[.:]" "_"
           (replace-regexp-in-string
            "[[:space:]]+" "-"
            (file-name-nondirectory (directory-file-name dir))))))

(defun cj/--ai-term-live-tmux-sessions ()
  "Return live tmux session names that carry the AI-term prefix.

Runs `tmux list-sessions'.  Returns the names beginning with
`cj/ai-term-tmux-session-prefix', or nil when tmux is not installed,
no server is running, or the command exits non-zero -- the picker
treats nil as \"no sessions to surface\" and falls back to a plain
alphabetical list."
  (let* ((prefix cj/ai-term-tmux-session-prefix)
         (exit nil)
         (output (with-temp-buffer
                   (setq exit (condition-case nil
                                  (process-file "tmux" nil '(t nil) nil
                                                "list-sessions" "-F"
                                                "#{session_name}")
                                (error nil)))
                   (buffer-string))))
    (when (and (integerp exit) (zerop exit))
      (seq-filter (lambda (name) (string-prefix-p prefix name))
                  (split-string output "\n" t)))))

(defun cj/--ai-term-session-active-p (dir sessions)
  "Return non-nil when DIR's tmux session name is in SESSIONS.

SESSIONS is the list from `cj/--ai-term-live-tmux-sessions' (or nil).
The match is forward: DIR's expected session name is computed and
looked up in SESSIONS, so the lossy whitespace->hyphen transform in
`cj/--ai-term-tmux-session-name' never needs reversing."
  (and (member (cj/--ai-term-tmux-session-name dir) sessions) t))

(defun cj/--ai-term-launch-command (dir &optional agent-command)
  "Return the shell command line that runs the AI tool in a project tmux session.

Uses `tmux new-session -A' so a second toggle on the same project reattaches
to the running session instead of spawning a new one.  The session name
comes from `cj/--ai-term-tmux-session-name'; the first window is named
`cj/ai-term-tmux-window-name' (default \"ai\") so a later hand-opened
window auto-names after its command and the two read distinctly.

The shell command run on first creation is
  <agent command>; exec bash
so the tmux window survives the AI command exiting -- the session stays
alive with a bare bash prompt for recovery, and reattach works the same way.
AGENT-COMMAND overrides `cj/ai-term-agent-command' for the fresh-session
case (the multi-backend picker passes the chosen runtime's command); on a
reattach `tmux new-session -A' ignores the command either way."
  (let ((session (cj/--ai-term-tmux-session-name dir))
        (start-dir (expand-file-name dir)))
    ;; Pass the inner shell-command-string through `shell-quote-argument'
    ;; so any single quotes embedded in a user-customized
    ;; `cj/ai-term-agent-command' don't break the literal single-quote
    ;; wrap below.  The default value carries embedded double quotes
    ;; (\"Read .ai/protocols.org and follow all instructions.\") which
    ;; was safe in the prior shape but a single-quoted custom value
    ;; silently broke the shell parse.
    (format "tmux new-session -A -s %s -n %s -c %s %s"
            (shell-quote-argument session)
            (shell-quote-argument cj/ai-term-tmux-window-name)
            (shell-quote-argument start-dir)
            (shell-quote-argument
             (concat (or agent-command cj/ai-term-agent-command)
                     "; exec bash")))))

(defun cj/--ai-term-kill-tmux-session (session)
  "Kill the tmux SESSION via `tmux kill-session -t SESSION'.

Returns the process exit status (0 on success), or nil when tmux is
unavailable or already gone -- a session that no longer exists is not
an error worth surfacing, since the goal is just to make sure it's
down."
  (condition-case nil
      (process-file "tmux" nil nil nil "kill-session" "-t" session)
    (error nil)))

(defun cj/--ai-term-has-marker-p (dir)
  "Return non-nil when DIR contains .ai/protocols.org."
  (file-exists-p (expand-file-name ".ai/protocols.org" dir)))

(defun cj/--ai-term-candidates ()
  "Return the list of AI-agent project paths.

Each entry of `cj/ai-term-project-roots' contributes itself when it
exists and contains .ai/protocols.org.  Each entry of
`cj/ai-term-container-roots' contributes its immediate child
directories that contain .ai/protocols.org.

Returns absolute paths.  Nonexistent roots are skipped silently."
  (let (result)
    (dolist (root cj/ai-term-project-roots)
      (let ((expanded (expand-file-name root)))
        (when (and (file-directory-p expanded)
                   (cj/--ai-term-has-marker-p expanded))
          (push expanded result))))
    (dolist (root cj/ai-term-container-roots)
      (let ((expanded (expand-file-name root)))
        (when (file-directory-p expanded)
          (dolist (child (directory-files
                          expanded t directory-files-no-dot-files-regexp t))
            (when (and (file-directory-p child)
                       (cj/--ai-term-has-marker-p child))
              (push child result))))))
    (nreverse result)))

(defvar cj/--ai-term-mru nil
  "Project dirs opened via the AI-term launcher this session, newest first.

Maintained by `cj/--ai-term-record-mru' (called from
`cj/--ai-term-show-or-create') and consumed by
`cj/--ai-term-sort-candidates' so the project picker puts
recently-opened projects at the top of the active-sessions group.
In-memory only -- not persisted across Emacs restarts.")

(defun cj/--ai-term-record-mru (dir)
  "Move DIR to the front of `cj/--ai-term-mru'.

DIR is normalized with `expand-file-name' + `directory-file-name' so a
trailing slash or `~' form doesn't create a duplicate entry; any prior
occurrence is removed first, keeping the list a true MRU order."
  (let ((d (directory-file-name (expand-file-name dir))))
    (setq cj/--ai-term-mru (cons d (delete d cj/--ai-term-mru)))))

(defun cj/--ai-term-mru-rank (dir)
  "Return DIR's index in `cj/--ai-term-mru', or nil when it isn't there.

DIR is normalized the same way `cj/--ai-term-record-mru' stores
entries, so a trailing slash doesn't defeat the lookup."
  (seq-position cj/--ai-term-mru
                (directory-file-name (expand-file-name dir))))

(defun cj/--ai-term-sort-candidates (dirs sessions)
  "Order DIRS for the project picker.

DIRS with a live tmux session in SESSIONS (per
`cj/--ai-term-session-active-p') come first, ordered most-recently-
opened first (per `cj/--ai-term-mru'); active dirs not opened yet this
session fall after them, alphabetical by abbreviated path.  DIRS with no
session follow, always alphabetical.  SESSIONS nil means nothing is
active, so the result is a plain alphabetical list; an empty MRU makes
the active group alphabetical too."
  (let* ((alpha (lambda (a b)
                  (string< (abbreviate-file-name a) (abbreviate-file-name b))))
         (mru-then-alpha
          (lambda (a b)
            (let ((ra (cj/--ai-term-mru-rank a))
                  (rb (cj/--ai-term-mru-rank b)))
              (cond ((and ra rb) (< ra rb))
                    (ra t)
                    (rb nil)
                    (t (funcall alpha a b))))))
         (active-p (lambda (d) (cj/--ai-term-session-active-p d sessions)))
         (active (seq-filter active-p dirs))
         (inactive (seq-remove active-p dirs)))
    (append (sort active mru-then-alpha) (sort inactive alpha))))

(defun cj/--ai-term-process-live-p (buffer)
  "Return non-nil when BUFFER has a live process attached."
  (let ((proc (get-buffer-process buffer)))
    (and proc (process-live-p proc))))

(defun cj/--ai-term-format-candidate (path &optional sessions)
  "Return the display name for PATH in the AI-term project picker.

Appends \" [running]\" when the project's agent buffer exists with
a live process; otherwise \" [detached]\" when PATH's tmux session
name is in SESSIONS (a session that survived an Emacs crash, no
buffer yet); otherwise just the abbreviated path.  Path is
abbreviated via `abbreviate-file-name' so it reads as ~/code/foo
rather than the full home-dir form."
  (let* ((name (cj/--ai-term-buffer-name path))
         (buf (get-buffer name))
         (running (and buf (cj/--ai-term-process-live-p buf)))
         (detached (and (not running)
                        (cj/--ai-term-session-active-p path sessions)))
         (display-path (abbreviate-file-name path)))
    (cond
     (running  (format "%s [running]" display-path))
     (detached (format "%s [detached]" display-path))
     (t        display-path))))

(defun cj/--ai-term-completion-table (alist)
  "Return a `completing-read' table over ALIST that pins candidate order.

`completing-read' over a bare alist lets the front-end (Vertico)
re-sort candidates by recency / length / alpha, which would defeat
the picker's active-sessions-first grouping.  Returning
`display-sort-function' and `cycle-sort-function' of `identity' in
the metadata keeps the order ALIST was built in."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action alist string predicate))))

(defun cj/--ai-term-pick-project ()
  "Prompt for an AI-agent project; return its absolute path.

Candidates come from `cj/--ai-term-candidates', ordered by
`cj/--ai-term-sort-candidates' so projects with a live tmux session
appear first (then alphabetical by abbreviated path).  Display uses
`cj/--ai-term-format-candidate', which abbreviates the path and
flags a live session via \" [running]\" (an Emacs terminal buffer is
alive) or \" [detached]\" (the tmux session survived, no buffer).
Signals `user-error' when no candidates exist."
  (let ((candidates (cj/--ai-term-candidates)))
    (unless candidates
      (user-error "No AI-agent projects found under %s"
                  (mapconcat #'identity
                             (append cj/ai-term-project-roots
                                     cj/ai-term-container-roots)
                             ", ")))
    (let* ((sessions (cj/--ai-term-live-tmux-sessions))
           (sorted (cj/--ai-term-sort-candidates candidates sessions))
           (display-alist
            (mapcar (lambda (p)
                      (cons (cj/--ai-term-format-candidate p sessions) p))
                    sorted))
           (chosen (completing-read
                    "AI terminal project: "
                    (cj/--ai-term-completion-table display-alist)
                    nil t)))
      (or (cdr (assoc chosen display-alist))
          (expand-file-name chosen)))))

(provide 'ai-term-sessions)
;;; ai-term-sessions.el ends here
