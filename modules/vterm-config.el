;;; vterm-config.el --- Settings for vterm and the F12 toggle -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; VTERM
;; At the moment, vterm behaves like a real terminal. For most keys, vterm will
;; just send them to the process that is currently running. So, C-a may be
;; beginning-of-the-line in a shell, or the prefix key in a screen session.

;; Two ways to lift text out of a vterm, both with the same key story:
;;   - C-; x c  enters copy-mode via `cj/vterm-copy-mode-dwim'.  When a tmux
;;     client is attached to the vterm (typical -- `cj/vterm-launch-tmux'
;;     auto-starts tmux), sends tmux's prefix C-b [ so the user lands in
;;     tmux's own copy-mode with the full pane history available
;;     (history-limit, default 100000 in this config's tmux.conf).  Without
;;     tmux, falls back to `vterm-copy-mode' against vterm's scrollback.
;;   - C-; x h  captures the current tmux pane's full history into a temporary
;;     Emacs buffer.
;; In all three surfaces (vterm-copy-mode, tmux copy-mode, history buffer),
;; M-w copies the active region and stays open so several pieces can be
;; grabbed in a row; C-g, <escape>, or q leaves without copying; RET is
;; unbound -- no special "copy and exit" shortcut.  The tmux-side bindings
;; live in ~/code/archsetup/dotfiles/common/.tmux.conf.

;; ANSI-TERM & TERM
;; I haven't yet found a need for term or ansi-term in my workflows, so I leave
;; them with their default configurations.

;;; Code:

(require 'keybindings)
(require 'seq)
(require 'subr-x)
(require 'cj-window-geometry-lib)
(require 'cj-window-toggle-lib)

(defvar-keymap cj/vterm-map
  :doc "Personal vterm command map.")
;; Lowercase x picked over V for fewer Shift presses; v is the VC menu.
(keymap-set cj/custom-keymap "x" cj/vterm-map)

(defvar-local cj/vterm-tmux-history--origin-buffer nil
  "Buffer active before opening the tmux history buffer.")

(defvar-local cj/vterm-tmux-history--origin-window nil
  "Window active before opening the tmux history buffer.")

(defvar-local cj/vterm-tmux-history--origin-point nil
  "Point in the origin buffer before opening the tmux history buffer.")

(defun cj/vterm--tmux-output (&rest args)
  "Run tmux with ARGS and return its stdout.
Signal `user-error' when tmux exits with a non-zero status."
  (with-temp-buffer
    (let ((exit-code (apply #'process-file "tmux" nil t nil args)))
      (unless (zerop exit-code)
        (user-error "tmux failed: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun cj/vterm--tmux-pane-id-for-tty (tty)
  "Return the tmux pane id for client TTY."
  (let* ((output (cj/vterm--tmux-output
                  "list-clients" "-F" "#{client_tty}\t#{pane_id}"))
         (lines (split-string output "\n" t))
         (match (seq-find
                 (lambda (line)
                   (let ((fields (split-string line "\t")))
                     (equal (car fields) tty)))
                 lines)))
    (unless match
      (user-error "No tmux client found for vterm tty %s" tty))
    (cadr (split-string match "\t"))))

(defun cj/vterm--tmux-capture-pane (pane-id)
  "Return full joined tmux history for PANE-ID."
  (cj/vterm--tmux-output
   "capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" pane-id))

(defun cj/vterm--current-tmux-pane-id ()
  "Return the tmux pane id for the current vterm buffer."
  (unless (eq major-mode 'vterm-mode)
    (user-error "Current buffer is not a vterm buffer"))
  (let* ((proc (get-buffer-process (current-buffer)))
         (tty (and proc (process-tty-name proc))))
    (unless (and tty (not (string-empty-p tty)))
      (user-error "Could not determine vterm tty"))
    (cj/vterm--tmux-pane-id-for-tty tty)))

(defvar-keymap cj/vterm-tmux-history-mode-map
  :doc "Keymap for `cj/vterm-tmux-history-mode'.
M-w copies the active region without leaving the buffer; C-g, <escape>, or q
returns to the vterm without copying.  RET is left unbound."
  "M-w" #'kill-ring-save
  "C-g" #'cj/vterm-tmux-history-quit
  "<escape>" #'cj/vterm-tmux-history-quit
  "q" #'cj/vterm-tmux-history-quit)

(define-derived-mode cj/vterm-tmux-history-mode special-mode "Tmux History"
  "Mode for copying captured tmux pane history with normal Emacs keys."
  (setq-local truncate-lines t)
  (goto-address-mode 1))

(defun cj/vterm-tmux-history-quit ()
  "Quit tmux history and return to its origin buffer."
  (interactive)
  (let ((history-buffer (current-buffer))
        (origin-buffer cj/vterm-tmux-history--origin-buffer)
        (origin-window cj/vterm-tmux-history--origin-window)
        (origin-point cj/vterm-tmux-history--origin-point))
    (when (buffer-live-p origin-buffer)
      (if (window-live-p origin-window)
          (progn
            (set-window-buffer origin-window origin-buffer)
            (select-window origin-window))
        (pop-to-buffer origin-buffer))
      (with-current-buffer origin-buffer
        (when (integer-or-marker-p origin-point)
          (goto-char origin-point))))
    (when (buffer-live-p history-buffer)
      (kill-buffer history-buffer))))

(defun cj/vterm-tmux-history ()
  "Open full tmux pane history in a temporary Emacs buffer.

The history buffer uses normal Emacs navigation and selection.  `M-w'
copies the active region and stays open, so several pieces can be
copied in a row; `q', `<escape>', or `C-g' returns point to the vterm
buffer that launched it.

The history view replaces the origin vterm buffer in the same window
(via `switch-to-buffer'), not a split or a popped-up window -- reading
past output should keep the agent's frame slot intact, and quit puts
the live terminal back where it was."
  (interactive)
  (let* ((origin-buffer (current-buffer))
         (origin-window (selected-window))
         (origin-point (point))
         (pane-id (cj/vterm--current-tmux-pane-id))
         (history (cj/vterm--tmux-capture-pane pane-id))
         (buffer (get-buffer-create
                  (format "*vterm tmux history: %s*" (buffer-name origin-buffer)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert history))
      (cj/vterm-tmux-history-mode)
      (setq-local cj/vterm-tmux-history--origin-buffer origin-buffer)
      (setq-local cj/vterm-tmux-history--origin-window origin-window)
      (setq-local cj/vterm-tmux-history--origin-point origin-point)
      (goto-char (point-max)))
    (switch-to-buffer buffer)))

(defun cj/vterm-copy-mode-cancel ()
  "Exit `vterm-copy-mode' without copying."
  (interactive)
  (unless (bound-and-true-p vterm-copy-mode)
    (user-error "This command is effective only in vterm-copy-mode"))
  (vterm-copy-mode -1))

(defun cj/vterm--in-tmux-p ()
  "Return non-nil when the current vterm has a tmux client attached.
Errors from the pane-id lookup (not in vterm-mode, no tty, no
matching client, tmux not installed) are treated as nil so callers
can use this as a cheap boolean predicate."
  (and (eq major-mode 'vterm-mode)
       (condition-case _
           (and (cj/vterm--current-tmux-pane-id) t)
         (error nil))))

(declare-function vterm-send-string "vterm" (string &optional paste-p))

(defun cj/vterm-copy-mode-dwim ()
  "Enter copy-mode using the engine appropriate to this vterm.

When tmux is attached to the current vterm, write tmux's default
prefix sequence (C-b [) into the pty so the user lands in tmux's
copy-mode with the full pane history (`history-limit', default
100000) available.  The matching tmux keys in
`~/code/archsetup/dotfiles/common/.tmux.conf' mirror this module's
Emacs story: M-w copies and stays, C-g / q / <escape> exit, Enter
is unbound.

Without tmux, falls through to `vterm-copy-mode' which walks only
vterm's own scrollback (effectively just the visible screen,
because tmux redraws via cursor positioning rather than scrolling
new lines through vterm's buffer)."
  (interactive)
  (if (cj/vterm--in-tmux-p)
      (vterm-send-string "\C-b[")
    (vterm-copy-mode)))

(defun cj/vterm--send-mouse-wheel (button)
  "Forward a wheel event to the program running in the current vterm.

BUTTON is the SGR mouse button code: 64 for wheel up, 65 for wheel
down.  X / Y coordinates are placeholders (1,1); tmux dispatches
`WheelUpPane' / `WheelDownPane' on the button code and ignores the
position when there is only one pane.

vterm's keymap binds only `mouse-1' and `mouse-yank-primary' --
wheel events fall through to Emacs's default scroll behavior, which
moves the window over vterm's scrollback instead of reaching the
pty.  Without this forwarding, tmux's `set -g mouse on' never fires
because tmux never sees the events."
  (vterm-send-string (format "\e[<%d;1;1M" button)))

(defun cj/vterm-mouse-wheel-up ()
  "Forward a wheel-up event to the program running in this vterm."
  (interactive)
  (cj/vterm--send-mouse-wheel 64))

(defun cj/vterm-mouse-wheel-down ()
  "Forward a wheel-down event to the program running in this vterm."
  (interactive)
  (cj/vterm--send-mouse-wheel 65))

(use-package vterm
  :defer .5
  :commands (vterm vterm-other-window)
  :init
  (defvar vterm-keymap-exceptions
    '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y")
    "Exceptions for `vterm-keymap'.")
  (add-to-list 'vterm-keymap-exceptions "C-;")
  (setq vterm-always-compile-module t)

  (defun cj/turn-off-chrome-for-vterm ()
	(hl-line-mode -1)
	(display-line-numbers-mode -1))

  (defun cj/vterm-launch-tmux ()
	"Automatically launch tmux in vterm if not already in a tmux session.

Skipped when `cj/--ai-vterm-suppress-tmux' is non-nil so the AI-vterm
flow can run its own project-named tmux session instead of a bare,
auto-named one.  `bound-and-true-p' keeps this safe whether or not
ai-vterm.el is loaded."
	(let ((proc (get-buffer-process (current-buffer))))
	  (when (and proc
				 (not (getenv "TMUX")) ; Check if not already in tmux
				 (not (bound-and-true-p cj/--ai-vterm-suppress-tmux)))
		(vterm-send-string "tmux\n"))))
  :hook
  ((vterm-mode . cj/turn-off-chrome-for-vterm)
   (vterm-mode . cj/vterm-launch-tmux))
  :bind
  (:map vterm-mode-map
		("<f8>"    . nil)
		("<f9>"    . nil)
		("<f10>"   . nil)
		("<f12>"   . nil)
		("C-c C-t" . nil)
		("C-y"     . vterm-yank)
		("<wheel-up>"   . cj/vterm-mouse-wheel-up)
		("<wheel-down>" . cj/vterm-mouse-wheel-down)
		("<mouse-4>"    . cj/vterm-mouse-wheel-up)
		("<mouse-5>"    . cj/vterm-mouse-wheel-down))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 100000)
  :config
  (setq vterm-timer-delay nil))

;; vterm-toggle is kept installed so `M-x vterm-toggle' still works,
;; but F12 below is bound to a custom toggle (`cj/vterm-toggle') that
;; excludes agent-prefixed buffers from its candidate set.
(use-package vterm-toggle
  :defer .5
  :config
  (setq vterm-toggle-fullscreen-p nil))

;; ----------------------- F12 toggle (custom) -----------------------
;;
;; Replacement for `vterm-toggle' on F12.  Two reasons to roll our own:
;;
;; 1. agent exclusion.  vterm-toggle picks the most-recently-selected
;;    vterm buffer as the toggle target.  When the user just used F9
;;    on an agent vterm, the most-recent vterm IS agent, so F12 ends
;;    up toggling agent -- which has its own F9 / C-F9 / M-F9 surface
;;    in `ai-vterm.el' and shouldn't be affected by F12.  The agent
;;    exclusion lives in the candidate filter (`cj/--vterm-toggle-buffer-p').
;;
;; 2. user-modified geometry.  vterm-toggle's display rule had a
;;    hard-coded `(window-height . 0.7)' that overrode any mouse-resize
;;    or M-S-t orientation flip on the next toggle.  This module mirrors
;;    the geometry-preservation pattern shipped in ai-vterm.el: capture
;;    direction + body size at toggle-off, replay them via a custom
;;    display action (`cj/--vterm-toggle-display-saved') that uses
;;    frame-edge directions and `(body-columns . N)' / `(body-lines . N)'
;;    so the result is divider-independent and layout-stable.

(defcustom cj/vterm-toggle-window-height 0.7
  "Default fraction of frame height for the F12 vterm window.
Used as the size fallback when `cj/--vterm-toggle-last-size' is nil
(i.e. the user hasn't toggled off a vterm yet this session)."
  :type 'number
  :group 'vterm)

(defvar cj/--vterm-toggle-last-direction nil
  "Last user-chosen direction for the F12 vterm display.
Symbol: right, left, below, above.  nil means use the default
`below' for F12's traditional bottom split.")

(defvar cj/--vterm-toggle-last-size nil
  "Last user-chosen body size for the F12 vterm display.
Positive integer: body-cols (right/left) or body-lines (below/above).
nil means fall back to `cj/vterm-toggle-window-height' as a fraction.")

(defun cj/--vterm-toggle-buffer-p (buffer)
  "Return non-nil when BUFFER is a vterm buffer F12 should manage.

Qualifies when BUFFER is alive, has `vterm-mode' (or its name starts
with the vterm-toggle prefix), AND its name does NOT start with the
agent prefix used by ai-vterm.el.  The agent exclusion keeps F12
from grabbing buffers that ai-vterm.el's F9 dispatch owns."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (or (eq major-mode 'vterm-mode)
                  (string-prefix-p (or (bound-and-true-p vterm-buffer-name)
                                       "*vterm*")
                                   (buffer-name buffer)))
              (not (string-prefix-p "agent [" (buffer-name buffer)))))))

(defun cj/--vterm-toggle-buffers ()
  "Return live F12-managed vterm buffers in `buffer-list' (MRU) order."
  (seq-filter #'cj/--vterm-toggle-buffer-p (buffer-list)))

(defun cj/--vterm-toggle-displayed-window (&optional frame)
  "Return a window in FRAME currently displaying an F12 vterm buffer, or nil.
FRAME defaults to the selected frame.  Minibuffer is excluded."
  (seq-find (lambda (w)
              (cj/--vterm-toggle-buffer-p (window-buffer w)))
            (window-list (or frame (selected-frame)) 'never)))

(defun cj/--vterm-toggle-capture-state (window)
  "Capture WINDOW's direction + body size into module-level state.

Default direction is `below' to match F12's traditional bottom
split when WINDOW fills the frame's root area."
  (cj/window-toggle-capture-state
   window 'below
   'cj/--vterm-toggle-last-direction
   'cj/--vterm-toggle-last-size))

(defun cj/--vterm-toggle-display-saved (buffer alist)
  "Display-buffer action: split per saved direction and body size.

Delegates to `cj/window-toggle-display-saved' against the F12 state
vars, falling back to `below' and `cj/vterm-toggle-window-height'."
  (cj/window-toggle-display-saved
   buffer alist
   'cj/--vterm-toggle-last-direction 'below
   'cj/--vterm-toggle-last-size cj/vterm-toggle-window-height))

(defun cj/--vterm-toggle-display-rule-list ()
  "Return the `display-buffer-alist' entry list installed by F12.

Routes any vterm buffer that satisfies `cj/--vterm-toggle-buffer-p'
through two actions: reuse-window (for visible vterm windows) then
the saved-geometry display action.  Excludes agent buffers via the
predicate -- those are handled by ai-vterm.el's display rule."
  '(((lambda (buffer-or-name _)
       (cj/--vterm-toggle-buffer-p (get-buffer buffer-or-name)))
     (display-buffer-reuse-window
      cj/--vterm-toggle-display-saved)
     (inhibit-same-window . t))))

(dolist (entry (cj/--vterm-toggle-display-rule-list))
  (add-to-list 'display-buffer-alist entry))

(defun cj/--vterm-toggle-dispatch ()
  "Compute the F12 (`cj/vterm-toggle') action without performing it.

Returns one of:
- (toggle-off . WINDOW)        -- vterm displayed in WINDOW; hide it.
- (show-recent . BUFFER)       -- vterm alive but not shown; redisplay.
- (create-new)                 -- no vterm buffer alive; create one."
  (let ((win (cj/--vterm-toggle-displayed-window)))
    (cond
     (win (cons 'toggle-off win))
     (t
      (let ((buffers (cj/--vterm-toggle-buffers)))
        (cond
         (buffers (cons 'show-recent (car buffers)))
         (t '(create-new))))))))

(declare-function vterm "vterm" (&optional buffer-name))

(defun cj/vterm-toggle ()
  "Toggle a normal (non-agent) vterm buffer.

- If an F12-managed vterm is currently displayed in this frame,
  capture its geometry and delete its window (toggle off).  Falls
  back to burying the buffer when the vterm is the only window in
  the frame.
- Otherwise, if any F12-managed vterm buffer is alive, display the
  most-recent one via the saved-geometry action.
- Otherwise, create a new vterm via `(vterm)' which routes through
  the same display action.

Excludes agent-prefixed vterm buffers; those have their own F9 /
C-F9 / M-F9 dispatch via `cj/ai-vterm'."
  (interactive)
  (pcase (cj/--vterm-toggle-dispatch)
    (`(toggle-off . ,win)
     (cj/--vterm-toggle-capture-state win)
     (if (one-window-p)
         (bury-buffer (window-buffer win))
       (delete-window win))
     nil)
    (`(show-recent . ,buf)
     (display-buffer buf)
     (let ((w (get-buffer-window buf)))
       (when w (select-window w)))
     buf)
    (`(create-new)
     (vterm))))

(keymap-global-set "<f12>" #'cj/vterm-toggle)

(keymap-set cj/vterm-map "c" #'cj/vterm-copy-mode-dwim)
(keymap-set cj/vterm-map "h" #'cj/vterm-tmux-history)
(keymap-set cj/vterm-map "l" #'vterm-clear-scrollback)
(keymap-set cj/vterm-map "N" #'vterm)
(keymap-set cj/vterm-map "n" #'vterm-next-prompt)
(keymap-set cj/vterm-map "o" #'vterm-other-window)
(keymap-set cj/vterm-map "p" #'vterm-previous-prompt)
(keymap-set cj/vterm-map "q" #'vterm-send-next-key)
(keymap-set cj/vterm-map "r" #'vterm-reset-cursor-point)
(keymap-set cj/vterm-map "t" #'cj/vterm-toggle)

(defun cj/vterm-install-prefix-key ()
  "Make `C-;' resolve as the personal keymap inside vterm buffers."
  (when (boundp 'vterm-mode-map)
    (keymap-set vterm-mode-map "C-;" cj/custom-keymap)))

(defun cj/vterm-install-copy-mode-cancel-keys ()
  "Install copy and exit keys in `vterm-copy-mode-map'.

`M-w' copies the active region without leaving copy-mode, so several
pieces can be copied in a row.  `C-g', `<escape>', and `q' all leave
copy-mode without copying.  vterm's default `RET' / `<return>' ->
`vterm-copy-mode-done' bindings are removed so RET isn't a special
\"copy and exit\" -- matching the tmux history buffer."
  (when (boundp 'vterm-copy-mode-map)
    (keymap-set vterm-copy-mode-map "M-w" #'kill-ring-save)
    (keymap-set vterm-copy-mode-map "C-g" #'cj/vterm-copy-mode-cancel)
    (keymap-set vterm-copy-mode-map "<escape>" #'cj/vterm-copy-mode-cancel)
    (keymap-set vterm-copy-mode-map "q" #'cj/vterm-copy-mode-cancel)
    (keymap-unset vterm-copy-mode-map "RET" t)
    (keymap-unset vterm-copy-mode-map "<return>" t)))

(cj/vterm-install-prefix-key)
(cj/vterm-install-copy-mode-cancel-keys)
(with-eval-after-load 'vterm
  (cj/vterm-install-prefix-key)
  (cj/vterm-install-copy-mode-cancel-keys))

(defun cj/--vterm-copy-mode-restore-cursor ()
  "Force a visible cursor on entry to `vterm-copy-mode'.

The vterm C module sets `cursor-type' to nil whenever the underlying
TUI sends DECTCEM (`\\e[?25l') to hide the terminal cursor — typical
for full-screen TUIs like Claude Code.  In `vterm-copy-mode' the user
is navigating the buffer, not watching the TUI, so the cursor must
be visible.  Switches to a `box' so the cursor color and blinking
behavior follow Emacs's normal cursor-face / `blink-cursor-mode'
defaults.  On exit, kills the buffer-local override so vterm's normal
cursor-visibility tracking resumes."
  (if vterm-copy-mode
      (setq-local cursor-type 'box)
    (kill-local-variable 'cursor-type)))

(add-hook 'vterm-copy-mode-hook #'cj/--vterm-copy-mode-restore-cursor)

(add-hook 'vterm-mode-hook #'goto-address-mode)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; x" "vterm menu"
    "C-; x c" "copy mode (tmux/vterm)"
    "C-; x h" "tmux scrollback history"
    "C-; x l" "clear vterm scrollback"
    "C-; x N" "new vterm"
    "C-; x n" "next prompt"
    "C-; x o" "vterm other window"
    "C-; x p" "previous prompt"
    "C-; x q" "send next key to vterm"
    "C-; x r" "reset vterm cursor point"
    "C-; x t" "toggle vterm"))

(provide 'vterm-config)
;;; vterm-config.el ends here.
