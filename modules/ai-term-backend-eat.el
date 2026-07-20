;;; ai-term-backend-eat.el --- EAT terminal backend for ai-term -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: library.
;; Top-level side effects: binds M-SPC in `eat-semi-char-mode-map' after EAT
;;   loads.
;; Runtime requires: ai-term-sessions.
;; Direct test load: yes.
;;
;; EAT backend of ai-term: terminal buffer creation and tmux reattach, the
;; pty send helper, and the EAT keymap integration.  The name is
;; backend-specific on purpose -- a future terminal backend lands as a
;; sibling ai-term-backend-<name>.el and everything above this layer stays
;; put.  The display routing the created buffer goes through lives in
;; ai-term-display (the display-buffer-alist rule); this module only calls
;; `display-buffer'.

;;; Code:

(require 'ai-term-sessions)

(declare-function eat "eat" (&optional program arg))
(declare-function eat-term-set-parameter "eat" (terminal parameter value))
(declare-function cj/ai-term-next "ai-term" ())
(declare-function cj/--ai-term-project-color "ai-term" (dir))
(defvar eat-buffer-name)
(defvar eat-semi-char-mode-map)
(defvar eat-terminal)
(defvar cj/ai-term-palette-faces)

(defun cj/--ai-term-send-string (buffer string)
  "Send STRING to BUFFER's terminal process (the agent's shell).
Sends to the pty directly so the launch command reaches the shell EAT runs."
  (let ((proc (get-buffer-process buffer)))
    (when (process-live-p proc)
      (process-send-string proc string))))

(defun cj/--ai-term-apply-accent (buffer)
  "Point BUFFER's terminal palette entries at their dupre faces.
Repaints each (INDEX . FACE) in `cj/ai-term-palette-faces' in this
terminal's own 256-color palette (eat keeps one per terminal), so Claude
Code's accents -- the bypass banner and every /color session color --
render in dupre hues while other eat terminals keep the true palette.
A no-op when BUFFER has no live eat terminal.  Takes effect on the
terminal's next redraw; text already on screen keeps its old color until
the program repaints it (Claude Code's TUI repaints continuously)."
  (with-current-buffer buffer
    (when (bound-and-true-p eat-terminal)
      (dolist (entry cj/ai-term-palette-faces)
        (eat-term-set-parameter eat-terminal
                                (intern (format "color-%d-face" (car entry)))
                                (cdr entry))))))

(defun cj/--ai-term-color-ready-p (buffer)
  "Return non-nil when BUFFER's Claude TUI is ready for a /color injection.
Ready means the bypass-permissions banner is on screen (the TUI booted;
a plain shell never shows it, so this fails safe) AND the input prompt
line is still empty (the user hasn't started typing -- injecting into a
half-typed prompt would corrupt their input)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (and (search-forward "⏵⏵" nil t)
             (progn (goto-char (point-min))
                    (re-search-forward "^❯ *$" nil t))
             t)))))

(defun cj/--ai-term-send-color (buffer color)
  "Type \"/color COLOR\" into BUFFER's TUI, then Enter a beat later.
The Enter is deferred because the slash-command menu pops up while the
text arrives; a CR in the same write can select a menu entry instead of
running the typed command (the same race the color probe dodged)."
  (cj/--ai-term-send-string buffer (concat "/color " color))
  (run-at-time 1 nil #'cj/--ai-term-send-string buffer "\r"))

(defun cj/--ai-term-schedule-color (buffer color)
  "Poll BUFFER until its Claude TUI is ready, then send /color COLOR.
Polls every 2 seconds and gives up after 45 tries (90s) or when the
buffer dies -- covering a Claude that never launches, so nothing is ever
typed into a bare shell.  Returns the poll timer."
  (let ((tries 0) (timer nil))
    (setq timer
          (run-at-time
           2 2
           (lambda ()
             (setq tries (1+ tries))
             (cond
              ((not (buffer-live-p buffer))
               (cancel-timer timer))
              ((cj/--ai-term-color-ready-p buffer)
               (cancel-timer timer)
               (cj/--ai-term-send-color buffer color))
              ((>= tries 45)
               (cancel-timer timer))))))
    timer))

(defun cj/--ai-term-show-or-create (dir name &optional agent-command)
  "Show or create the AI-term buffer for project DIR with buffer NAME.

If a buffer named NAME exists with a live process, display it.  If
the buffer exists but its process is dead, kill it and recreate.  If
no such buffer exists, create a new EAT terminal in DIR and send
the project's tmux launch command (see `cj/--ai-term-launch-command') so
the same project basename reattaches across Emacs restarts.
AGENT-COMMAND, when non-nil, is the full agent launch command for a
fresh session (the multi-backend picker's choice); nil falls back to
`cj/ai-term-agent-command'.  A reattach ignores it (`tmux new-session
-A' attaches without running the command).

EAT runs a plain shell with no auto-tmux hook, so the named
`tmux new-session -A' launch command is the only thing that starts the
session -- the spike confirmed EAT + tmux detach and reattach exactly
like ghostel + tmux did.

Records DIR in `cj/--ai-term-mru' (whichever branch runs) so the
project picker can list recently-opened projects first.  Returns the
buffer."
  (cj/--ai-term-record-mru dir)
  (let ((existing (get-buffer name)))
    (cond
     ((and existing (cj/--ai-term-process-live-p existing))
      (display-buffer existing)
      existing)
     (t
      (when existing
        (kill-buffer existing))
      ;; Fresh vs reattach is decided BEFORE the launch command runs (the
      ;; `tmux new-session -A' it sends creates the session).  Only a fresh
      ;; session gets the project /color injected below; a reattach carries
      ;; whatever color the running Claude already has.
      (let ((fresh (not (cj/--ai-term-session-active-p
                         dir (cj/--ai-term-live-tmux-sessions)))))
        ;; `eat' switches to its buffer in the selected window before our
        ;; display-buffer-alist rule can route it; `save-window-excursion'
        ;; reverts that, and the explicit display-buffer below routes the buffer
        ;; through the alist into the agent slot.  `eat-buffer-name' is bound to
        ;; NAME so the terminal is created under the agent name; EAT (unlike
        ;; ghostel) does not rename the buffer from the terminal's OSC title, so
        ;; the "agent [" prefix that buffer detection and the display rule key on
        ;; stays put.
        (save-window-excursion
          (let ((default-directory dir)
                (eat-buffer-name name))
            (eat)))
        (let ((buf (get-buffer name)))
          (with-current-buffer buf
            (cj/--ai-term-apply-accent buf)
            (cj/--ai-term-send-string
             buf (concat (cj/--ai-term-launch-command dir agent-command) "\n")))
          (when fresh
            (cj/--ai-term-schedule-color buf (cj/--ai-term-project-color dir)))
          (display-buffer buf)
          buf))))))

;; In EAT's semi-char mode, keys not bound in `eat-semi-char-mode-map' are
;; forwarded to the pty.  The swap-to-next chords must reach Emacs from inside
;; an agent buffer, so bind them in that map -- no exception-list or rebuild
;; dance like ghostel needed.  C-; is already bound there (eat-config), so the
;; C-; a family resolves through the global prefix without extra wiring.
;; M-SPC cycles attached agents only; M-S-SPC cycles all (attaching a detached).
(with-eval-after-load 'eat
  (keymap-set eat-semi-char-mode-map "M-SPC" #'cj/ai-term-next-attached)
  (keymap-set eat-semi-char-mode-map "M-S-SPC" #'cj/ai-term-next))

(provide 'ai-term-backend-eat)
;;; ai-term-backend-eat.el ends here
