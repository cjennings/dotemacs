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
(defvar eat-buffer-name)
(defvar eat-semi-char-mode-map)
(defvar eat-terminal)
(defvar cj/ai-term-accent-color-indices)

(defun cj/--ai-term-send-string (buffer string)
  "Send STRING to BUFFER's terminal process (the agent's shell).
Sends to the pty directly so the launch command reaches the shell EAT runs."
  (let ((proc (get-buffer-process buffer)))
    (when (process-live-p proc)
      (process-send-string proc string))))

(defun cj/--ai-term-apply-accent (buffer)
  "Point BUFFER's terminal accent palette entries at `cj/ai-term-accent'.
Repaints each index in `cj/ai-term-accent-color-indices' in this
terminal's own 256-color palette (eat keeps one per terminal), so the
agent's accent -- Claude Code's rose banner, borders, spinner -- renders
in the accent face's color while every other eat terminal keeps the true
palette.  A no-op when BUFFER has no live eat terminal.  Takes effect on
the terminal's next redraw; text already on screen keeps its old color
until the program repaints it (Claude Code's TUI repaints continuously)."
  (with-current-buffer buffer
    (when (bound-and-true-p eat-terminal)
      (dolist (index cj/ai-term-accent-color-indices)
        (eat-term-set-parameter eat-terminal
                                (intern (format "color-%d-face" index))
                                'cj/ai-term-accent)))))

(defun cj/--ai-term-show-or-create (dir name)
  "Show or create the AI-term buffer for project DIR with buffer NAME.

If a buffer named NAME exists with a live process, display it.  If
the buffer exists but its process is dead, kill it and recreate.  If
no such buffer exists, create a new EAT terminal in DIR and send
the project's tmux launch command (see `cj/--ai-term-launch-command') so
the same project basename reattaches across Emacs restarts.

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
           buf (concat (cj/--ai-term-launch-command dir) "\n")))
        (display-buffer buf)
        buf)))))

;; In EAT's semi-char mode, keys not bound in `eat-semi-char-mode-map' are
;; forwarded to the pty.  M-SPC (swap to the next agent) must reach Emacs from
;; inside an agent buffer, so bind it in that map -- no exception-list or rebuild
;; dance like ghostel needed.  C-; is already bound there (eat-config), so the
;; C-; a family resolves through the global prefix without extra wiring.
(with-eval-after-load 'eat
  (keymap-set eat-semi-char-mode-map "M-SPC" #'cj/ai-term-next))

(provide 'ai-term-backend-eat)
;;; ai-term-backend-eat.el ends here
