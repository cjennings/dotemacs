;;; test-ai-term--next-single-agent.el --- cj/ai-term-next sole-agent message -*- lexical-binding: t; -*-

;;; Commentary:
;; When M-SPC fires with a single ai-term open and that agent's window
;; selected, `cj/ai-term-next' has nowhere to go: the rotation wraps back
;; to the same agent.  Instead of a misleading "Agent: <name>" swap
;; message, it should say there are no other ai-terms to switch to.
;; A sole agent that is displayed but NOT selected still gets selected
;; (the swap key doubles as "take me to the agent"), and a sole agent
;; with no window still gets shown.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(defun test-ai-term-next-single--buffer-name (dir)
  "Deterministic fake agent buffer name for DIR."
  (concat "agent-buf-" dir))

(ert-deftest test-ai-term-next-single-agent-focused-messages-no-others ()
  "Normal: sole agent focused -> echo 'no other ai-terms', no swap."
  (let ((buf (get-buffer-create "agent-buf-a"))
        (captured nil)
        (shown 0))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (cl-letf (((symbol-function 'cj/--ai-term-active-agent-dirs)
                     (lambda (&rest _) '("a")))
                    ((symbol-function 'cj/--ai-term-displayed-agent-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'cj/--ai-term-buffer-name)
                     #'test-ai-term-next-single--buffer-name)
                    ((symbol-function 'cj/--ai-term-process-live-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'cj/--ai-term-show-or-create)
                     (lambda (&rest _) (setq shown (1+ shown))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (when fmt (setq captured (apply #'format fmt args))))))
            (cj/ai-term-next)
            (should (equal captured "No other ai-terms to switch to"))
            (should (= shown 0))
            (should (eq (window-buffer (selected-window)) buf))))
      (kill-buffer buf))))

(ert-deftest test-ai-term-next-single-agent-undisplayed-shows-it ()
  "Normal: sole agent with no window -> shown, not the no-others message."
  (let ((captured nil)
        (shown 0))
    (cl-letf (((symbol-function 'cj/--ai-term-active-agent-dirs)
               (lambda (&rest _) '("a")))
              ((symbol-function 'cj/--ai-term-displayed-agent-window)
               (lambda (&rest _) nil))
              ((symbol-function 'cj/--ai-term-buffer-name)
               #'test-ai-term-next-single--buffer-name)
              ((symbol-function 'cj/--ai-term-process-live-p)
               (lambda (&rest _) t))
              ((symbol-function 'cj/--ai-term-show-or-create)
               (lambda (&rest _) (setq shown (1+ shown))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (setq captured (apply #'format fmt args))))))
      (cj/ai-term-next)
      (should (= shown 1))
      (should-not (equal captured "No other ai-terms to switch to")))))

(ert-deftest test-ai-term-next-single-agent-unselected-window-gets-selected ()
  "Boundary: sole agent displayed in another window -> selected, no message change."
  (skip-unless (not noninteractive)) ; window splitting is unreliable in batch
  (let ((buf (get-buffer-create "agent-buf-a"))
        (captured nil))
    (unwind-protect
        (let* ((w1 (selected-window))
               (w2 (split-window)))
          (set-window-buffer w2 buf)
          (cl-letf (((symbol-function 'cj/--ai-term-active-agent-dirs)
                     (lambda (&rest _) '("a")))
                    ((symbol-function 'cj/--ai-term-displayed-agent-window)
                     (lambda (&rest _) w2))
                    ((symbol-function 'cj/--ai-term-buffer-name)
                     #'test-ai-term-next-single--buffer-name)
                    ((symbol-function 'cj/--ai-term-process-live-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (when fmt (setq captured (apply #'format fmt args))))))
            (select-window w1)
            (cj/ai-term-next)
            (should (eq (selected-window) w2))
            (should-not (equal captured "No other ai-terms to switch to"))))
      (delete-other-windows)
      (kill-buffer buf))))

(ert-deftest test-ai-term-next-two-agents-still-swaps ()
  "Normal: two agents, one focused -> swaps silently to the other.
The modeline announces the agent (buffer name + eat state), so the swap
emits no echo-area message (2026-07-02: the \"Agent: <name>\" echo was
dropped as clutter duplicating the modeline)."
  (let ((buf-a (get-buffer-create "agent-buf-a"))
        (buf-b (get-buffer-create "agent-buf-b"))
        (captured nil))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf-a)
          (cl-letf (((symbol-function 'cj/--ai-term-active-agent-dirs)
                     (lambda (&rest _) '("a" "b")))
                    ((symbol-function 'cj/--ai-term-displayed-agent-window)
                     (lambda (&rest _) (selected-window)))
                    ((symbol-function 'cj/--ai-term-buffer-name)
                     #'test-ai-term-next-single--buffer-name)
                    ((symbol-function 'cj/--ai-term-process-live-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (when fmt (setq captured (apply #'format fmt args))))))
            (cj/ai-term-next)
            (should (eq (window-buffer (selected-window)) buf-b))
            (should-not captured)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(provide 'test-ai-term--next-single-agent)
;;; test-ai-term--next-single-agent.el ends here
