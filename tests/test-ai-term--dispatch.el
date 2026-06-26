;;; test-ai-term--dispatch.el --- Tests for cj/--ai-term-dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; The dispatch helper is a pure decision function used by F9.
;; Returns one of (toggle-off . WIN), (redisplay-recent . BUF),
;; or (pick-project) based on whether an agent buffer is currently
;; displayed and whether any alive agent buffers exist.  Tests mock
;; the two underlying helpers so the dispatch logic can be exercised
;; without touching real windows.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-term)
(require 'testutil-terminal-buffers)

(ert-deftest test-ai-term--dispatch-window-displayed-returns-toggle-off ()
  "Normal: displayed agent window -> (toggle-off . WIN)."
  (let ((sentinel-win 'fake-window))
    (cl-letf (((symbol-function 'cj/--ai-term-displayed-agent-window)
               (lambda (&optional _frame) sentinel-win)))
      (should (equal (cj/--ai-term-dispatch)
                     (cons 'toggle-off sentinel-win))))))

(ert-deftest test-ai-term--dispatch-no-window-single-buffer-returns-redisplay-recent ()
  "Normal: no displayed agent, one alive buffer -> redisplay-recent + buffer."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [single]")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-displayed-agent-window)
                   (lambda (&optional _frame) nil))
                  ((symbol-function 'cj/--ai-term-agent-buffers)
                   (lambda () (list b1))))
          (should (equal (cj/--ai-term-dispatch)
                         (cons 'redisplay-recent b1))))
      (kill-buffer b1))))

(ert-deftest test-ai-term--dispatch-no-window-multiple-buffers-returns-redisplay-recent ()
  "Normal: no displayed agent, 2+ alive buffers -> redisplay-recent + MRU.
F9 redisplays the most-recently-selected agent (head of buffer-list
order) rather than opening the project picker, so the user toggles
THE agent they were last using.  Other agents are reachable via M-F9."
  (cj/test--kill-agent-buffers)
  (let ((b1 (get-buffer-create "agent [a]"))
        (b2 (get-buffer-create "agent [b]")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-displayed-agent-window)
                   (lambda (&optional _frame) nil))
                  ((symbol-function 'cj/--ai-term-agent-buffers)
                   (lambda () (list b1 b2))))
          (should (equal (cj/--ai-term-dispatch)
                         (cons 'redisplay-recent b1))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest test-ai-term--dispatch-no-window-zero-buffers-returns-pick-project ()
  "Boundary: no displayed agent, zero alive buffers -> pick-project."
  (cj/test--kill-agent-buffers)
  (cl-letf (((symbol-function 'cj/--ai-term-displayed-agent-window)
             (lambda (&optional _frame) nil))
            ((symbol-function 'cj/--ai-term-agent-buffers)
             (lambda () nil)))
    (should (equal (cj/--ai-term-dispatch) '(pick-project)))))

(provide 'test-ai-term--dispatch)
;;; test-ai-term--dispatch.el ends here
