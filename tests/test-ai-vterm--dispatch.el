;;; test-ai-vterm--dispatch.el --- Tests for cj/--ai-vterm-dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; The dispatch helper is a pure decision function used by F9.
;; Returns one of (toggle-off . WIN), (redisplay-recent . BUF),
;; or (pick-project) based on whether a claude buffer is currently
;; displayed and whether any alive claude buffers exist.  Tests mock
;; the two underlying helpers so the dispatch logic can be exercised
;; without touching real windows.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

(ert-deftest test-ai-vterm--dispatch-window-displayed-returns-toggle-off ()
  "Normal: displayed claude window -> (toggle-off . WIN)."
  (let ((sentinel-win 'fake-window))
    (cl-letf (((symbol-function 'cj/--ai-vterm-displayed-claude-window)
               (lambda (&optional _frame) sentinel-win)))
      (should (equal (cj/--ai-vterm-dispatch)
                     (cons 'toggle-off sentinel-win))))))

(ert-deftest test-ai-vterm--dispatch-no-window-single-buffer-returns-redisplay-recent ()
  "Normal: no displayed claude, one alive buffer -> redisplay-recent + buffer."
  (cj/test--kill-claude-buffers)
  (let ((b1 (get-buffer-create "claude [single]")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-vterm-displayed-claude-window)
                   (lambda (&optional _frame) nil))
                  ((symbol-function 'cj/--ai-vterm-claude-buffers)
                   (lambda () (list b1))))
          (should (equal (cj/--ai-vterm-dispatch)
                         (cons 'redisplay-recent b1))))
      (kill-buffer b1))))

(ert-deftest test-ai-vterm--dispatch-no-window-multiple-buffers-returns-redisplay-recent ()
  "Normal: no displayed claude, 2+ alive buffers -> redisplay-recent + MRU.
F9 redisplays the most-recently-selected claude (head of buffer-list
order) rather than opening the project picker, so the user toggles
THE claude they were last using.  Other claudes are reachable via M-F9."
  (cj/test--kill-claude-buffers)
  (let ((b1 (get-buffer-create "claude [a]"))
        (b2 (get-buffer-create "claude [b]")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-vterm-displayed-claude-window)
                   (lambda (&optional _frame) nil))
                  ((symbol-function 'cj/--ai-vterm-claude-buffers)
                   (lambda () (list b1 b2))))
          (should (equal (cj/--ai-vterm-dispatch)
                         (cons 'redisplay-recent b1))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest test-ai-vterm--dispatch-no-window-zero-buffers-returns-pick-project ()
  "Boundary: no displayed claude, zero alive buffers -> pick-project."
  (cj/test--kill-claude-buffers)
  (cl-letf (((symbol-function 'cj/--ai-vterm-displayed-claude-window)
             (lambda (&optional _frame) nil))
            ((symbol-function 'cj/--ai-vterm-claude-buffers)
             (lambda () nil)))
    (should (equal (cj/--ai-vterm-dispatch) '(pick-project)))))

(provide 'test-ai-vterm--dispatch)
;;; test-ai-vterm--dispatch.el ends here
