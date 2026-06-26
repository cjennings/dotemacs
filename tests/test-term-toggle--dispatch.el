;;; test-term-toggle--dispatch.el --- Tests for cj/--term-toggle-dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure decision helper for F12.  Returns one of (toggle-off . WIN),
;; (show-recent . BUFFER), or (create-new) based on whether a terminal
;; window is currently displayed and whether any terminal buffers are
;; alive.  Mocking the underlying helpers keeps the dispatch logic
;; exercisable without touching real windows.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'eat-config)
(require 'testutil-terminal-buffers)

(ert-deftest test-term-toggle--dispatch-window-displayed-returns-toggle-off ()
  "Normal: displayed terminal window -> (toggle-off . WIN)."
  (let ((sentinel-win 'fake-window))
    (cl-letf (((symbol-function 'cj/--term-toggle-displayed-window)
               (lambda (&optional _frame) sentinel-win)))
      (should (equal (cj/--term-toggle-dispatch)
                     (cons 'toggle-off sentinel-win))))))

(ert-deftest test-term-toggle--dispatch-no-window-buffer-alive-returns-show-recent ()
  "Normal: no displayed terminal, at least one alive -> show-recent + first."
  (cj/test--kill-test-term-buffers)
  (let ((b1 (get-buffer-create "*test-term-mru-1*"))
        (b2 (get-buffer-create "*test-term-mru-2*")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--term-toggle-displayed-window)
                   (lambda (&optional _frame) nil))
                  ((symbol-function 'cj/--term-toggle-buffers)
                   (lambda () (list b1 b2))))
          (should (equal (cj/--term-toggle-dispatch)
                         (cons 'show-recent b1))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest test-term-toggle--dispatch-no-window-no-buffer-returns-create-new ()
  "Boundary: nothing displayed, no alive terminals -> create-new."
  (cj/test--kill-test-term-buffers)
  (cl-letf (((symbol-function 'cj/--term-toggle-displayed-window)
             (lambda (&optional _frame) nil))
            ((symbol-function 'cj/--term-toggle-buffers)
             (lambda () nil)))
    (should (equal (cj/--term-toggle-dispatch) '(create-new)))))

(provide 'test-term-toggle--dispatch)
;;; test-term-toggle--dispatch.el ends here
