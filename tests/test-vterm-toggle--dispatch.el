;;; test-vterm-toggle--dispatch.el --- Tests for cj/--vterm-toggle-dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure decision helper for F12.  Returns one of (toggle-off . WIN),
;; (show-recent . BUFFER), or (create-new) based on whether a vterm
;; window is currently displayed and whether any vterm buffers are
;; alive.  Mocking the underlying helpers keeps the dispatch logic
;; exercisable without touching real windows.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'eshell-vterm-config)
(require 'testutil-vterm-buffers)

(ert-deftest test-vterm-toggle--dispatch-window-displayed-returns-toggle-off ()
  "Normal: displayed vterm window -> (toggle-off . WIN)."
  (let ((sentinel-win 'fake-window))
    (cl-letf (((symbol-function 'cj/--vterm-toggle-displayed-window)
               (lambda (&optional _frame) sentinel-win)))
      (should (equal (cj/--vterm-toggle-dispatch)
                     (cons 'toggle-off sentinel-win))))))

(ert-deftest test-vterm-toggle--dispatch-no-window-buffer-alive-returns-show-recent ()
  "Normal: no displayed vterm, at least one alive -> show-recent + first."
  (cj/test--kill-test-vterm-buffers)
  (let ((b1 (get-buffer-create "*test-vterm-mru-1*"))
        (b2 (get-buffer-create "*test-vterm-mru-2*")))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--vterm-toggle-displayed-window)
                   (lambda (&optional _frame) nil))
                  ((symbol-function 'cj/--vterm-toggle-buffers)
                   (lambda () (list b1 b2))))
          (should (equal (cj/--vterm-toggle-dispatch)
                         (cons 'show-recent b1))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest test-vterm-toggle--dispatch-no-window-no-buffer-returns-create-new ()
  "Boundary: nothing displayed, no alive vterms -> create-new."
  (cj/test--kill-test-vterm-buffers)
  (cl-letf (((symbol-function 'cj/--vterm-toggle-displayed-window)
             (lambda (&optional _frame) nil))
            ((symbol-function 'cj/--vterm-toggle-buffers)
             (lambda () nil)))
    (should (equal (cj/--vterm-toggle-dispatch) '(create-new)))))

(provide 'test-vterm-toggle--dispatch)
;;; test-vterm-toggle--dispatch.el ends here
