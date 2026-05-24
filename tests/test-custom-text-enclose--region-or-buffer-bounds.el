;;; test-custom-text-enclose--region-or-buffer-bounds.el --- Tests for bounds helper -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/--region-or-buffer-bounds, the single source of the
;; "operate on the active region, else the whole buffer" contract shared by
;; the *-in-region-or-buffer editing commands.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-text-enclose)

;;; Normal Cases

(ert-deftest test-ctenc-bounds-uses-region-when-active ()
  "Normal: with an active region, returns its beginning and end."
  (cl-letf (((symbol-function 'use-region-p) (lambda () t))
            ((symbol-function 'region-beginning) (lambda () 3))
            ((symbol-function 'region-end) (lambda () 8)))
    (should (equal '(3 . 8) (cj/--region-or-buffer-bounds)))))

(ert-deftest test-ctenc-bounds-uses-whole-buffer-when-no-region ()
  "Normal: with no active region, returns the whole buffer's bounds."
  (with-temp-buffer
    (insert "line one\nline two\n")
    (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
      (should (equal (cons (point-min) (point-max))
                     (cj/--region-or-buffer-bounds))))))

;;; Boundary Cases

(ert-deftest test-ctenc-bounds-empty-buffer-no-region ()
  "Boundary: an empty buffer with no region yields equal start and end."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
      (let ((b (cj/--region-or-buffer-bounds)))
        (should (= (car b) (cdr b)))))))

(provide 'test-custom-text-enclose--region-or-buffer-bounds)
;;; test-custom-text-enclose--region-or-buffer-bounds.el ends here
