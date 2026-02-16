;;; test-custom-case-upcase-dwim.el --- Tests for cj/upcase-dwim -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/upcase-dwim function from custom-case.el.
;;
;; This is a thin wrapper: if a region is active, upcase the region;
;; otherwise upcase the symbol at point. Signals user-error if no symbol.
;; Tests focus on the dispatch logic, not on Emacs's upcase-region.

;;; Code:

(require 'ert)

(unless (boundp 'cj/custom-keymap)
  (defvar cj/custom-keymap (make-sparse-keymap)))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-case)

;;; Normal Cases

(ert-deftest test-custom-case-upcase-dwim-normal-region-upcased ()
  "Active region should be upcased."
  (with-temp-buffer
    (insert "hello world")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (cj/upcase-dwim)
    (should (equal (buffer-string) "HELLO WORLD"))))

(ert-deftest test-custom-case-upcase-dwim-normal-partial-region ()
  "Only the selected region should be upcased."
  (with-temp-buffer
    (insert "hello world")
    (set-mark 1)
    (goto-char 6)
    (activate-mark)
    (cj/upcase-dwim)
    (should (equal (buffer-string) "HELLO world"))))

(ert-deftest test-custom-case-upcase-dwim-normal-symbol-at-point ()
  "Without region, symbol at point should be upcased."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (cj/upcase-dwim)
    (should (equal (buffer-string) "HELLO world"))))

(ert-deftest test-custom-case-upcase-dwim-normal-symbol-mid-word ()
  "Point in the middle of a symbol should upcase the whole symbol."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 3)
    (cj/upcase-dwim)
    (should (equal (buffer-string) "HELLO world"))))

(ert-deftest test-custom-case-upcase-dwim-normal-already-uppercase ()
  "Already uppercase text should remain unchanged."
  (with-temp-buffer
    (insert "HELLO world")
    (goto-char 1)
    (cj/upcase-dwim)
    (should (equal (buffer-string) "HELLO world"))))

;;; Boundary Cases

(ert-deftest test-custom-case-upcase-dwim-boundary-single-char-symbol ()
  "Single character symbol should be upcased."
  (with-temp-buffer
    (insert "a")
    (goto-char 1)
    (cj/upcase-dwim)
    (should (equal (buffer-string) "A"))))

;;; Error Cases

(ert-deftest test-custom-case-upcase-dwim-error-no-symbol-signals-error ()
  "With no region and no symbol at point, should signal user-error."
  (with-temp-buffer
    (insert "   ")
    (goto-char 2)
    (should-error (cj/upcase-dwim) :type 'user-error)))

(provide 'test-custom-case-upcase-dwim)
;;; test-custom-case-upcase-dwim.el ends here
