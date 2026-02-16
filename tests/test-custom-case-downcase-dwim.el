;;; test-custom-case-downcase-dwim.el --- Tests for cj/downcase-dwim -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/downcase-dwim function from custom-case.el.
;;
;; Mirror of upcase-dwim: if a region is active, downcase the region;
;; otherwise downcase the symbol at point. Signals user-error if no symbol.
;; Tests focus on the dispatch logic, not on Emacs's downcase-region.

;;; Code:

(require 'ert)

(unless (boundp 'cj/custom-keymap)
  (defvar cj/custom-keymap (make-sparse-keymap)))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-case)

;;; Normal Cases

(ert-deftest test-custom-case-downcase-dwim-normal-region-downcased ()
  "Active region should be downcased."
  (with-temp-buffer
    (insert "HELLO WORLD")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (cj/downcase-dwim)
    (should (equal (buffer-string) "hello world"))))

(ert-deftest test-custom-case-downcase-dwim-normal-partial-region ()
  "Only the selected region should be downcased."
  (with-temp-buffer
    (insert "HELLO WORLD")
    (set-mark 1)
    (goto-char 6)
    (activate-mark)
    (cj/downcase-dwim)
    (should (equal (buffer-string) "hello WORLD"))))

(ert-deftest test-custom-case-downcase-dwim-normal-symbol-at-point ()
  "Without region, symbol at point should be downcased."
  (with-temp-buffer
    (insert "HELLO WORLD")
    (goto-char 1)
    (cj/downcase-dwim)
    (should (equal (buffer-string) "hello WORLD"))))

(ert-deftest test-custom-case-downcase-dwim-normal-symbol-mid-word ()
  "Point in the middle of a symbol should downcase the whole symbol."
  (with-temp-buffer
    (insert "HELLO WORLD")
    (goto-char 3)
    (cj/downcase-dwim)
    (should (equal (buffer-string) "hello WORLD"))))

(ert-deftest test-custom-case-downcase-dwim-normal-already-lowercase ()
  "Already lowercase text should remain unchanged."
  (with-temp-buffer
    (insert "hello WORLD")
    (goto-char 1)
    (cj/downcase-dwim)
    (should (equal (buffer-string) "hello WORLD"))))

;;; Boundary Cases

(ert-deftest test-custom-case-downcase-dwim-boundary-single-char-symbol ()
  "Single character symbol should be downcased."
  (with-temp-buffer
    (insert "A")
    (goto-char 1)
    (cj/downcase-dwim)
    (should (equal (buffer-string) "a"))))

;;; Error Cases

(ert-deftest test-custom-case-downcase-dwim-error-no-symbol-signals-error ()
  "With no region and no symbol at point, should signal user-error."
  (with-temp-buffer
    (insert "   ")
    (goto-char 2)
    (should-error (cj/downcase-dwim) :type 'user-error)))

(provide 'test-custom-case-downcase-dwim)
;;; test-custom-case-downcase-dwim.el ends here
