;;; test-custom-misc-format-region.el --- Tests for cj/--format-region -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--format-region function from custom-misc.el
;;
;; This function reformats text by applying three operations:
;; 1. untabify - converts tabs to spaces
;; 2. indent-region - reindents according to major mode
;; 3. delete-trailing-whitespace - removes trailing whitespace
;;
;; Note: indent-region behavior is major-mode dependent. We test in
;; emacs-lisp-mode and fundamental-mode for predictable results.
;;
;; We test the NON-INTERACTIVE implementation (cj/--format-region)
;; to avoid mocking region selection. This follows our testing best practice
;; of separating business logic from UI interaction.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-misc)

;;; Test Helpers

(defun test-format-region (input-text &optional mode)
  "Test cj/--format-region on INPUT-TEXT.
MODE is the major mode to use (defaults to fundamental-mode).
Returns the buffer string after operation."
  (with-temp-buffer
    (funcall (or mode #'fundamental-mode))
    (insert input-text)
    (cj/--format-region (point-min) (point-max))
    (buffer-string)))

;;; Normal Cases - Tab Conversion

(ert-deftest test-format-region-converts-tabs ()
  "Should convert tabs to spaces."
  (let ((result (test-format-region "hello\tworld")))
    (should-not (string-match-p "\t" result))
    (should (string-match-p " " result))))

(ert-deftest test-format-region-multiple-tabs ()
  "Should convert multiple tabs."
  (let ((result (test-format-region "\t\thello\t\tworld\t\t")))
    (should-not (string-match-p "\t" result))))

;;; Normal Cases - Trailing Whitespace

(ert-deftest test-format-region-removes-trailing-spaces ()
  "Should remove trailing spaces."
  (let ((result (test-format-region "hello world   ")))
    (should (string= result "hello world"))))

(ert-deftest test-format-region-removes-trailing-tabs ()
  "Should remove trailing tabs."
  (let ((result (test-format-region "hello world\t\t")))
    (should (string= result "hello world"))))

(ert-deftest test-format-region-removes-trailing-mixed ()
  "Should remove trailing mixed whitespace."
  (let ((result (test-format-region "hello world \t \t ")))
    (should (string= result "hello world"))))

(ert-deftest test-format-region-multiline-trailing ()
  "Should remove trailing whitespace from multiple lines."
  (let ((result (test-format-region "line1   \nline2\t\t\nline3 \t ")))
    (should (string= result "line1\nline2\nline3"))))

;;; Normal Cases - Combined Operations

(ert-deftest test-format-region-tabs-and-trailing ()
  "Should handle both tabs and trailing whitespace."
  (let ((result (test-format-region "\thello\tworld\t\t")))
    (should-not (string-match-p "\t" result))
    ;; Should not end with whitespace
    (should-not (string-match-p "[ \t]+$" result))))

(ert-deftest test-format-region-preserves-interior-spaces ()
  "Should preserve interior spaces while fixing edges."
  (let ((result (test-format-region "\thello   world\t")))
    (should (string-match-p "hello   world" result))
    (should-not (string-match-p "\t" result))))

;;; Normal Cases - Indentation (Mode-Specific)

(ert-deftest test-format-region-elisp-indentation ()
  "Should reindent Elisp code."
  (let* ((input "(defun foo ()\n(+ 1 2))")
         (result (test-format-region input #'emacs-lisp-mode))
         (lines (split-string result "\n")))
    ;; The inner form should be indented - second line should start with 2 spaces
    (should (= 2 (length lines)))
    (should (string-prefix-p "(defun foo ()" (car lines)))
    (should (string-prefix-p "  " (cadr lines)))))

;;; Boundary Cases

(ert-deftest test-format-region-empty-string ()
  "Should handle empty string."
  (let ((result (test-format-region "")))
    (should (string= result ""))))

(ert-deftest test-format-region-no-issues ()
  "Should handle text with no formatting issues (no-op)."
  (let ((result (test-format-region "hello world")))
    (should (string= result "hello world"))))

(ert-deftest test-format-region-only-whitespace ()
  "Should handle text with only whitespace."
  (let ((result (test-format-region "\t   \t   ")))
    ;; Should become empty or just spaces, no tabs
    (should-not (string-match-p "\t" result))))

(ert-deftest test-format-region-single-line ()
  "Should handle single line."
  (let ((result (test-format-region "\thello\t")))
    (should-not (string-match-p "\t" result))))

(ert-deftest test-format-region-very-long-text ()
  "Should handle very long text."
  (let* ((long-text (mapconcat (lambda (_) "\thello\t") (make-list 100 nil) "\n"))
         (result (test-format-region long-text)))
    (should-not (string-match-p "\t" result))))

(ert-deftest test-format-region-newlines-preserved ()
  "Should preserve newlines while fixing formatting."
  (let ((result (test-format-region "line1\t \nline2\t \nline3\t ")))
    (should (= 2 (cl-count ?\n result)))))

;;; Error Cases

(ert-deftest test-format-region-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--format-region (point-max) (point-min)))
   :type 'error))

(ert-deftest test-format-region-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--format-region pos pos)
      ;; Should complete without error
      (should (string= (buffer-string) "hello world")))))

(provide 'test-custom-misc-format-region)
;;; test-custom-misc-format-region.el ends here
