;;; test-custom-whitespace-collapse.el --- Tests for cj/--collapse-whitespace -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--collapse-whitespace function from custom-whitespace.el
;;
;; This function collapses whitespace in text by:
;; - Converting all tabs to spaces
;; - Removing leading and trailing whitespace
;; - Collapsing multiple consecutive spaces to single space
;; - Preserving newlines and text structure
;;
;; We test the NON-INTERACTIVE implementation (cj/--collapse-whitespace)
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
(require 'custom-whitespace)

;;; Test Helpers

(defun test-collapse-whitespace (input-text)
  "Test cj/--collapse-whitespace on INPUT-TEXT.
Returns the buffer string after operation."
  (with-temp-buffer
    (insert input-text)
    (cj/--collapse-whitespace (point-min) (point-max))
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-collapse-whitespace-multiple-spaces ()
  "Should collapse multiple spaces to single space."
  (let ((result (test-collapse-whitespace "hello    world")))
    (should (string= result "hello world"))))

(ert-deftest test-collapse-whitespace-multiple-tabs ()
  "Should convert tabs to spaces and collapse."
  (let ((result (test-collapse-whitespace "hello\t\t\tworld")))
    (should (string= result "hello world"))))

(ert-deftest test-collapse-whitespace-mixed-tabs-spaces ()
  "Should handle mixed tabs and spaces."
  (let ((result (test-collapse-whitespace "hello \t  \t world")))
    (should (string= result "hello world"))))

(ert-deftest test-collapse-whitespace-leading-trailing ()
  "Should remove leading and trailing whitespace."
  (let ((result (test-collapse-whitespace "   hello world   ")))
    (should (string= result "hello world"))))

(ert-deftest test-collapse-whitespace-tabs-leading-trailing ()
  "Should remove leading and trailing tabs."
  (let ((result (test-collapse-whitespace "\t\thello world\t\t")))
    (should (string= result "hello world"))))

(ert-deftest test-collapse-whitespace-multiple-words ()
  "Should collapse spaces between multiple words."
  (let ((result (test-collapse-whitespace "one   two    three     four")))
    (should (string= result "one two three four"))))

(ert-deftest test-collapse-whitespace-preserve-newlines ()
  "Should preserve newlines while collapsing spaces."
  (let ((result (test-collapse-whitespace "hello   world\nfoo   bar")))
    (should (string= result "hello world\nfoo bar"))))

(ert-deftest test-collapse-whitespace-multiple-lines ()
  "Should handle multiple lines with various whitespace."
  (let ((result (test-collapse-whitespace "  hello   world  \n\t\tfoo    bar\t\t")))
    (should (string= result "hello world\nfoo bar"))))

;;; Boundary Cases

(ert-deftest test-collapse-whitespace-empty-string ()
  "Should handle empty string."
  (let ((result (test-collapse-whitespace "")))
    (should (string= result ""))))

(ert-deftest test-collapse-whitespace-single-char ()
  "Should handle single character with surrounding spaces."
  (let ((result (test-collapse-whitespace "   x   ")))
    (should (string= result "x"))))

(ert-deftest test-collapse-whitespace-only-whitespace ()
  "Should handle text with only whitespace (becomes empty)."
  (let ((result (test-collapse-whitespace "   \t   \t   ")))
    (should (string= result ""))))

(ert-deftest test-collapse-whitespace-no-extra-whitespace ()
  "Should handle text with no extra whitespace (no-op)."
  (let ((result (test-collapse-whitespace "hello world")))
    (should (string= result "hello world"))))

(ert-deftest test-collapse-whitespace-single-space ()
  "Should handle text with already-collapsed spaces (no-op)."
  (let ((result (test-collapse-whitespace "one two three")))
    (should (string= result "one two three"))))

(ert-deftest test-collapse-whitespace-very-long-line ()
  "Should handle very long lines with many spaces."
  (let ((result (test-collapse-whitespace "word      word      word      word      word")))
    (should (string= result "word word word word word"))))

(ert-deftest test-collapse-whitespace-multiple-newlines ()
  "Should preserve multiple newlines while removing spaces."
  (let ((result (test-collapse-whitespace "hello   world\n\n\nfoo   bar")))
    (should (string= result "hello world\n\n\nfoo bar"))))

(ert-deftest test-collapse-whitespace-spaces-around-newlines ()
  "Should remove spaces before/after newlines."
  (let ((result (test-collapse-whitespace "hello   \n   world")))
    (should (string= result "hello\nworld"))))

(ert-deftest test-collapse-whitespace-empty-lines ()
  "Should handle empty lines (lines become empty after whitespace removal)."
  (let ((result (test-collapse-whitespace "line1\n   \nline2")))
    (should (string= result "line1\n\nline2"))))

;;; Error Cases

(ert-deftest test-collapse-whitespace-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--collapse-whitespace (point-max) (point-min)))
   :type 'error))

(ert-deftest test-collapse-whitespace-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--collapse-whitespace pos pos)
      ;; Should complete without error and not change buffer
      (should (string= (buffer-string) "hello world")))))

(provide 'test-custom-whitespace-collapse)
;;; test-custom-whitespace-collapse.el ends here
