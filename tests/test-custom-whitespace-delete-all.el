;;; test-custom-whitespace-delete-all.el --- Tests for cj/--delete-all-whitespace -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--delete-all-whitespace function from custom-whitespace.el
;;
;; This function removes ALL whitespace characters from the region:
;; spaces, tabs, newlines, and carriage returns. Useful for creating
;; compact identifiers or removing all formatting.
;;
;; Uses the regexp [ \t\n\r]+ to match all whitespace.
;;
;; We test the NON-INTERACTIVE implementation (cj/--delete-all-whitespace)
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

(defun test-delete-all-whitespace (input-text)
  "Test cj/--delete-all-whitespace on INPUT-TEXT.
Returns the buffer string after operation."
  (with-temp-buffer
    (insert input-text)
    (cj/--delete-all-whitespace (point-min) (point-max))
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-delete-all-whitespace-single-space ()
  "Should remove single space."
  (let ((result (test-delete-all-whitespace "hello world")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-multiple-spaces ()
  "Should remove multiple spaces."
  (let ((result (test-delete-all-whitespace "hello    world")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-tabs ()
  "Should remove tabs."
  (let ((result (test-delete-all-whitespace "hello\tworld")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-newlines ()
  "Should remove newlines (joining lines)."
  (let ((result (test-delete-all-whitespace "hello\nworld")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-mixed ()
  "Should remove all types of whitespace."
  (let ((result (test-delete-all-whitespace "hello \t\n world")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-multiple-words ()
  "Should remove whitespace from multiple words."
  (let ((result (test-delete-all-whitespace "one two three four")))
    (should (string= result "onetwothreefour"))))

(ert-deftest test-delete-all-whitespace-multiline ()
  "Should remove all whitespace across multiple lines."
  (let ((result (test-delete-all-whitespace "line1\nline2\nline3")))
    (should (string= result "line1line2line3"))))

(ert-deftest test-delete-all-whitespace-leading-trailing ()
  "Should remove leading and trailing whitespace."
  (let ((result (test-delete-all-whitespace "  hello world  ")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-carriage-returns ()
  "Should handle carriage returns."
  (let ((result (test-delete-all-whitespace "hello\r\nworld")))
    (should (string= result "helloworld"))))

;;; Boundary Cases

(ert-deftest test-delete-all-whitespace-empty-string ()
  "Should handle empty string."
  (let ((result (test-delete-all-whitespace "")))
    (should (string= result ""))))

(ert-deftest test-delete-all-whitespace-no-whitespace ()
  "Should handle text with no whitespace (no-op)."
  (let ((result (test-delete-all-whitespace "helloworld")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-only-whitespace ()
  "Should delete all content when only whitespace exists."
  (let ((result (test-delete-all-whitespace "   \t   \n   ")))
    (should (string= result ""))))

(ert-deftest test-delete-all-whitespace-single-char ()
  "Should handle single character with surrounding whitespace."
  (let ((result (test-delete-all-whitespace "  x  ")))
    (should (string= result "x"))))

(ert-deftest test-delete-all-whitespace-very-long-text ()
  "Should handle very long text."
  (let ((result (test-delete-all-whitespace "word word word word word word word word")))
    (should (string= result "wordwordwordwordwordwordwordword"))))

(ert-deftest test-delete-all-whitespace-single-whitespace ()
  "Should delete single whitespace character."
  (let ((result (test-delete-all-whitespace " ")))
    (should (string= result ""))))

(ert-deftest test-delete-all-whitespace-consecutive-newlines ()
  "Should remove all consecutive newlines."
  (let ((result (test-delete-all-whitespace "hello\n\n\nworld")))
    (should (string= result "helloworld"))))

(ert-deftest test-delete-all-whitespace-complex-structure ()
  "Should handle complex whitespace patterns."
  (let ((result (test-delete-all-whitespace "  hello\n\t  world  \n  foo\t\tbar  ")))
    (should (string= result "helloworldfoobar"))))

;;; Error Cases

(ert-deftest test-delete-all-whitespace-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--delete-all-whitespace (point-max) (point-min)))
   :type 'error))

(ert-deftest test-delete-all-whitespace-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--delete-all-whitespace pos pos)
      ;; Should complete without error and not change buffer
      (should (string= (buffer-string) "hello world")))))

(provide 'test-custom-whitespace-delete-all)
;;; test-custom-whitespace-delete-all.el ends here
