;;; test-custom-ordering-unarrayify.el --- Tests for cj/--unarrayify -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--unarrayify function from custom-ordering.el
;;
;; This function converts comma-separated array format back to separate lines.
;; It splits by ", " (comma-space), removes quotes (both " and '), and joins with newlines.
;;
;; Examples:
;; Input:  "\"apple\", \"banana\", \"cherry\""
;; Output: "apple\nbanana\ncherry"
;;
;; Input:  "'one', 'two', 'three'"
;; Output: "one\ntwo\nthree"
;;
;; We test the NON-INTERACTIVE implementation (cj/--unarrayify) to avoid
;; mocking region selection. This follows our testing best practice of
;; separating business logic from UI interaction.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-ordering)

;;; Test Helpers

(defun test-unarrayify (input-text)
  "Test cj/--unarrayify on INPUT-TEXT.
Returns the transformed string."
  (with-temp-buffer
    (insert input-text)
    (cj/--unarrayify (point-min) (point-max))))

;;; Normal Cases - Double Quotes

(ert-deftest test-unarrayify-double-quotes-simple ()
  "Should unarrayify double-quoted elements."
  (let ((result (test-unarrayify "\"apple\", \"banana\", \"cherry\"")))
    (should (string= result "apple\nbanana\ncherry"))))

(ert-deftest test-unarrayify-double-quotes-single-element ()
  "Should unarrayify single double-quoted element."
  (let ((result (test-unarrayify "\"hello\"")))
    (should (string= result "hello"))))

(ert-deftest test-unarrayify-double-quotes-two-elements ()
  "Should unarrayify two double-quoted elements."
  (let ((result (test-unarrayify "\"one\", \"two\"")))
    (should (string= result "one\ntwo"))))

;;; Normal Cases - Single Quotes

(ert-deftest test-unarrayify-single-quotes-simple ()
  "Should unarrayify single-quoted elements."
  (let ((result (test-unarrayify "'alpha', 'beta', 'gamma'")))
    (should (string= result "alpha\nbeta\ngamma"))))

(ert-deftest test-unarrayify-single-quotes-single-element ()
  "Should unarrayify single single-quoted element."
  (let ((result (test-unarrayify "'hello'")))
    (should (string= result "hello"))))

;;; Normal Cases - Mixed Quotes

(ert-deftest test-unarrayify-mixed-quotes ()
  "Should unarrayify mixed quote types."
  (let ((result (test-unarrayify "\"apple\", 'banana', \"cherry\"")))
    (should (string= result "apple\nbanana\ncherry"))))

;;; Normal Cases - No Quotes

(ert-deftest test-unarrayify-no-quotes ()
  "Should unarrayify unquoted elements."
  (let ((result (test-unarrayify "foo, bar, baz")))
    (should (string= result "foo\nbar\nbaz"))))

;;; Normal Cases - Various Content

(ert-deftest test-unarrayify-with-numbers ()
  "Should unarrayify numbers."
  (let ((result (test-unarrayify "\"1\", \"2\", \"3\"")))
    (should (string= result "1\n2\n3"))))

(ert-deftest test-unarrayify-with-spaces-in-elements ()
  "Should preserve spaces within elements."
  (let ((result (test-unarrayify "\"hello world\", \"foo bar\"")))
    (should (string= result "hello world\nfoo bar"))))

(ert-deftest test-unarrayify-mixed-content ()
  "Should unarrayify mixed alphanumeric content."
  (let ((result (test-unarrayify "\"item1\", \"item2\", \"item3\"")))
    (should (string= result "item1\nitem2\nitem3"))))

;;; Boundary Cases

(ert-deftest test-unarrayify-empty-string ()
  "Should handle empty string."
  (let ((result (test-unarrayify "")))
    (should (string= result ""))))

(ert-deftest test-unarrayify-only-quotes ()
  "Should remove quotes from quote-only string."
  (let ((result (test-unarrayify "\"\"")))
    (should (string= result ""))))

(ert-deftest test-unarrayify-very-long-list ()
  "Should handle very long list."
  (let* ((elements (mapcar (lambda (i) (format "\"%d\"" i)) (number-sequence 1 100)))
         (input (mapconcat #'identity elements ", "))
         (result (test-unarrayify input))
         (lines (split-string result "\n")))
    (should (= 100 (length lines)))))

(ert-deftest test-unarrayify-with-empty-elements ()
  "Should handle empty quoted elements."
  (let ((result (test-unarrayify "\"\", \"test\", \"\"")))
    (should (string= result "\ntest\n"))))

;;; Edge Cases - Nested or Mismatched Quotes

(ert-deftest test-unarrayify-double-quotes-in-single ()
  "Should handle double quotes inside single-quoted strings."
  (let ((result (test-unarrayify "'he said \"hello\"', 'world'")))
    (should (string= result "he said hello\nworld"))))

(ert-deftest test-unarrayify-only-opening-quotes ()
  "Should remove all quote characters even if mismatched."
  (let ((result (test-unarrayify "\"apple, \"banana, \"cherry")))
    (should (string= result "apple\nbanana\ncherry"))))

;;; Error Cases

(ert-deftest test-unarrayify-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "\"a\", \"b\"")
     (cj/--unarrayify (point-max) (point-min)))
   :type 'error))

(ert-deftest test-unarrayify-empty-region ()
  "Should handle empty region (start == end)."
  (with-temp-buffer
    (insert "\"a\", \"b\"")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "" (cj/--unarrayify pos pos))))))

(provide 'test-custom-ordering-unarrayify)
;;; test-custom-ordering-unarrayify.el ends here
