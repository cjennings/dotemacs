;;; test-custom-ordering-comma-to-lines.el --- Tests for cj/--comma-separated-text-to-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--comma-separated-text-to-lines function from custom-ordering.el
;;
;; This function converts comma-separated text to separate lines.
;; It replaces commas with newlines and removes trailing whitespace from each line.
;;
;; Examples:
;; Input:  "apple, banana, cherry"
;; Output: "apple\nbanana\ncherry"
;;
;; Input:  "one,two,three"
;; Output: "one\ntwo\nthree"
;;
;; We test the NON-INTERACTIVE implementation (cj/--comma-separated-text-to-lines)
;; to avoid mocking region selection. This follows our testing best practice of
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

(defun test-comma-to-lines (input-text)
  "Test cj/--comma-separated-text-to-lines on INPUT-TEXT.
Returns the transformed string."
  (with-temp-buffer
    (insert input-text)
    (cj/--comma-separated-text-to-lines (point-min) (point-max))))

;;; Normal Cases - Simple Comma-Separated

(ert-deftest test-comma-to-lines-simple ()
  "Should convert simple comma-separated text to lines."
  (let ((result (test-comma-to-lines "apple, banana, cherry")))
    (should (string= result "apple\n banana\n cherry"))))

(ert-deftest test-comma-to-lines-no-spaces ()
  "Should convert comma-separated text without spaces."
  (let ((result (test-comma-to-lines "one,two,three")))
    (should (string= result "one\ntwo\nthree"))))

(ert-deftest test-comma-to-lines-two-elements ()
  "Should convert two comma-separated elements."
  (let ((result (test-comma-to-lines "hello,world")))
    (should (string= result "hello\nworld"))))

(ert-deftest test-comma-to-lines-with-varied-spacing ()
  "Should preserve leading spaces after commas."
  (let ((result (test-comma-to-lines "alpha,  beta,   gamma")))
    (should (string= result "alpha\n  beta\n   gamma"))))

;;; Normal Cases - Trailing Whitespace

(ert-deftest test-comma-to-lines-trailing-spaces ()
  "Should remove trailing spaces but preserve leading spaces."
  (let ((result (test-comma-to-lines "apple  ,  banana  ,  cherry  ")))
    (should (string= result "apple\n  banana\n  cherry"))))

(ert-deftest test-comma-to-lines-trailing-tabs ()
  "Should remove trailing tabs after conversion."
  (let ((result (test-comma-to-lines "apple\t,banana\t,cherry\t")))
    (should (string= result "apple\nbanana\ncherry"))))

;;; Boundary Cases

(ert-deftest test-comma-to-lines-empty-string ()
  "Should handle empty string."
  (let ((result (test-comma-to-lines "")))
    (should (string= result ""))))

(ert-deftest test-comma-to-lines-single-element ()
  "Should handle single element with no comma."
  (let ((result (test-comma-to-lines "hello")))
    (should (string= result "hello"))))

(ert-deftest test-comma-to-lines-single-element-with-trailing-comma ()
  "Should handle single element with trailing comma."
  (let ((result (test-comma-to-lines "hello,")))
    (should (string= result "hello\n"))))

(ert-deftest test-comma-to-lines-leading-comma ()
  "Should handle leading comma."
  (let ((result (test-comma-to-lines ",apple,banana")))
    (should (string= result "\napple\nbanana"))))

(ert-deftest test-comma-to-lines-consecutive-commas ()
  "Should handle consecutive commas."
  (let ((result (test-comma-to-lines "apple,,banana")))
    (should (string= result "apple\n\nbanana"))))

(ert-deftest test-comma-to-lines-many-consecutive-commas ()
  "Should handle many consecutive commas."
  (let ((result (test-comma-to-lines "apple,,,banana")))
    (should (string= result "apple\n\n\nbanana"))))

(ert-deftest test-comma-to-lines-only-commas ()
  "Should handle string with only commas (trailing blank lines removed)."
  (let ((result (test-comma-to-lines ",,,")))
    ;; delete-trailing-whitespace removes trailing blank lines
    (should (string= result "\n"))))

;;; Normal Cases - With Spaces Around Elements

(ert-deftest test-comma-to-lines-leading-spaces ()
  "Should preserve leading spaces within elements."
  (let ((result (test-comma-to-lines " apple,  banana,   cherry")))
    (should (string= result " apple\n  banana\n   cherry"))))

(ert-deftest test-comma-to-lines-mixed-content ()
  "Should handle mixed alphanumeric content."
  (let ((result (test-comma-to-lines "item1,item2,item3")))
    (should (string= result "item1\nitem2\nitem3"))))

(ert-deftest test-comma-to-lines-with-numbers ()
  "Should handle numbers."
  (let ((result (test-comma-to-lines "1,2,3,4,5")))
    (should (string= result "1\n2\n3\n4\n5"))))

(ert-deftest test-comma-to-lines-very-long-list ()
  "Should handle very long list."
  (let* ((elements (mapcar #'number-to-string (number-sequence 1 100)))
         (input (mapconcat #'identity elements ","))
         (result (test-comma-to-lines input))
         (lines (split-string result "\n")))
    (should (= 100 (length lines)))))

;;; Error Cases

(ert-deftest test-comma-to-lines-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "a,b,c")
     (cj/--comma-separated-text-to-lines (point-max) (point-min)))
   :type 'error))

(ert-deftest test-comma-to-lines-empty-region ()
  "Should handle empty region (start == end)."
  (with-temp-buffer
    (insert "a,b,c")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "" (cj/--comma-separated-text-to-lines pos pos))))))

(provide 'test-custom-ordering-comma-to-lines)
;;; test-custom-ordering-comma-to-lines.el ends here
