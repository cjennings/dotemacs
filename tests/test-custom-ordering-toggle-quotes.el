;;; test-custom-ordering-toggle-quotes.el --- Tests for cj/--toggle-quotes -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--toggle-quotes function from custom-ordering.el
;;
;; This function toggles between double quotes and single quotes.
;; All " become ' and all ' become ".
;;
;; Examples:
;; Input:  "apple", "banana"
;; Output: 'apple', 'banana'
;;
;; Input:  'hello', 'world'
;; Output: "hello", "world"
;;
;; We test the NON-INTERACTIVE implementation (cj/--toggle-quotes) to avoid
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

(defun test-toggle-quotes (input-text)
  "Test cj/--toggle-quotes on INPUT-TEXT.
Returns the transformed string."
  (with-temp-buffer
    (insert input-text)
    (cj/--toggle-quotes (point-min) (point-max))))

;;; Normal Cases - Double to Single

(ert-deftest test-toggle-quotes-double-to-single ()
  "Should convert double quotes to single quotes."
  (let ((result (test-toggle-quotes "\"apple\", \"banana\"")))
    (should (string= result "'apple', 'banana'"))))

(ert-deftest test-toggle-quotes-single-double-quote ()
  "Should convert single double quote."
  (let ((result (test-toggle-quotes "\"")))
    (should (string= result "'"))))

(ert-deftest test-toggle-quotes-multiple-double-quotes ()
  "Should convert multiple double quotes."
  (let ((result (test-toggle-quotes "\"hello\" \"world\" \"test\"")))
    (should (string= result "'hello' 'world' 'test'"))))

;;; Normal Cases - Single to Double

(ert-deftest test-toggle-quotes-single-to-double ()
  "Should convert single quotes to double quotes."
  (let ((result (test-toggle-quotes "'apple', 'banana'")))
    (should (string= result "\"apple\", \"banana\""))))

(ert-deftest test-toggle-quotes-single-single-quote ()
  "Should convert single single quote."
  (let ((result (test-toggle-quotes "'")))
    (should (string= result "\""))))

(ert-deftest test-toggle-quotes-multiple-single-quotes ()
  "Should convert multiple single quotes."
  (let ((result (test-toggle-quotes "'hello' 'world' 'test'")))
    (should (string= result "\"hello\" \"world\" \"test\""))))

;;; Normal Cases - Mixed Quotes

(ert-deftest test-toggle-quotes-mixed ()
  "Should toggle mixed quotes."
  (let ((result (test-toggle-quotes "\"double\" 'single'")))
    (should (string= result "'double' \"single\""))))

(ert-deftest test-toggle-quotes-bidirectional ()
  "Should toggle back and forth correctly."
  (let* ((original "\"apple\", \"banana\"")
         (toggled (test-toggle-quotes original))
         (back (test-toggle-quotes toggled)))
    (should (string= toggled "'apple', 'banana'"))
    (should (string= back original))))

;;; Normal Cases - With Text Content

(ert-deftest test-toggle-quotes-preserves-content ()
  "Should preserve content while toggling quotes."
  (let ((result (test-toggle-quotes "var x = \"hello world\";")))
    (should (string= result "var x = 'hello world';"))))

(ert-deftest test-toggle-quotes-sql-style ()
  "Should toggle SQL-style quotes."
  (let ((result (test-toggle-quotes "SELECT * FROM users WHERE name='John'")))
    (should (string= result "SELECT * FROM users WHERE name=\"John\""))))

(ert-deftest test-toggle-quotes-multiline ()
  "Should toggle quotes across multiple lines."
  (let ((result (test-toggle-quotes "\"line1\"\n\"line2\"\n\"line3\"")))
    (should (string= result "'line1'\n'line2'\n'line3'"))))

;;; Boundary Cases

(ert-deftest test-toggle-quotes-empty-string ()
  "Should handle empty string."
  (let ((result (test-toggle-quotes "")))
    (should (string= result ""))))

(ert-deftest test-toggle-quotes-no-quotes ()
  "Should handle text with no quotes."
  (let ((result (test-toggle-quotes "hello world")))
    (should (string= result "hello world"))))

(ert-deftest test-toggle-quotes-only-double-quotes ()
  "Should handle string with only double quotes."
  (let ((result (test-toggle-quotes "\"\"\"\"")))
    (should (string= result "''''"))))

(ert-deftest test-toggle-quotes-only-single-quotes ()
  "Should handle string with only single quotes."
  (let ((result (test-toggle-quotes "''''")))
    (should (string= result "\"\"\"\""))))

(ert-deftest test-toggle-quotes-adjacent-quotes ()
  "Should handle adjacent quotes."
  (let ((result (test-toggle-quotes "\"\"''")))
    (should (string= result "''\"\""))))

;;; Error Cases

(ert-deftest test-toggle-quotes-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "\"hello\"")
     (cj/--toggle-quotes (point-max) (point-min)))
   :type 'error))

(ert-deftest test-toggle-quotes-empty-region ()
  "Should handle empty region (start == end)."
  (with-temp-buffer
    (insert "\"hello\"")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "" (cj/--toggle-quotes pos pos))))))

(provide 'test-custom-ordering-toggle-quotes)
;;; test-custom-ordering-toggle-quotes.el ends here
