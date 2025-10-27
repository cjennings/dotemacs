;;; test-custom-ordering-arrayify.el --- Tests for cj/--arrayify -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--arrayify function from custom-ordering.el
;;
;; This function converts lines of text into a quoted, comma-separated array format.
;; It splits input by whitespace, wraps each element in quotes, and joins with ", ".
;;
;; Examples:
;; Input:  "apple\nbanana\ncherry"
;; Output: "\"apple\", \"banana\", \"cherry\""
;;
;; Input:  "one two three" (with single quotes)
;; Output: "'one', 'two', 'three'"
;;
;; We test the NON-INTERACTIVE implementation (cj/--arrayify) to avoid
;; mocking user input for quote characters. This follows our testing best
;; practice of separating business logic from UI interaction.

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

(defun test-arrayify (input-text quote)
  "Test cj/--arrayify on INPUT-TEXT with QUOTE character.
Returns the transformed string."
  (with-temp-buffer
    (insert input-text)
    (cj/--arrayify (point-min) (point-max) quote)))

(defun test-arrayify-with-prefix-suffix (input-text quote prefix suffix)
  "Test cj/--arrayify with PREFIX and SUFFIX on INPUT-TEXT.
Returns the transformed string."
  (with-temp-buffer
    (insert input-text)
    (cj/--arrayify (point-min) (point-max) quote prefix suffix)))

;;; Normal Cases - Double Quotes

(ert-deftest test-arrayify-single-line-double-quotes ()
  "Should arrayify single line with double quotes."
  (let ((result (test-arrayify "apple banana cherry" "\"")))
    (should (string= result "\"apple\", \"banana\", \"cherry\""))))

(ert-deftest test-arrayify-multiple-lines-double-quotes ()
  "Should arrayify multiple lines with double quotes."
  (let ((result (test-arrayify "apple\nbanana\ncherry" "\"")))
    (should (string= result "\"apple\", \"banana\", \"cherry\""))))

(ert-deftest test-arrayify-mixed-whitespace-double-quotes ()
  "Should arrayify text with mixed whitespace using double quotes."
  (let ((result (test-arrayify "apple  \n\n  banana\t\tcherry" "\"")))
    (should (string= result "\"apple\", \"banana\", \"cherry\""))))

;;; Normal Cases - Single Quotes

(ert-deftest test-arrayify-single-line-single-quotes ()
  "Should arrayify single line with single quotes."
  (let ((result (test-arrayify "one two three" "'")))
    (should (string= result "'one', 'two', 'three'"))))

(ert-deftest test-arrayify-multiple-lines-single-quotes ()
  "Should arrayify multiple lines with single quotes."
  (let ((result (test-arrayify "one\ntwo\nthree" "'")))
    (should (string= result "'one', 'two', 'three'"))))

;;; Normal Cases - Various Quote Types

(ert-deftest test-arrayify-backticks ()
  "Should arrayify with backticks."
  (let ((result (test-arrayify "foo bar baz" "`")))
    (should (string= result "`foo`, `bar`, `baz`"))))

(ert-deftest test-arrayify-no-quotes ()
  "Should arrayify with empty quote string."
  (let ((result (test-arrayify "alpha beta gamma" "")))
    (should (string= result "alpha, beta, gamma"))))

(ert-deftest test-arrayify-square-brackets ()
  "Should arrayify with square brackets as quotes."
  (let ((result (test-arrayify "x y z" "[]")))
    (should (string= result "[]x[], []y[], []z[]"))))

;;; Normal Cases - Various Content

(ert-deftest test-arrayify-with-numbers ()
  "Should arrayify numbers."
  (let ((result (test-arrayify "1 2 3 4 5" "\"")))
    (should (string= result "\"1\", \"2\", \"3\", \"4\", \"5\""))))

(ert-deftest test-arrayify-with-punctuation ()
  "Should arrayify words with punctuation."
  (let ((result (test-arrayify "hello! world? test." "\"")))
    (should (string= result "\"hello!\", \"world?\", \"test.\""))))

(ert-deftest test-arrayify-mixed-content ()
  "Should arrayify mixed alphanumeric content."
  (let ((result (test-arrayify "item1 item2 item3" "\"")))
    (should (string= result "\"item1\", \"item2\", \"item3\""))))

;;; Boundary Cases

(ert-deftest test-arrayify-empty-string ()
  "Should handle empty string."
  (let ((result (test-arrayify "" "\"")))
    (should (string= result ""))))

(ert-deftest test-arrayify-single-word ()
  "Should arrayify single word."
  (let ((result (test-arrayify "hello" "\"")))
    (should (string= result "\"hello\""))))

(ert-deftest test-arrayify-only-whitespace ()
  "Should handle whitespace-only text."
  (let ((result (test-arrayify "   \n\n\t\t   " "\"")))
    (should (string= result ""))))

(ert-deftest test-arrayify-leading-trailing-whitespace ()
  "Should ignore leading and trailing whitespace."
  (let ((result (test-arrayify "   apple banana   " "\"")))
    (should (string= result "\"apple\", \"banana\""))))

(ert-deftest test-arrayify-very-long-list ()
  "Should handle very long list."
  (let* ((words (make-list 100 "word"))
         (input (mapconcat #'identity words " "))
         (result (test-arrayify input "\"")))
    (should (= 100 (length (split-string result ", "))))))

(ert-deftest test-arrayify-two-words ()
  "Should arrayify two words."
  (let ((result (test-arrayify "hello world" "\"")))
    (should (string= result "\"hello\", \"world\""))))

;;; Normal Cases - Prefix/Suffix

(ert-deftest test-arrayify-with-square-brackets ()
  "Should arrayify with square brackets prefix/suffix."
  (let ((result (test-arrayify-with-prefix-suffix "apple banana cherry" "\"" "[" "]")))
    (should (string= result "[\"apple\", \"banana\", \"cherry\"]"))))

(ert-deftest test-arrayify-with-parens ()
  "Should arrayify with parentheses prefix/suffix."
  (let ((result (test-arrayify-with-prefix-suffix "one two three" "\"" "(" ")")))
    (should (string= result "(\"one\", \"two\", \"three\")"))))

(ert-deftest test-arrayify-unquoted-with-brackets ()
  "Should create unquoted list with brackets."
  (let ((result (test-arrayify-with-prefix-suffix "a b c" "" "[" "]")))
    (should (string= result "[a, b, c]"))))

(ert-deftest test-arrayify-single-quotes-with-brackets ()
  "Should create single-quoted array with brackets."
  (let ((result (test-arrayify-with-prefix-suffix "x y z" "'" "[" "]")))
    (should (string= result "['x', 'y', 'z']"))))

(ert-deftest test-arrayify-only-prefix ()
  "Should handle only prefix, no suffix."
  (let ((result (test-arrayify-with-prefix-suffix "foo bar" "\"" "[" nil)))
    (should (string= result "[\"foo\", \"bar\""))))

(ert-deftest test-arrayify-only-suffix ()
  "Should handle only suffix, no prefix."
  (let ((result (test-arrayify-with-prefix-suffix "foo bar" "\"" nil "]")))
    (should (string= result "\"foo\", \"bar\"]"))))

(ert-deftest test-arrayify-multichar-prefix-suffix ()
  "Should handle multi-character prefix/suffix."
  (let ((result (test-arrayify-with-prefix-suffix "a b" "\"" "Array(" ")")))
    (should (string= result "Array(\"a\", \"b\")"))))

(ert-deftest test-arrayify-json-style ()
  "Should create JSON-style array."
  (let ((result (test-arrayify-with-prefix-suffix "apple banana" "\"" "[" "]")))
    (should (string= result "[\"apple\", \"banana\"]"))))

;;; Error Cases

(ert-deftest test-arrayify-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--arrayify (point-max) (point-min) "\""))
   :type 'error))

(ert-deftest test-arrayify-empty-region ()
  "Should handle empty region (start == end)."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "" (cj/--arrayify pos pos "\""))))))

(ert-deftest test-arrayify-empty-region-with-brackets ()
  "Should handle empty region with brackets."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "[]" (cj/--arrayify pos pos "\"" "[" "]"))))))

(provide 'test-custom-ordering-arrayify)
;;; test-custom-ordering-arrayify.el ends here
