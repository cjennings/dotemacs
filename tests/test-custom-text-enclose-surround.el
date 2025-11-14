;;; test-custom-text-enclose-surround.el --- Tests for cj/--surround -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--surround function from custom-text-enclose.el
;;
;; This function surrounds text with a given string.
;; The surround string is both prepended and appended to the text.
;;
;; Examples:
;; Input:  "hello", surround: "\""
;; Output: "\"hello\""
;;
;; Input:  "world", surround: "**"
;; Output: "**world**"
;;
;; We test the NON-INTERACTIVE implementation (cj/--surround) to avoid
;; mocking user input. This follows our testing best practice of
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
(require 'custom-text-enclose)

;;; Test Helpers

(defun test-surround (text surround-string)
  "Test cj/--surround on TEXT with SURROUND-STRING.
Returns the transformed string."
  (cj/--surround text surround-string))

;;; Normal Cases - Common Surround Strings

(ert-deftest test-surround-double-quotes ()
  "Should surround text with double quotes."
  (let ((result (test-surround "hello" "\"")))
    (should (string= result "\"hello\""))))

(ert-deftest test-surround-single-quotes ()
  "Should surround text with single quotes."
  (let ((result (test-surround "world" "'")))
    (should (string= result "'world'"))))

(ert-deftest test-surround-parentheses ()
  "Should surround text with parentheses."
  (let ((result (test-surround "text" "(")))
    (should (string= result "(text("))))

(ert-deftest test-surround-square-brackets ()
  "Should surround text with square brackets."
  (let ((result (test-surround "item" "[")))
    (should (string= result "[item["))))

(ert-deftest test-surround-asterisks ()
  "Should surround text with asterisks for markdown."
  (let ((result (test-surround "bold" "*")))
    (should (string= result "*bold*"))))

(ert-deftest test-surround-double-asterisks ()
  "Should surround text with double asterisks."
  (let ((result (test-surround "bold" "**")))
    (should (string= result "**bold**"))))

;;; Normal Cases - Multi-Character Surround Strings

(ert-deftest test-surround-html-tag ()
  "Should surround text with HTML-like tags."
  (let ((result (test-surround "content" "<tag>")))
    (should (string= result "<tag>content<tag>"))))

(ert-deftest test-surround-backticks ()
  "Should surround text with backticks for code."
  (let ((result (test-surround "code" "`")))
    (should (string= result "`code`"))))

(ert-deftest test-surround-triple-backticks ()
  "Should surround text with triple backticks."
  (let ((result (test-surround "code block" "```")))
    (should (string= result "```code block```"))))

(ert-deftest test-surround-custom-delimiter ()
  "Should surround text with custom delimiter."
  (let ((result (test-surround "data" "||")))
    (should (string= result "||data||"))))

;;; Normal Cases - Various Text Content

(ert-deftest test-surround-single-word ()
  "Should surround single word."
  (let ((result (test-surround "word" "\"")))
    (should (string= result "\"word\""))))

(ert-deftest test-surround-multiple-words ()
  "Should surround multiple words."
  (let ((result (test-surround "hello world" "\"")))
    (should (string= result "\"hello world\""))))

(ert-deftest test-surround-sentence ()
  "Should surround full sentence."
  (let ((result (test-surround "This is a sentence." "\"")))
    (should (string= result "\"This is a sentence.\""))))

(ert-deftest test-surround-with-numbers ()
  "Should surround text with numbers."
  (let ((result (test-surround "123" "'")))
    (should (string= result "'123'"))))

(ert-deftest test-surround-with-special-chars ()
  "Should surround text with special characters."
  (let ((result (test-surround "hello@world.com" "\"")))
    (should (string= result "\"hello@world.com\""))))

;;; Normal Cases - Multiline Text

(ert-deftest test-surround-multiline ()
  "Should surround multiline text."
  (let ((result (test-surround "line1\nline2\nline3" "\"")))
    (should (string= result "\"line1\nline2\nline3\""))))

(ert-deftest test-surround-text-with-newlines ()
  "Should surround text containing newlines."
  (let ((result (test-surround "first\nsecond" "**")))
    (should (string= result "**first\nsecond**"))))

;;; Boundary Cases

(ert-deftest test-surround-empty-string ()
  "Should surround empty string."
  (let ((result (test-surround "" "\"")))
    (should (string= result "\"\""))))

(ert-deftest test-surround-single-character ()
  "Should surround single character."
  (let ((result (test-surround "x" "\"")))
    (should (string= result "\"x\""))))

(ert-deftest test-surround-empty-surround-string ()
  "Should handle empty surround string."
  (let ((result (test-surround "hello" "")))
    (should (string= result "hello"))))

(ert-deftest test-surround-very-long-text ()
  "Should surround very long text."
  (let* ((long-text (make-string 1000 ?a))
         (result (test-surround long-text "\"")))
    (should (string-prefix-p "\"" result))
    (should (string-suffix-p "\"" result))
    (should (= (length result) 1002))))

(ert-deftest test-surround-whitespace-only ()
  "Should surround whitespace-only text."
  (let ((result (test-surround "   " "\"")))
    (should (string= result "\"   \""))))

(ert-deftest test-surround-tabs ()
  "Should surround text with tabs."
  (let ((result (test-surround "\t\ttext\t\t" "\"")))
    (should (string= result "\"\t\ttext\t\t\""))))

;;; Edge Cases - Already Surrounded

(ert-deftest test-surround-already-quoted ()
  "Should surround text that is already quoted."
  (let ((result (test-surround "\"hello\"" "\"")))
    (should (string= result "\"\"hello\"\""))))

(ert-deftest test-surround-nested ()
  "Should surround text creating nested delimiters."
  (let ((result (test-surround "'inner'" "\"")))
    (should (string= result "\"'inner'\""))))

;;; Edge Cases - Special Surround Strings

(ert-deftest test-surround-space ()
  "Should surround text with spaces."
  (let ((result (test-surround "text" " ")))
    (should (string= result " text "))))

(ert-deftest test-surround-newline ()
  "Should surround text with newlines."
  (let ((result (test-surround "text" "\n")))
    (should (string= result "\ntext\n"))))

(ert-deftest test-surround-mixed-delimiters ()
  "Should surround with mixed delimiter string."
  (let ((result (test-surround "content" "<>")))
    (should (string= result "<>content<>"))))

(provide 'test-custom-text-enclose-surround)
;;; test-custom-text-enclose-surround.el ends here
