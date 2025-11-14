;;; test-custom-text-enclose-wrap.el --- Tests for cj/--wrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--wrap function from custom-text-enclose.el
;;
;; This function wraps text with different opening and closing strings.
;; Unlike surround which uses the same string on both sides, wrap allows
;; asymmetric delimiters.
;;
;; Examples:
;; Input:  "content", opening: "<div>", closing: "</div>"
;; Output: "<div>content</div>"
;;
;; Input:  "text", opening: "(", closing: ")"
;; Output: "(text)"
;;
;; We test the NON-INTERACTIVE implementation (cj/--wrap) to avoid
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

(defun test-wrap (text opening closing)
  "Test cj/--wrap on TEXT with OPENING and CLOSING.
Returns the transformed string."
  (cj/--wrap text opening closing))

;;; Normal Cases - Common Bracket Types

(ert-deftest test-wrap-parentheses ()
  "Should wrap text with parentheses."
  (let ((result (test-wrap "text" "(" ")")))
    (should (string= result "(text)"))))

(ert-deftest test-wrap-square-brackets ()
  "Should wrap text with square brackets."
  (let ((result (test-wrap "item" "[" "]")))
    (should (string= result "[item]"))))

(ert-deftest test-wrap-curly-braces ()
  "Should wrap text with curly braces."
  (let ((result (test-wrap "code" "{" "}")))
    (should (string= result "{code}"))))

(ert-deftest test-wrap-angle-brackets ()
  "Should wrap text with angle brackets."
  (let ((result (test-wrap "tag" "<" ">")))
    (should (string= result "<tag>"))))

;;; Normal Cases - HTML/XML Tags

(ert-deftest test-wrap-html-div ()
  "Should wrap text with HTML div tags."
  (let ((result (test-wrap "content" "<div>" "</div>")))
    (should (string= result "<div>content</div>"))))

(ert-deftest test-wrap-html-span ()
  "Should wrap text with HTML span tags."
  (let ((result (test-wrap "text" "<span>" "</span>")))
    (should (string= result "<span>text</span>"))))

(ert-deftest test-wrap-xml-tag ()
  "Should wrap text with XML tags."
  (let ((result (test-wrap "data" "<item>" "</item>")))
    (should (string= result "<item>data</item>"))))

(ert-deftest test-wrap-html-with-attributes ()
  "Should wrap text with HTML tag containing attributes."
  (let ((result (test-wrap "link" "<a href=\"url\">" "</a>")))
    (should (string= result "<a href=\"url\">link</a>"))))

;;; Normal Cases - Markdown Syntax

(ert-deftest test-wrap-markdown-bold ()
  "Should wrap text with markdown bold syntax."
  (let ((result (test-wrap "bold" "**" "**")))
    (should (string= result "**bold**"))))

(ert-deftest test-wrap-markdown-italic ()
  "Should wrap text with markdown italic syntax."
  (let ((result (test-wrap "italic" "*" "*")))
    (should (string= result "*italic*"))))

(ert-deftest test-wrap-markdown-code ()
  "Should wrap text with markdown code syntax."
  (let ((result (test-wrap "code" "`" "`")))
    (should (string= result "`code`"))))

(ert-deftest test-wrap-markdown-link ()
  "Should wrap text with markdown link syntax."
  (let ((result (test-wrap "text" "[" "](url)")))
    (should (string= result "[text](url)"))))

;;; Normal Cases - Various Content

(ert-deftest test-wrap-single-word ()
  "Should wrap single word."
  (let ((result (test-wrap "word" "(" ")")))
    (should (string= result "(word)"))))

(ert-deftest test-wrap-multiple-words ()
  "Should wrap multiple words."
  (let ((result (test-wrap "hello world" "(" ")")))
    (should (string= result "(hello world)"))))

(ert-deftest test-wrap-sentence ()
  "Should wrap full sentence."
  (let ((result (test-wrap "This is a sentence." "(" ")")))
    (should (string= result "(This is a sentence.)"))))

(ert-deftest test-wrap-with-numbers ()
  "Should wrap text with numbers."
  (let ((result (test-wrap "123" "[" "]")))
    (should (string= result "[123]"))))

(ert-deftest test-wrap-with-special-chars ()
  "Should wrap text with special characters."
  (let ((result (test-wrap "hello@world.com" "<" ">")))
    (should (string= result "<hello@world.com>"))))

;;; Normal Cases - Multiline Text

(ert-deftest test-wrap-multiline ()
  "Should wrap multiline text."
  (let ((result (test-wrap "line1\nline2\nline3" "<div>" "</div>")))
    (should (string= result "<div>line1\nline2\nline3</div>"))))

(ert-deftest test-wrap-text-with-newlines ()
  "Should wrap text containing newlines."
  (let ((result (test-wrap "first\nsecond" "(" ")")))
    (should (string= result "(first\nsecond)"))))

;;; Boundary Cases

(ert-deftest test-wrap-empty-string ()
  "Should wrap empty string."
  (let ((result (test-wrap "" "(" ")")))
    (should (string= result "()"))))

(ert-deftest test-wrap-single-character ()
  "Should wrap single character."
  (let ((result (test-wrap "x" "[" "]")))
    (should (string= result "[x]"))))

(ert-deftest test-wrap-empty-opening ()
  "Should handle empty opening delimiter."
  (let ((result (test-wrap "text" "" ")")))
    (should (string= result "text)"))))

(ert-deftest test-wrap-empty-closing ()
  "Should handle empty closing delimiter."
  (let ((result (test-wrap "text" "(" "")))
    (should (string= result "(text"))))

(ert-deftest test-wrap-both-empty ()
  "Should handle both delimiters empty."
  (let ((result (test-wrap "text" "" "")))
    (should (string= result "text"))))

(ert-deftest test-wrap-very-long-text ()
  "Should wrap very long text."
  (let* ((long-text (make-string 1000 ?a))
         (result (test-wrap long-text "(" ")")))
    (should (string-prefix-p "(" result))
    (should (string-suffix-p ")" result))
    (should (= (length result) 1002))))

(ert-deftest test-wrap-whitespace-only ()
  "Should wrap whitespace-only text."
  (let ((result (test-wrap "   " "(" ")")))
    (should (string= result "(   )"))))

(ert-deftest test-wrap-tabs ()
  "Should wrap text with tabs."
  (let ((result (test-wrap "\t\ttext\t\t" "[" "]")))
    (should (string= result "[\t\ttext\t\t]"))))

;;; Edge Cases - Already Wrapped

(ert-deftest test-wrap-already-wrapped ()
  "Should wrap text that is already wrapped."
  (let ((result (test-wrap "(hello)" "[" "]")))
    (should (string= result "[(hello)]"))))

(ert-deftest test-wrap-nested ()
  "Should wrap text creating nested delimiters."
  (let ((result (test-wrap "[inner]" "(" ")")))
    (should (string= result "([inner])"))))

;;; Edge Cases - Special Delimiters

(ert-deftest test-wrap-asymmetric-length ()
  "Should wrap with different length delimiters."
  (let ((result (test-wrap "text" "<<" ">>>")))
    (should (string= result "<<text>>>"))))

(ert-deftest test-wrap-multi-char-delimiters ()
  "Should wrap with multi-character delimiters."
  (let ((result (test-wrap "data" "BEGIN" "END")))
    (should (string= result "BEGINdataEND"))))

(ert-deftest test-wrap-space-delimiters ()
  "Should wrap with space delimiters."
  (let ((result (test-wrap "text" " " " ")))
    (should (string= result " text "))))

(ert-deftest test-wrap-newline-delimiters ()
  "Should wrap with newline delimiters."
  (let ((result (test-wrap "text" "\n" "\n")))
    (should (string= result "\ntext\n"))))

(ert-deftest test-wrap-quote-delimiters ()
  "Should wrap with quote delimiters."
  (let ((result (test-wrap "text" "\"" "\"")))
    (should (string= result "\"text\""))))

;;; Edge Cases - Same Opening and Closing

(ert-deftest test-wrap-same-delimiters ()
  "Should work like surround when delimiters are the same."
  (let ((result (test-wrap "text" "*" "*")))
    (should (string= result "*text*"))))

(provide 'test-custom-text-enclose-wrap)
;;; test-custom-text-enclose-wrap.el ends here
