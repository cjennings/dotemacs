;;; test-custom-text-enclose-unwrap.el --- Tests for cj/--unwrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--unwrap function from custom-text-enclose.el
;;
;; This function removes surrounding delimiters from text.
;; It checks if text starts with opening and ends with closing,
;; and if so, removes them.
;;
;; Examples:
;; Input:  "(text)", opening: "(", closing: ")"
;; Output: "text"
;;
;; Input:  "<div>content</div>", opening: "<div>", closing: "</div>"
;; Output: "content"
;;
;; We test the NON-INTERACTIVE implementation (cj/--unwrap) to avoid
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

(defun test-unwrap (text opening closing)
  "Test cj/--unwrap on TEXT with OPENING and CLOSING.
Returns the transformed string."
  (cj/--unwrap text opening closing))

;;; Normal Cases - Common Bracket Types

(ert-deftest test-unwrap-parentheses ()
  "Should unwrap text with parentheses."
  (let ((result (test-unwrap "(text)" "(" ")")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-square-brackets ()
  "Should unwrap text with square brackets."
  (let ((result (test-unwrap "[item]" "[" "]")))
    (should (string= result "item"))))

(ert-deftest test-unwrap-curly-braces ()
  "Should unwrap text with curly braces."
  (let ((result (test-unwrap "{code}" "{" "}")))
    (should (string= result "code"))))

(ert-deftest test-unwrap-angle-brackets ()
  "Should unwrap text with angle brackets."
  (let ((result (test-unwrap "<tag>" "<" ">")))
    (should (string= result "tag"))))

;;; Normal Cases - HTML/XML Tags

(ert-deftest test-unwrap-html-div ()
  "Should unwrap HTML div tags."
  (let ((result (test-unwrap "<div>content</div>" "<div>" "</div>")))
    (should (string= result "content"))))

(ert-deftest test-unwrap-html-span ()
  "Should unwrap HTML span tags."
  (let ((result (test-unwrap "<span>text</span>" "<span>" "</span>")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-xml-tag ()
  "Should unwrap XML tags."
  (let ((result (test-unwrap "<item>data</item>" "<item>" "</item>")))
    (should (string= result "data"))))

(ert-deftest test-unwrap-html-with-attributes ()
  "Should unwrap HTML tag containing attributes."
  (let ((result (test-unwrap "<a href=\"url\">link</a>" "<a href=\"url\">" "</a>")))
    (should (string= result "link"))))

;;; Normal Cases - Markdown Syntax

(ert-deftest test-unwrap-markdown-bold ()
  "Should unwrap markdown bold syntax."
  (let ((result (test-unwrap "**bold**" "**" "**")))
    (should (string= result "bold"))))

(ert-deftest test-unwrap-markdown-italic ()
  "Should unwrap markdown italic syntax."
  (let ((result (test-unwrap "*italic*" "*" "*")))
    (should (string= result "italic"))))

(ert-deftest test-unwrap-markdown-code ()
  "Should unwrap markdown code syntax."
  (let ((result (test-unwrap "`code`" "`" "`")))
    (should (string= result "code"))))

(ert-deftest test-unwrap-quotes ()
  "Should unwrap double quotes."
  (let ((result (test-unwrap "\"text\"" "\"" "\"")))
    (should (string= result "text"))))

;;; Normal Cases - Various Content

(ert-deftest test-unwrap-single-word ()
  "Should unwrap single word."
  (let ((result (test-unwrap "(word)" "(" ")")))
    (should (string= result "word"))))

(ert-deftest test-unwrap-multiple-words ()
  "Should unwrap multiple words."
  (let ((result (test-unwrap "(hello world)" "(" ")")))
    (should (string= result "hello world"))))

(ert-deftest test-unwrap-sentence ()
  "Should unwrap full sentence."
  (let ((result (test-unwrap "(This is a sentence.)" "(" ")")))
    (should (string= result "This is a sentence."))))

(ert-deftest test-unwrap-with-numbers ()
  "Should unwrap text with numbers."
  (let ((result (test-unwrap "[123]" "[" "]")))
    (should (string= result "123"))))

(ert-deftest test-unwrap-with-special-chars ()
  "Should unwrap text with special characters."
  (let ((result (test-unwrap "<hello@world.com>" "<" ">")))
    (should (string= result "hello@world.com"))))

;;; Normal Cases - Multiline Text

(ert-deftest test-unwrap-multiline ()
  "Should unwrap multiline text."
  (let ((result (test-unwrap "<div>line1\nline2\nline3</div>" "<div>" "</div>")))
    (should (string= result "line1\nline2\nline3"))))

(ert-deftest test-unwrap-text-with-newlines ()
  "Should unwrap text containing newlines."
  (let ((result (test-unwrap "(first\nsecond)" "(" ")")))
    (should (string= result "first\nsecond"))))

;;; Boundary Cases - No Match

(ert-deftest test-unwrap-no-opening ()
  "Should not unwrap when opening is missing."
  (let ((result (test-unwrap "text)" "(" ")")))
    (should (string= result "text)"))))

(ert-deftest test-unwrap-no-closing ()
  "Should not unwrap when closing is missing."
  (let ((result (test-unwrap "(text" "(" ")")))
    (should (string= result "(text"))))

(ert-deftest test-unwrap-neither-delimiter ()
  "Should not unwrap when neither delimiter is present."
  (let ((result (test-unwrap "text" "(" ")")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-wrong-opening ()
  "Should not unwrap with wrong opening delimiter."
  (let ((result (test-unwrap "[text)" "(" ")")))
    (should (string= result "[text)"))))

(ert-deftest test-unwrap-wrong-closing ()
  "Should not unwrap with wrong closing delimiter."
  (let ((result (test-unwrap "(text]" "(" ")")))
    (should (string= result "(text]"))))

;;; Boundary Cases - Empty

(ert-deftest test-unwrap-empty-content ()
  "Should unwrap to empty string."
  (let ((result (test-unwrap "()" "(" ")")))
    (should (string= result ""))))

(ert-deftest test-unwrap-just-delimiters ()
  "Should unwrap when only delimiters present."
  (let ((result (test-unwrap "[]" "[" "]")))
    (should (string= result ""))))

(ert-deftest test-unwrap-empty-string ()
  "Should return empty string unchanged."
  (let ((result (test-unwrap "" "(" ")")))
    (should (string= result ""))))

(ert-deftest test-unwrap-too-short ()
  "Should not unwrap when text is shorter than delimiters."
  (let ((result (test-unwrap "x" "<div>" "</div>")))
    (should (string= result "x"))))

;;; Boundary Cases - Nested/Multiple

(ert-deftest test-unwrap-nested-same ()
  "Should unwrap only outer layer of nested delimiters."
  (let ((result (test-unwrap "((text))" "(" ")")))
    (should (string= result "(text)"))))

(ert-deftest test-unwrap-nested-different ()
  "Should unwrap outer layer with different inner delimiters."
  (let ((result (test-unwrap "([text])" "(" ")")))
    (should (string= result "[text]"))))

(ert-deftest test-unwrap-multiple-in-content ()
  "Should not unwrap when delimiters appear in content."
  (let ((result (test-unwrap "(a)b(c)" "(" ")")))
    (should (string= result "a)b(c"))))

;;; Edge Cases - Special Delimiters

(ert-deftest test-unwrap-asymmetric-length ()
  "Should unwrap with different length delimiters."
  (let ((result (test-unwrap "<<text>>>" "<<" ">>>")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-multi-char-delimiters ()
  "Should unwrap with multi-character delimiters."
  (let ((result (test-unwrap "BEGINdataEND" "BEGIN" "END")))
    (should (string= result "data"))))

(ert-deftest test-unwrap-space-delimiters ()
  "Should unwrap with space delimiters."
  (let ((result (test-unwrap " text " " " " ")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-newline-delimiters ()
  "Should unwrap with newline delimiters."
  (let ((result (test-unwrap "\ntext\n" "\n" "\n")))
    (should (string= result "text"))))

;;; Edge Cases - Same Opening and Closing

(ert-deftest test-unwrap-same-delimiters ()
  "Should unwrap when opening and closing are the same."
  (let ((result (test-unwrap "*text*" "*" "*")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-same-multi-char ()
  "Should unwrap same multi-char delimiters."
  (let ((result (test-unwrap "***text***" "***" "***")))
    (should (string= result "text"))))

;;; Edge Cases - Empty Delimiters

(ert-deftest test-unwrap-empty-opening ()
  "Should handle empty opening delimiter."
  (let ((result (test-unwrap "text)" "" ")")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-empty-closing ()
  "Should handle empty closing delimiter."
  (let ((result (test-unwrap "(text" "(" "")))
    (should (string= result "text"))))

(ert-deftest test-unwrap-both-delimiters-empty ()
  "Should return text unchanged when both delimiters empty."
  (let ((result (test-unwrap "text" "" "")))
    (should (string= result "text"))))

(provide 'test-custom-text-enclose-unwrap)
;;; test-custom-text-enclose-unwrap.el ends here
