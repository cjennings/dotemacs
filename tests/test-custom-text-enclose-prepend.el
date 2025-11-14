;;; test-custom-text-enclose-prepend.el --- Tests for cj/--prepend-to-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--prepend-to-lines function from custom-text-enclose.el
;;
;; This function prepends a prefix string to the beginning of each line in text.
;; It preserves the structure of lines and handles trailing newlines correctly.
;;
;; Examples:
;; Input:  "line1\nline2", prefix: "// "
;; Output: "// line1\n// line2"
;;
;; Input:  "single", prefix: "> "
;; Output: "> single"
;;
;; We test the NON-INTERACTIVE implementation (cj/--prepend-to-lines) to avoid
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
(require 'custom-text-enclose)

;;; Test Helpers

(defun test-prepend-to-lines (text prefix)
  "Test cj/--prepend-to-lines on TEXT with PREFIX.
Returns the transformed string."
  (cj/--prepend-to-lines text prefix))

;;; Normal Cases - Single Line

(ert-deftest test-prepend-single-line ()
  "Should prepend to single line."
  (let ((result (test-prepend-to-lines "hello" "> ")))
    (should (string= result "> hello"))))

(ert-deftest test-prepend-single-line-comment ()
  "Should prepend comment marker to single line."
  (let ((result (test-prepend-to-lines "code here" "// ")))
    (should (string= result "// code here"))))

(ert-deftest test-prepend-single-line-bullet ()
  "Should prepend bullet to single line."
  (let ((result (test-prepend-to-lines "item" "- ")))
    (should (string= result "- item"))))

;;; Normal Cases - Multiple Lines

(ert-deftest test-prepend-two-lines ()
  "Should prepend to two lines."
  (let ((result (test-prepend-to-lines "line1\nline2" "> ")))
    (should (string= result "> line1\n> line2"))))

(ert-deftest test-prepend-three-lines ()
  "Should prepend to three lines."
  (let ((result (test-prepend-to-lines "a\nb\nc" "* ")))
    (should (string= result "* a\n* b\n* c"))))

(ert-deftest test-prepend-many-lines ()
  "Should prepend to many lines."
  (let* ((lines (make-list 10 "line"))
         (input (mapconcat #'identity lines "\n"))
         (result (test-prepend-to-lines input "# "))
         (result-lines (split-string result "\n")))
    (should (= 10 (length result-lines)))
    (should (cl-every (lambda (line) (string-prefix-p "# " line)) result-lines))))

;;; Normal Cases - Various Prefixes

(ert-deftest test-prepend-comment-marker ()
  "Should prepend comment marker."
  (let ((result (test-prepend-to-lines "line1\nline2" "// ")))
    (should (string= result "// line1\n// line2"))))

(ert-deftest test-prepend-hash-comment ()
  "Should prepend hash comment."
  (let ((result (test-prepend-to-lines "line1\nline2" "# ")))
    (should (string= result "# line1\n# line2"))))

(ert-deftest test-prepend-multi-char ()
  "Should prepend multi-character prefix."
  (let ((result (test-prepend-to-lines "line" "TODO: ")))
    (should (string= result "TODO: line"))))

(ert-deftest test-prepend-empty-prefix ()
  "Should handle empty prefix."
  (let ((result (test-prepend-to-lines "line1\nline2" "")))
    (should (string= result "line1\nline2"))))

;;; Boundary Cases - Trailing Newlines

(ert-deftest test-prepend-with-trailing-newline ()
  "Should preserve trailing newline."
  (let ((result (test-prepend-to-lines "line1\nline2\n" "> ")))
    (should (string= result "> line1\n> line2\n"))))

(ert-deftest test-prepend-no-trailing-newline ()
  "Should work without trailing newline."
  (let ((result (test-prepend-to-lines "line1\nline2" "> ")))
    (should (string= result "> line1\n> line2"))))

(ert-deftest test-prepend-single-line-with-newline ()
  "Should preserve trailing newline on single line."
  (let ((result (test-prepend-to-lines "line\n" "> ")))
    (should (string= result "> line\n"))))

;;; Boundary Cases - Empty Lines

(ert-deftest test-prepend-empty-line-between ()
  "Should prepend to empty line between other lines."
  (let ((result (test-prepend-to-lines "line1\n\nline3" "> ")))
    (should (string= result "> line1\n> \n> line3"))))

(ert-deftest test-prepend-only-empty-lines ()
  "Should prepend to only empty lines."
  (let ((result (test-prepend-to-lines "\n\n" "> ")))
    (should (string= result "> \n> \n"))))

(ert-deftest test-prepend-empty-first-line ()
  "Should prepend to empty first line."
  (let ((result (test-prepend-to-lines "\nline2\nline3" "> ")))
    (should (string= result "> \n> line2\n> line3"))))

;;; Boundary Cases - Whitespace

(ert-deftest test-prepend-preserves-leading-whitespace ()
  "Should preserve leading whitespace after prefix."
  (let ((result (test-prepend-to-lines "  line1\n    line2" "// ")))
    (should (string= result "//   line1\n//     line2"))))

(ert-deftest test-prepend-preserves-trailing-whitespace ()
  "Should preserve trailing whitespace on line."
  (let ((result (test-prepend-to-lines "line1  \nline2  " "> ")))
    (should (string= result "> line1  \n> line2  "))))

(ert-deftest test-prepend-whitespace-only-line ()
  "Should prepend to whitespace-only line."
  (let ((result (test-prepend-to-lines "line1\n   \nline3" "> ")))
    (should (string= result "> line1\n>    \n> line3"))))

;;; Boundary Cases - Special Cases

(ert-deftest test-prepend-empty-string ()
  "Should handle empty string."
  (let ((result (test-prepend-to-lines "" "> ")))
    (should (string= result "> "))))

(ert-deftest test-prepend-very-long-line ()
  "Should prepend to very long line."
  (let* ((long-line (make-string 1000 ?a))
         (result (test-prepend-to-lines long-line "> ")))
    (should (string-prefix-p "> " result))
    (should (= (length result) 1002))))

(ert-deftest test-prepend-with-existing-prefix ()
  "Should prepend even if line already has the prefix."
  (let ((result (test-prepend-to-lines "> line" "> ")))
    (should (string= result "> > line"))))

;;; Edge Cases - Special Characters in Prefix

(ert-deftest test-prepend-newline-prefix ()
  "Should prepend newline as prefix."
  (let ((result (test-prepend-to-lines "line1\nline2" "\n")))
    (should (string= result "\nline1\n\nline2"))))

(ert-deftest test-prepend-tab-prefix ()
  "Should prepend tab as prefix."
  (let ((result (test-prepend-to-lines "line1\nline2" "\t")))
    (should (string= result "\tline1\n\tline2"))))

(ert-deftest test-prepend-quote-prefix ()
  "Should prepend quote as prefix."
  (let ((result (test-prepend-to-lines "line1\nline2" "\"")))
    (should (string= result "\"line1\n\"line2"))))

;;; Edge Cases - Common Use Cases

(ert-deftest test-prepend-markdown-quote ()
  "Should prepend markdown quote marker."
  (let ((result (test-prepend-to-lines "quote text\nmore text" "> ")))
    (should (string= result "> quote text\n> more text"))))

(ert-deftest test-prepend-numbered-list ()
  "Should prepend numbers (though simpler uses would vary the prefix)."
  (let ((result (test-prepend-to-lines "item" "1. ")))
    (should (string= result "1. item"))))

(ert-deftest test-prepend-indentation ()
  "Should prepend indentation spaces."
  (let ((result (test-prepend-to-lines "code\nmore" "    ")))
    (should (string= result "    code\n    more"))))

(provide 'test-custom-text-enclose-prepend)
;;; test-custom-text-enclose-prepend.el ends here
