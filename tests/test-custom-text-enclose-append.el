;;; test-custom-text-enclose-append.el --- Tests for cj/--append-to-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--append-to-lines function from custom-text-enclose.el
;;
;; This function appends a suffix string to the end of each line in text.
;; It preserves the structure of lines and handles trailing newlines correctly.
;;
;; Examples:
;; Input:  "line1\nline2", suffix: ";"
;; Output: "line1;\nline2;"
;;
;; Input:  "single", suffix: "!"
;; Output: "single!"
;;
;; We test the NON-INTERACTIVE implementation (cj/--append-to-lines) to avoid
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

(defun test-append-to-lines (text suffix)
  "Test cj/--append-to-lines on TEXT with SUFFIX.
Returns the transformed string."
  (cj/--append-to-lines text suffix))

;;; Normal Cases - Single Line

(ert-deftest test-append-single-line ()
  "Should append to single line."
  (let ((result (test-append-to-lines "hello" ";")))
    (should (string= result "hello;"))))

(ert-deftest test-append-single-line-semicolon ()
  "Should append semicolon to single line."
  (let ((result (test-append-to-lines "var x = 5" ";")))
    (should (string= result "var x = 5;"))))

(ert-deftest test-append-single-line-exclamation ()
  "Should append exclamation mark to single line."
  (let ((result (test-append-to-lines "Hello world" "!")))
    (should (string= result "Hello world!"))))

;;; Normal Cases - Multiple Lines

(ert-deftest test-append-two-lines ()
  "Should append to two lines."
  (let ((result (test-append-to-lines "line1\nline2" ";")))
    (should (string= result "line1;\nline2;"))))

(ert-deftest test-append-three-lines ()
  "Should append to three lines."
  (let ((result (test-append-to-lines "a\nb\nc" ".")))
    (should (string= result "a.\nb.\nc."))))

(ert-deftest test-append-many-lines ()
  "Should append to many lines."
  (let* ((lines (make-list 10 "line"))
         (input (mapconcat #'identity lines "\n"))
         (result (test-append-to-lines input ";"))
         (result-lines (split-string result "\n")))
    (should (= 10 (length result-lines)))
    (should (cl-every (lambda (line) (string-suffix-p ";" line)) result-lines))))

;;; Normal Cases - Various Suffixes

(ert-deftest test-append-comma ()
  "Should append comma to lines."
  (let ((result (test-append-to-lines "apple\nbanana" ",")))
    (should (string= result "apple,\nbanana,"))))

(ert-deftest test-append-multi-char ()
  "Should append multi-character suffix."
  (let ((result (test-append-to-lines "line" " // comment")))
    (should (string= result "line // comment"))))

(ert-deftest test-append-pipe ()
  "Should append pipe character."
  (let ((result (test-append-to-lines "col1\ncol2" " |")))
    (should (string= result "col1 |\ncol2 |"))))

(ert-deftest test-append-empty-suffix ()
  "Should handle empty suffix."
  (let ((result (test-append-to-lines "line1\nline2" "")))
    (should (string= result "line1\nline2"))))

;;; Boundary Cases - Trailing Newlines

(ert-deftest test-append-with-trailing-newline ()
  "Should preserve trailing newline."
  (let ((result (test-append-to-lines "line1\nline2\n" ";")))
    (should (string= result "line1;\nline2;\n"))))

(ert-deftest test-append-no-trailing-newline ()
  "Should work without trailing newline."
  (let ((result (test-append-to-lines "line1\nline2" ";")))
    (should (string= result "line1;\nline2;"))))

(ert-deftest test-append-single-line-with-newline ()
  "Should preserve trailing newline on single line."
  (let ((result (test-append-to-lines "line\n" ";")))
    (should (string= result "line;\n"))))

;;; Boundary Cases - Empty Lines

(ert-deftest test-append-empty-line-between ()
  "Should append to empty line between other lines."
  (let ((result (test-append-to-lines "line1\n\nline3" ";")))
    (should (string= result "line1;\n;\nline3;"))))

(ert-deftest test-append-only-empty-lines ()
  "Should append to only empty lines."
  (let ((result (test-append-to-lines "\n\n" ";")))
    (should (string= result ";\n;\n"))))

(ert-deftest test-append-empty-first-line ()
  "Should append to empty first line."
  (let ((result (test-append-to-lines "\nline2\nline3" ";")))
    (should (string= result ";\nline2;\nline3;"))))

;;; Boundary Cases - Whitespace

(ert-deftest test-append-preserves-leading-whitespace ()
  "Should preserve leading whitespace."
  (let ((result (test-append-to-lines "  line1\n    line2" ";")))
    (should (string= result "  line1;\n    line2;"))))

(ert-deftest test-append-preserves-trailing-whitespace ()
  "Should preserve trailing whitespace on line."
  (let ((result (test-append-to-lines "line1  \nline2  " ";")))
    (should (string= result "line1  ;\nline2  ;"))))

(ert-deftest test-append-whitespace-only-line ()
  "Should append to whitespace-only line."
  (let ((result (test-append-to-lines "line1\n   \nline3" ";")))
    (should (string= result "line1;\n   ;\nline3;"))))

;;; Boundary Cases - Special Cases

(ert-deftest test-append-empty-string ()
  "Should handle empty string."
  (let ((result (test-append-to-lines "" ";")))
    (should (string= result ";"))))

(ert-deftest test-append-very-long-line ()
  "Should append to very long line."
  (let* ((long-line (make-string 1000 ?a))
         (result (test-append-to-lines long-line ";")))
    (should (string-suffix-p ";" result))
    (should (= (length result) 1001))))

(ert-deftest test-append-with-existing-suffix ()
  "Should append even if line already has the suffix."
  (let ((result (test-append-to-lines "line;" ";")))
    (should (string= result "line;;"))))

;;; Edge Cases - Special Characters in Suffix

(ert-deftest test-append-newline-suffix ()
  "Should append newline as suffix."
  (let ((result (test-append-to-lines "line1\nline2" "\n")))
    (should (string= result "line1\n\nline2\n"))))

(ert-deftest test-append-tab-suffix ()
  "Should append tab as suffix."
  (let ((result (test-append-to-lines "col1\ncol2" "\t")))
    (should (string= result "col1\t\ncol2\t"))))

(ert-deftest test-append-quote-suffix ()
  "Should append quote as suffix."
  (let ((result (test-append-to-lines "value1\nvalue2" "\"")))
    (should (string= result "value1\"\nvalue2\""))))

(provide 'test-custom-text-enclose-append)
;;; test-custom-text-enclose-append.el ends here
