;;; test-custom-text-enclose-indent.el --- Tests for cj/--indent-lines and cj/--dedent-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--indent-lines and cj/--dedent-lines functions from custom-text-enclose.el
;;
;; cj/--indent-lines adds leading whitespace (spaces or tabs) to each line.
;; cj/--dedent-lines removes up to COUNT leading whitespace characters from each line.
;;
;; Examples (indent):
;; Input:  "line1\nline2", count: 4, use-tabs: nil
;; Output: "    line1\n    line2"
;;
;; Examples (dedent):
;; Input:  "    line1\n    line2", count: 4
;; Output: "line1\nline2"
;;
;; We test the NON-INTERACTIVE implementations to avoid mocking user input.

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

(defun test-indent (text count use-tabs)
  "Test cj/--indent-lines on TEXT with COUNT and USE-TABS.
Returns the transformed string."
  (cj/--indent-lines text count use-tabs))

(defun test-dedent (text count)
  "Test cj/--dedent-lines on TEXT with COUNT.
Returns the transformed string."
  (cj/--dedent-lines text count))

;;; Indent Tests - Normal Cases with Spaces

(ert-deftest test-indent-single-line-4-spaces ()
  "Should indent single line with 4 spaces."
  (let ((result (test-indent "line" 4 nil)))
    (should (string= result "    line"))))

(ert-deftest test-indent-two-lines-4-spaces ()
  "Should indent two lines with 4 spaces."
  (let ((result (test-indent "line1\nline2" 4 nil)))
    (should (string= result "    line1\n    line2"))))

(ert-deftest test-indent-three-lines-2-spaces ()
  "Should indent three lines with 2 spaces."
  (let ((result (test-indent "a\nb\nc" 2 nil)))
    (should (string= result "  a\n  b\n  c"))))

(ert-deftest test-indent-many-lines ()
  "Should indent many lines."
  (let ((result (test-indent "1\n2\n3\n4\n5" 4 nil)))
    (should (string= result "    1\n    2\n    3\n    4\n    5"))))

;;; Indent Tests - Normal Cases with Tabs

(ert-deftest test-indent-single-line-1-tab ()
  "Should indent single line with 1 tab."
  (let ((result (test-indent "line" 1 t)))
    (should (string= result "\tline"))))

(ert-deftest test-indent-two-lines-1-tab ()
  "Should indent two lines with 1 tab."
  (let ((result (test-indent "line1\nline2" 1 t)))
    (should (string= result "\tline1\n\tline2"))))

(ert-deftest test-indent-with-2-tabs ()
  "Should indent with 2 tabs."
  (let ((result (test-indent "code" 2 t)))
    (should (string= result "\t\tcode"))))

;;; Indent Tests - Boundary Cases

(ert-deftest test-indent-empty-string ()
  "Should indent empty string."
  (let ((result (test-indent "" 4 nil)))
    (should (string= result "    "))))

(ert-deftest test-indent-zero-count ()
  "Should not indent with count 0."
  (let ((result (test-indent "line" 0 nil)))
    (should (string= result "line"))))

(ert-deftest test-indent-already-indented ()
  "Should add more indentation to already indented lines."
  (let ((result (test-indent "  line1\n  line2" 2 nil)))
    (should (string= result "    line1\n    line2"))))

(ert-deftest test-indent-empty-lines ()
  "Should indent empty lines."
  (let ((result (test-indent "line1\n\nline3" 4 nil)))
    (should (string= result "    line1\n    \n    line3"))))

(ert-deftest test-indent-trailing-newline ()
  "Should preserve trailing newline."
  (let ((result (test-indent "line1\nline2\n" 4 nil)))
    (should (string= result "    line1\n    line2\n"))))

(ert-deftest test-indent-no-trailing-newline ()
  "Should work without trailing newline."
  (let ((result (test-indent "line1\nline2" 4 nil)))
    (should (string= result "    line1\n    line2"))))

;;; Dedent Tests - Normal Cases

(ert-deftest test-dedent-single-line-4-spaces ()
  "Should dedent single line with 4 spaces."
  (let ((result (test-dedent "    line" 4)))
    (should (string= result "line"))))

(ert-deftest test-dedent-two-lines-4-spaces ()
  "Should dedent two lines with 4 spaces."
  (let ((result (test-dedent "    line1\n    line2" 4)))
    (should (string= result "line1\nline2"))))

(ert-deftest test-dedent-three-lines-2-spaces ()
  "Should dedent three lines with 2 spaces."
  (let ((result (test-dedent "  a\n  b\n  c" 2)))
    (should (string= result "a\nb\nc"))))

(ert-deftest test-dedent-with-tabs ()
  "Should dedent lines with tabs."
  (let ((result (test-dedent "\tline1\n\tline2" 1)))
    (should (string= result "line1\nline2"))))

(ert-deftest test-dedent-mixed-spaces-tabs ()
  "Should dedent mixed spaces and tabs."
  (let ((result (test-dedent "  \tline" 3)))
    (should (string= result "line"))))

;;; Dedent Tests - Partial Dedent

(ert-deftest test-dedent-partial ()
  "Should dedent only COUNT characters."
  (let ((result (test-dedent "      line" 2)))
    (should (string= result "    line"))))

(ert-deftest test-dedent-less-than-count ()
  "Should dedent all available spaces when less than COUNT."
  (let ((result (test-dedent "  line" 4)))
    (should (string= result "line"))))

(ert-deftest test-dedent-no-leading-space ()
  "Should not affect lines with no leading whitespace."
  (let ((result (test-dedent "line" 4)))
    (should (string= result "line"))))

(ert-deftest test-dedent-varying-indentation ()
  "Should dedent each line independently."
  (let ((result (test-dedent "    line1\n  line2\nline3" 2)))
    (should (string= result "  line1\nline2\nline3"))))

;;; Dedent Tests - Boundary Cases

(ert-deftest test-dedent-empty-string ()
  "Should handle empty string."
  (let ((result (test-dedent "" 4)))
    (should (string= result ""))))

(ert-deftest test-dedent-zero-count ()
  "Should not dedent with count 0."
  (let ((result (test-dedent "    line" 0)))
    (should (string= result "    line"))))

(ert-deftest test-dedent-empty-lines ()
  "Should handle empty lines."
  (let ((result (test-dedent "    line1\n    \n    line3" 4)))
    (should (string= result "line1\n\nline3"))))

(ert-deftest test-dedent-only-whitespace ()
  "Should dedent whitespace-only lines."
  (let ((result (test-dedent "    " 4)))
    (should (string= result ""))))

(ert-deftest test-dedent-trailing-newline ()
  "Should preserve trailing newline."
  (let ((result (test-dedent "    line1\n    line2\n" 4)))
    (should (string= result "line1\nline2\n"))))

(ert-deftest test-dedent-preserves-internal-spaces ()
  "Should not affect internal whitespace."
  (let ((result (test-dedent "    hello  world" 4)))
    (should (string= result "hello  world"))))

;;; Round-trip Tests

(ert-deftest test-indent-dedent-roundtrip ()
  "Should be able to indent then dedent back to original."
  (let* ((original "line1\nline2")
         (indented (test-indent original 4 nil))
         (dedented (test-dedent indented 4)))
    (should (string= dedented original))))

(ert-deftest test-dedent-indent-roundtrip ()
  "Should be able to dedent then indent back to original."
  (let* ((original "    line1\n    line2")
         (dedented (test-dedent original 4))
         (indented (test-indent dedented 4 nil)))
    (should (string= indented original))))

;;; Edge Cases

(ert-deftest test-indent-very-long-line ()
  "Should indent very long line."
  (let* ((long-line (make-string 1000 ?a))
         (result (test-indent long-line 4 nil)))
    (should (string-prefix-p "    " result))
    (should (= (length result) 1004))))

(ert-deftest test-dedent-very-indented ()
  "Should dedent very indented line."
  (let* ((many-spaces (make-string 100 ?\s))
         (text (concat many-spaces "text"))
         (result (test-dedent text 50)))
    (should (string-prefix-p (make-string 50 ?\s) result))))

(ert-deftest test-indent-with-existing-tabs ()
  "Should indent lines that already have tabs."
  (let ((result (test-indent "\tcode" 4 nil)))
    (should (string= result "    \tcode"))))

(ert-deftest test-dedent-stops-at-non-whitespace ()
  "Should stop dedenting at first non-whitespace character."
  (let ((result (test-dedent "  a  b" 4)))
    (should (string= result "a  b"))))

(provide 'test-custom-text-enclose-indent)
;;; test-custom-text-enclose-indent.el ends here
