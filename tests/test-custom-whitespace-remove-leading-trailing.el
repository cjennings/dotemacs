;;; test-custom-whitespace-remove-leading-trailing.el --- Tests for cj/--remove-leading-trailing-whitespace -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--remove-leading-trailing-whitespace function from custom-whitespace.el
;;
;; This function removes leading and trailing whitespace (spaces and tabs) from text.
;; - Removes leading whitespace: ^[ \t]+
;; - Removes trailing whitespace: [ \t]+$
;; - Preserves interior whitespace
;; - Operates on any region defined by START and END
;;
;; We test the NON-INTERACTIVE implementation (cj/--remove-leading-trailing-whitespace)
;; to avoid mocking region selection and prefix arguments. This follows our testing
;; best practice of separating business logic from UI interaction.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-whitespace)

;;; Test Helpers

(defun test-remove-leading-trailing (input-text)
  "Test cj/--remove-leading-trailing-whitespace on INPUT-TEXT.
Returns the buffer string after operation."
  (with-temp-buffer
    (insert input-text)
    (cj/--remove-leading-trailing-whitespace (point-min) (point-max))
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-remove-leading-trailing-leading-spaces ()
  "Should remove leading spaces from single line."
  (let ((result (test-remove-leading-trailing "    hello world")))
    (should (string= result "hello world"))))

(ert-deftest test-remove-leading-trailing-trailing-spaces ()
  "Should remove trailing spaces from single line."
  (let ((result (test-remove-leading-trailing "hello world    ")))
    (should (string= result "hello world"))))

(ert-deftest test-remove-leading-trailing-both-spaces ()
  "Should remove both leading and trailing spaces."
  (let ((result (test-remove-leading-trailing "    hello world    ")))
    (should (string= result "hello world"))))

(ert-deftest test-remove-leading-trailing-leading-tabs ()
  "Should remove leading tabs from single line."
  (let ((result (test-remove-leading-trailing "\t\thello world")))
    (should (string= result "hello world"))))

(ert-deftest test-remove-leading-trailing-trailing-tabs ()
  "Should remove trailing tabs from single line."
  (let ((result (test-remove-leading-trailing "hello world\t\t")))
    (should (string= result "hello world"))))

(ert-deftest test-remove-leading-trailing-mixed-tabs-spaces ()
  "Should remove mixed tabs and spaces."
  (let ((result (test-remove-leading-trailing " \t hello world \t ")))
    (should (string= result "hello world"))))

(ert-deftest test-remove-leading-trailing-preserve-interior ()
  "Should preserve interior whitespace."
  (let ((result (test-remove-leading-trailing "  hello   world  \t")))
    (should (string= result "hello   world"))))

(ert-deftest test-remove-leading-trailing-multiple-lines ()
  "Should handle multiple lines with leading/trailing whitespace."
  (let ((result (test-remove-leading-trailing "  line1  \n\t\tline2\t\n   line3   ")))
    (should (string= result "line1\nline2\nline3"))))

(ert-deftest test-remove-leading-trailing-multiline-preserve-interior ()
  "Should preserve interior whitespace on multiple lines."
  (let ((result (test-remove-leading-trailing "  hello   world  \n  foo   bar  ")))
    (should (string= result "hello   world\nfoo   bar"))))

;;; Boundary Cases

(ert-deftest test-remove-leading-trailing-empty-string ()
  "Should handle empty string."
  (let ((result (test-remove-leading-trailing "")))
    (should (string= result ""))))

(ert-deftest test-remove-leading-trailing-single-char ()
  "Should handle single character with surrounding spaces."
  (let ((result (test-remove-leading-trailing "   x   ")))
    (should (string= result "x"))))

(ert-deftest test-remove-leading-trailing-only-whitespace ()
  "Should handle lines with only whitespace."
  (let ((result (test-remove-leading-trailing "   \t   ")))
    (should (string= result ""))))

(ert-deftest test-remove-leading-trailing-no-whitespace ()
  "Should handle text with no leading/trailing whitespace (no-op)."
  (let ((result (test-remove-leading-trailing "hello world")))
    (should (string= result "hello world"))))

(ert-deftest test-remove-leading-trailing-very-long-line ()
  "Should handle very long lines with whitespace."
  (let* ((long-text (make-string 500 ?x))
         (input (concat "  " long-text "  "))
         (result (test-remove-leading-trailing input)))
    (should (string= result long-text))))

(ert-deftest test-remove-leading-trailing-whitespace-between-lines ()
  "Should handle lines that become empty after removal."
  (let ((result (test-remove-leading-trailing "line1\n   \nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-remove-leading-trailing-newlines-only ()
  "Should preserve newlines while removing spaces."
  (let ((result (test-remove-leading-trailing "\n\n\n")))
    (should (string= result "\n\n\n"))))

(ert-deftest test-remove-leading-trailing-partial-region ()
  "Should work on partial buffer region."
  (with-temp-buffer
    (insert "  hello  \n  world  \n  test  ")
    ;; Only operate on middle line
    (let ((start (+ (point-min) 10))  ; Start of second line
          (end (+ (point-min) 19)))    ; End of second line
      (cj/--remove-leading-trailing-whitespace start end)
      (should (string= (buffer-string) "  hello  \nworld\n  test  ")))))

;;; Error Cases

(ert-deftest test-remove-leading-trailing-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--remove-leading-trailing-whitespace (point-max) (point-min)))
   :type 'error))

(ert-deftest test-remove-leading-trailing-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--remove-leading-trailing-whitespace pos pos)
      ;; Should complete without error and not change buffer
      (should (string= (buffer-string) "hello world")))))

(provide 'test-custom-whitespace-remove-leading-trailing)
;;; test-custom-whitespace-remove-leading-trailing.el ends here
