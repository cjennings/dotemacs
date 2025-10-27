;;; test-custom-whitespace-delete-blank-lines.el --- Tests for cj/--delete-blank-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--delete-blank-lines function from custom-whitespace.el
;;
;; This function deletes blank lines from text, where blank lines are defined
;; as lines containing only whitespace (spaces, tabs) or nothing at all.
;; Uses the regexp ^[[:space:]]*$ to match blank lines.
;;
;; We test the NON-INTERACTIVE implementation (cj/--delete-blank-lines)
;; to avoid mocking user prompts. This follows our testing best practice
;; of separating business logic from UI interaction.

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

(defun test-delete-blank-lines (input-text)
  "Test cj/--delete-blank-lines on INPUT-TEXT.
Returns the buffer string after operation."
  (with-temp-buffer
    (insert input-text)
    (cj/--delete-blank-lines (point-min) (point-max))
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-delete-blank-lines-single-blank ()
  "Should delete single blank line between text."
  (let ((result (test-delete-blank-lines "line1\n\nline2")))
    (should (string= result "line1\nline2"))))

(ert-deftest test-delete-blank-lines-multiple-consecutive ()
  "Should delete multiple consecutive blank lines."
  (let ((result (test-delete-blank-lines "line1\n\n\n\nline2")))
    (should (string= result "line1\nline2"))))

(ert-deftest test-delete-blank-lines-spaces-only ()
  "Should delete lines with spaces only."
  (let ((result (test-delete-blank-lines "line1\n   \nline2")))
    (should (string= result "line1\nline2"))))

(ert-deftest test-delete-blank-lines-tabs-only ()
  "Should delete lines with tabs only."
  (let ((result (test-delete-blank-lines "line1\n\t\t\nline2")))
    (should (string= result "line1\nline2"))))

(ert-deftest test-delete-blank-lines-mixed-whitespace ()
  "Should delete lines with mixed whitespace."
  (let ((result (test-delete-blank-lines "line1\n \t \t \nline2")))
    (should (string= result "line1\nline2"))))

(ert-deftest test-delete-blank-lines-no-blank-lines ()
  "Should handle text with no blank lines (no-op)."
  (let ((result (test-delete-blank-lines "line1\nline2\nline3")))
    (should (string= result "line1\nline2\nline3"))))

(ert-deftest test-delete-blank-lines-at-start ()
  "Should delete blank lines at start of region."
  (let ((result (test-delete-blank-lines "\n\nline1\nline2")))
    (should (string= result "line1\nline2"))))

(ert-deftest test-delete-blank-lines-at-end ()
  "Should delete blank lines at end of region."
  (let ((result (test-delete-blank-lines "line1\nline2\n\n")))
    (should (string= result "line1\nline2\n"))))

(ert-deftest test-delete-blank-lines-scattered ()
  "Should delete blank lines scattered throughout text."
  (let ((result (test-delete-blank-lines "line1\n\nline2\n   \nline3\n\t\nline4")))
    (should (string= result "line1\nline2\nline3\nline4"))))

;;; Boundary Cases

(ert-deftest test-delete-blank-lines-empty-string ()
  "Should handle empty string."
  (let ((result (test-delete-blank-lines "")))
    (should (string= result ""))))

(ert-deftest test-delete-blank-lines-only-blank-lines ()
  "Should delete all lines if only blank lines exist."
  (let ((result (test-delete-blank-lines "\n\n\n")))
    (should (string= result ""))))

(ert-deftest test-delete-blank-lines-only-whitespace ()
  "Should delete lines containing only whitespace."
  (let ((result (test-delete-blank-lines "   \n\t\t\n \t ")))
    (should (string= result ""))))

(ert-deftest test-delete-blank-lines-single-line-content ()
  "Should handle single line with content (no-op)."
  (let ((result (test-delete-blank-lines "hello world")))
    (should (string= result "hello world"))))

(ert-deftest test-delete-blank-lines-single-blank-line ()
  "Should delete single blank line."
  (let ((result (test-delete-blank-lines "\n")))
    (should (string= result ""))))

(ert-deftest test-delete-blank-lines-very-long-region ()
  "Should handle very long region with many blank lines."
  (let* ((lines (make-list 100 "content"))
         (input (mapconcat #'identity lines "\n\n"))
         (expected (mapconcat #'identity lines "\n"))
         (result (test-delete-blank-lines input)))
    (should (string= result expected))))

(ert-deftest test-delete-blank-lines-preserve-content-lines ()
  "Should preserve lines with any non-whitespace content."
  (let ((result (test-delete-blank-lines "x\n\ny\n   \nz")))
    (should (string= result "x\ny\nz"))))

;;; Error Cases

(ert-deftest test-delete-blank-lines-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "line1\n\nline2")
     (cj/--delete-blank-lines (point-max) (point-min)))
   :type 'error))

(ert-deftest test-delete-blank-lines-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "line1\n\nline2")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--delete-blank-lines pos pos)
      ;; Should complete without error
      (should (string-match-p "line1" (buffer-string))))))

(provide 'test-custom-whitespace-delete-blank-lines)
;;; test-custom-whitespace-delete-blank-lines.el ends here
