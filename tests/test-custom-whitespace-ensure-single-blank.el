;;; test-custom-whitespace-ensure-single-blank.el --- Tests for cj/--ensure-single-blank-line -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--ensure-single-blank-line function from custom-whitespace.el
;;
;; This function collapses multiple consecutive blank lines to exactly one blank line.
;; Different from delete-blank-lines which removes ALL blank lines, this function
;; preserves blank lines but ensures no more than one blank line appears consecutively.
;;
;; A blank line is defined as a line containing only whitespace (spaces, tabs) or nothing.
;; Uses the regexp (^[[:space:]]*$\n){2,} to match 2+ consecutive blank lines.
;;
;; We test the NON-INTERACTIVE implementation (cj/--ensure-single-blank-line)
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

(defun test-ensure-single-blank-line (input-text)
  "Test cj/--ensure-single-blank-line on INPUT-TEXT.
Returns the buffer string after operation."
  (with-temp-buffer
    (insert input-text)
    (cj/--ensure-single-blank-line (point-min) (point-max))
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-ensure-single-blank-two-blanks ()
  "Should collapse two blank lines to one."
  (let ((result (test-ensure-single-blank-line "line1\n\n\nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-ensure-single-blank-three-blanks ()
  "Should collapse three blank lines to one."
  (let ((result (test-ensure-single-blank-line "line1\n\n\n\nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-ensure-single-blank-many-blanks ()
  "Should collapse many blank lines to one."
  (let ((result (test-ensure-single-blank-line "line1\n\n\n\n\n\n\nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-ensure-single-blank-preserve-single ()
  "Should preserve single blank lines (no-op)."
  (let ((result (test-ensure-single-blank-line "line1\n\nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-ensure-single-blank-multiple-groups ()
  "Should handle multiple groups of consecutive blanks."
  (let ((result (test-ensure-single-blank-line "line1\n\n\nline2\n\n\n\nline3")))
    (should (string= result "line1\n\nline2\n\nline3"))))

(ert-deftest test-ensure-single-blank-blanks-with-spaces ()
  "Should handle blank lines with spaces only."
  (let ((result (test-ensure-single-blank-line "line1\n   \n   \nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-ensure-single-blank-blanks-with-tabs ()
  "Should handle blank lines with tabs only."
  (let ((result (test-ensure-single-blank-line "line1\n\t\t\n\t\t\nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-ensure-single-blank-mixed-whitespace ()
  "Should handle blank lines with mixed whitespace."
  (let ((result (test-ensure-single-blank-line "line1\n \t \n  \t  \nline2")))
    (should (string= result "line1\n\nline2"))))

(ert-deftest test-ensure-single-blank-no-blanks ()
  "Should handle text with no blank lines (no-op)."
  (let ((result (test-ensure-single-blank-line "line1\nline2\nline3")))
    (should (string= result "line1\nline2\nline3"))))

;;; Boundary Cases

(ert-deftest test-ensure-single-blank-empty-string ()
  "Should handle empty string."
  (let ((result (test-ensure-single-blank-line "")))
    (should (string= result ""))))

(ert-deftest test-ensure-single-blank-only-blanks ()
  "Should collapse many blank lines to one blank line."
  (let ((result (test-ensure-single-blank-line "\n\n\n\n")))
    (should (string= result "\n\n"))))

(ert-deftest test-ensure-single-blank-at-start ()
  "Should collapse multiple blank lines at start to one."
  (let ((result (test-ensure-single-blank-line "\n\n\nline1")))
    (should (string= result "\n\nline1"))))

(ert-deftest test-ensure-single-blank-at-end ()
  "Should collapse multiple blank lines at end to one."
  (let ((result (test-ensure-single-blank-line "line1\n\n\n")))
    (should (string= result "line1\n\n"))))

(ert-deftest test-ensure-single-blank-single-line ()
  "Should handle single line (no-op)."
  (let ((result (test-ensure-single-blank-line "line1")))
    (should (string= result "line1"))))

(ert-deftest test-ensure-single-blank-complex-structure ()
  "Should handle complex mix of content and blanks."
  (let ((result (test-ensure-single-blank-line "line1\n\n\nline2\nline3\n\n\n\nline4")))
    (should (string= result "line1\n\nline2\nline3\n\nline4"))))

(ert-deftest test-ensure-single-blank-preserves-content ()
  "Should not modify lines with content."
  (let ((result (test-ensure-single-blank-line "  line1  \n\n\n  line2  ")))
    (should (string= result "  line1  \n\n  line2  "))))

;;; Error Cases

(ert-deftest test-ensure-single-blank-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "line1\n\n\nline2")
     (cj/--ensure-single-blank-line (point-max) (point-min)))
   :type 'error))

(ert-deftest test-ensure-single-blank-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "line1\n\n\nline2")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--ensure-single-blank-line pos pos)
      ;; Should complete without error
      (should (string-match-p "line1" (buffer-string))))))

(provide 'test-custom-whitespace-ensure-single-blank)
;;; test-custom-whitespace-ensure-single-blank.el ends here
