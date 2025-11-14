;;; test-custom-ordering-reverse-lines.el --- Tests for cj/--reverse-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--reverse-lines function from custom-ordering.el
;;
;; This function reverses the order of lines in a region.
;; The first line becomes last, last becomes first, etc.
;;
;; Examples:
;; Input:  "line1\nline2\nline3"
;; Output: "line3\nline2\nline1"
;;
;; We test the NON-INTERACTIVE implementation (cj/--reverse-lines) to avoid
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
(require 'custom-ordering)

;;; Test Helpers

(defun test-reverse-lines (input-text)
  "Test cj/--reverse-lines on INPUT-TEXT.
Returns the transformed string."
  (with-temp-buffer
    (insert input-text)
    (cj/--reverse-lines (point-min) (point-max))))

;;; Normal Cases

(ert-deftest test-reverse-lines-three-lines ()
  "Should reverse three lines."
  (let ((result (test-reverse-lines "line1\nline2\nline3")))
    (should (string= result "line3\nline2\nline1"))))

(ert-deftest test-reverse-lines-two-lines ()
  "Should reverse two lines."
  (let ((result (test-reverse-lines "first\nsecond")))
    (should (string= result "second\nfirst"))))

(ert-deftest test-reverse-lines-many-lines ()
  "Should reverse many lines."
  (let ((result (test-reverse-lines "a\nb\nc\nd\ne")))
    (should (string= result "e\nd\nc\nb\na"))))

(ert-deftest test-reverse-lines-with-content ()
  "Should reverse lines with actual content."
  (let ((result (test-reverse-lines "apple banana\ncherry date\negg fig")))
    (should (string= result "egg fig\ncherry date\napple banana"))))

(ert-deftest test-reverse-lines-bidirectional ()
  "Should reverse back and forth correctly."
  (let* ((original "line1\nline2\nline3")
         (reversed (test-reverse-lines original))
         (back (test-reverse-lines reversed)))
    (should (string= reversed "line3\nline2\nline1"))
    (should (string= back original))))

;;; Boundary Cases

(ert-deftest test-reverse-lines-empty-string ()
  "Should handle empty string."
  (let ((result (test-reverse-lines "")))
    (should (string= result ""))))

(ert-deftest test-reverse-lines-single-line ()
  "Should handle single line (no change)."
  (let ((result (test-reverse-lines "single line")))
    (should (string= result "single line"))))

(ert-deftest test-reverse-lines-empty-lines ()
  "Should reverse including empty lines."
  (let ((result (test-reverse-lines "a\n\nb")))
    (should (string= result "b\n\na"))))

(ert-deftest test-reverse-lines-trailing-newline ()
  "Should handle trailing newline."
  (let ((result (test-reverse-lines "line1\nline2\n")))
    (should (string= result "\nline2\nline1"))))

(ert-deftest test-reverse-lines-only-newlines ()
  "Should reverse lines that are only newlines."
  (let ((result (test-reverse-lines "\n\n\n")))
    (should (string= result "\n\n\n"))))

(ert-deftest test-reverse-lines-numbers ()
  "Should reverse numbered lines."
  (let ((result (test-reverse-lines "1\n2\n3\n4\n5")))
    (should (string= result "5\n4\n3\n2\n1"))))

(ert-deftest test-reverse-lines-very-long ()
  "Should reverse very long list."
  (let* ((lines (mapcar #'number-to-string (number-sequence 1 100)))
         (input (mapconcat #'identity lines "\n"))
         (result (test-reverse-lines input))
         (result-lines (split-string result "\n")))
    (should (= 100 (length result-lines)))
    (should (string= "100" (car result-lines)))
    (should (string= "1" (car (last result-lines))))))

;;; Error Cases

(ert-deftest test-reverse-lines-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "line1\nline2")
     (cj/--reverse-lines (point-max) (point-min)))
   :type 'error))

(ert-deftest test-reverse-lines-empty-region ()
  "Should handle empty region (start == end)."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (should (string= "" (cj/--reverse-lines pos pos))))))

(provide 'test-custom-ordering-reverse-lines)
;;; test-custom-ordering-reverse-lines.el ends here
