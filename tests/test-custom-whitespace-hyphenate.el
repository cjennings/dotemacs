;;; test-custom-whitespace-hyphenate.el --- Tests for cj/--hyphenate-whitespace -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--hyphenate-whitespace function from custom-whitespace.el
;;
;; This function replaces all runs of whitespace (spaces, tabs, newlines,
;; carriage returns) with single hyphens. Useful for converting text with
;; whitespace into hyphenated identifiers or URLs.
;;
;; Uses the regexp [ \t\n\r]+ to match whitespace runs.
;;
;; We test the NON-INTERACTIVE implementation (cj/--hyphenate-whitespace)
;; to avoid mocking region selection. This follows our testing best practice
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

(defun test-hyphenate-whitespace (input-text)
  "Test cj/--hyphenate-whitespace on INPUT-TEXT.
Returns the buffer string after operation."
  (with-temp-buffer
    (insert input-text)
    (cj/--hyphenate-whitespace (point-min) (point-max))
    (buffer-string)))

;;; Normal Cases

(ert-deftest test-hyphenate-whitespace-single-space ()
  "Should replace single space with hyphen."
  (let ((result (test-hyphenate-whitespace "hello world")))
    (should (string= result "hello-world"))))

(ert-deftest test-hyphenate-whitespace-multiple-spaces ()
  "Should replace multiple spaces with single hyphen."
  (let ((result (test-hyphenate-whitespace "hello    world")))
    (should (string= result "hello-world"))))

(ert-deftest test-hyphenate-whitespace-tabs ()
  "Should replace tabs with hyphen."
  (let ((result (test-hyphenate-whitespace "hello\tworld")))
    (should (string= result "hello-world"))))

(ert-deftest test-hyphenate-whitespace-mixed-tabs-spaces ()
  "Should replace mixed tabs and spaces with single hyphen."
  (let ((result (test-hyphenate-whitespace "hello \t world")))
    (should (string= result "hello-world"))))

(ert-deftest test-hyphenate-whitespace-newlines ()
  "Should replace newlines with hyphen (joining lines)."
  (let ((result (test-hyphenate-whitespace "hello\nworld")))
    (should (string= result "hello-world"))))

(ert-deftest test-hyphenate-whitespace-multiple-newlines ()
  "Should replace multiple newlines with single hyphen."
  (let ((result (test-hyphenate-whitespace "hello\n\n\nworld")))
    (should (string= result "hello-world"))))

(ert-deftest test-hyphenate-whitespace-multiple-words ()
  "Should hyphenate multiple words with various whitespace."
  (let ((result (test-hyphenate-whitespace "one two  three\tfour\nfive")))
    (should (string= result "one-two-three-four-five"))))

(ert-deftest test-hyphenate-whitespace-carriage-returns ()
  "Should handle carriage returns."
  (let ((result (test-hyphenate-whitespace "hello\r\nworld")))
    (should (string= result "hello-world"))))

;;; Boundary Cases

(ert-deftest test-hyphenate-whitespace-empty-string ()
  "Should handle empty string."
  (let ((result (test-hyphenate-whitespace "")))
    (should (string= result ""))))

(ert-deftest test-hyphenate-whitespace-no-whitespace ()
  "Should handle text with no whitespace (no-op)."
  (let ((result (test-hyphenate-whitespace "helloworld")))
    (should (string= result "helloworld"))))

(ert-deftest test-hyphenate-whitespace-only-whitespace ()
  "Should convert text with only whitespace to single hyphen."
  (let ((result (test-hyphenate-whitespace "   \t   \n   ")))
    (should (string= result "-"))))

(ert-deftest test-hyphenate-whitespace-single-char ()
  "Should handle single character with surrounding spaces."
  (let ((result (test-hyphenate-whitespace "  x  ")))
    (should (string= result "-x-"))))

(ert-deftest test-hyphenate-whitespace-very-long-text ()
  "Should handle very long text with many spaces."
  (let ((result (test-hyphenate-whitespace "word word word word word word word word")))
    (should (string= result "word-word-word-word-word-word-word-word"))))

(ert-deftest test-hyphenate-whitespace-leading-whitespace ()
  "Should replace leading whitespace with hyphen."
  (let ((result (test-hyphenate-whitespace "   hello world")))
    (should (string= result "-hello-world"))))

(ert-deftest test-hyphenate-whitespace-trailing-whitespace ()
  "Should replace trailing whitespace with hyphen."
  (let ((result (test-hyphenate-whitespace "hello world   ")))
    (should (string= result "hello-world-"))))

;;; Error Cases

(ert-deftest test-hyphenate-whitespace-start-greater-than-end ()
  "Should error when start > end."
  (should-error
   (with-temp-buffer
     (insert "hello world")
     (cj/--hyphenate-whitespace (point-max) (point-min)))
   :type 'error))

(ert-deftest test-hyphenate-whitespace-empty-region ()
  "Should handle empty region (start == end) without error."
  (with-temp-buffer
    (insert "hello world")
    (let ((pos (/ (+ (point-min) (point-max)) 2)))
      (cj/--hyphenate-whitespace pos pos)
      ;; Should complete without error and not change buffer
      (should (string= (buffer-string) "hello world")))))

(provide 'test-custom-whitespace-hyphenate)
;;; test-custom-whitespace-hyphenate.el ends here
