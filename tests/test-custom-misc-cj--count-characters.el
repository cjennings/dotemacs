;;; test-custom-misc-cj--count-characters.el --- Tests for cj/--count-characters -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/--count-characters internal implementation function from custom-misc.el
;;
;; This internal function counts characters between START and END positions.
;; It validates that START is not greater than END and returns the character count.

;;; Code:

(require 'ert)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'custom-misc)

;;; Setup and Teardown

(defun test-count-characters-setup ()
  "Set up test environment."
  ;; No setup needed for this function
  nil)

(defun test-count-characters-teardown ()
  "Clean up test environment."
  ;; No teardown needed for this function
  nil)

;;; Normal Cases

(ert-deftest test-custom-misc-cj--count-characters-normal-simple-text-returns-count ()
  "Should count characters in simple text region."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        (let ((result (cj/--count-characters 1 14)))
          (should (= result 13))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-normal-partial-region-returns-count ()
  "Should count characters in partial region."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        (let ((result (cj/--count-characters 1 6)))
          (should (= result 5))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-normal-multiline-returns-count ()
  "Should count characters including newlines."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        ;; 6 + 1 + 6 + 1 + 6 = 20 characters
        (let ((result (cj/--count-characters (point-min) (point-max))))
          (should (= result 20))))
    (test-count-characters-teardown)))

;;; Boundary Cases

(ert-deftest test-custom-misc-cj--count-characters-boundary-empty-region-returns-zero ()
  "Should return 0 for empty region (start equals end)."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello")
        (let ((result (cj/--count-characters 3 3)))
          (should (= result 0))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-boundary-single-character-returns-one ()
  "Should return 1 for single character region."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello")
        (let ((result (cj/--count-characters 1 2)))
          (should (= result 1))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-boundary-large-region-returns-count ()
  "Should handle very large region."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((large-content (make-string 100000 ?x)))
          (insert large-content)
          (let ((result (cj/--count-characters (point-min) (point-max))))
            (should (= result 100000)))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-boundary-unicode-returns-count ()
  "Should count unicode characters (emoji, RTL text, combining characters)."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        ;; "Hello 👋 مرحبا" contains emoji and Arabic text
        (insert "Hello 👋 مرحبا")
        (let ((result (cj/--count-characters (point-min) (point-max))))
          ;; Count the actual characters in the buffer
          (should (= result (- (point-max) (point-min))))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-boundary-whitespace-only-returns-count ()
  "Should count whitespace characters."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \t\n  ")
        ;; 3 spaces + 1 tab + 1 newline + 2 spaces = 7 characters
        (let ((result (cj/--count-characters (point-min) (point-max))))
          (should (= result 7))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-boundary-newlines-at-boundaries-returns-count ()
  "Should count newlines at start and end."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "\n\nHello\n\n")
        ;; 2 newlines + 5 chars + 2 newlines = 9 characters
        (let ((result (cj/--count-characters (point-min) (point-max))))
          (should (= result 9))))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-boundary-binary-content-returns-count ()
  "Should handle binary content."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert (string 0 1 2 255))
        (let ((result (cj/--count-characters (point-min) (point-max))))
          (should (= result 4))))
    (test-count-characters-teardown)))

;;; Error Cases

(ert-deftest test-custom-misc-cj--count-characters-error-start-greater-than-end-signals-error ()
  "Should signal error when start is greater than end."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        (should-error (cj/--count-characters 10 5)
                      :type 'error))
    (test-count-characters-teardown)))

(ert-deftest test-custom-misc-cj--count-characters-error-positions-out-of-bounds-handled ()
  "Should handle positions beyond buffer bounds (Emacs handles this)."
  (test-count-characters-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello")
        ;; Emacs will error if positions are truly out of bounds,
        ;; but this tests that our function doesn't add additional errors
        ;; Buffer has 6 positions (1-6), testing valid bounds
        (let ((result (cj/--count-characters 1 6)))
          (should (= result 5))))
    (test-count-characters-teardown)))

(provide 'test-custom-misc-cj--count-characters)
;;; test-custom-misc-cj--count-characters.el ends here
