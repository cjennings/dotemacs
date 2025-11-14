;;; test-custom-buffer-file-copy-to-top-of-buffer.el --- Tests for cj/copy-to-top-of-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/copy-to-top-of-buffer function from custom-buffer-file.el
;;
;; This function copies all text from the beginning of the buffer to point
;; to the kill ring without modifying the buffer.

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Stub ps-print package
(provide 'ps-print)

;; Now load the actual production module
(require 'custom-buffer-file)

;;; Setup and Teardown

(defun test-copy-to-top-setup ()
  "Set up test environment."
  (setq kill-ring nil))

(defun test-copy-to-top-teardown ()
  "Clean up test environment."
  (setq kill-ring nil))

;;; Normal Cases

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-normal-point-in-middle-copies-from-beginning ()
  "Should copy from beginning to point when point in middle."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (forward-line 2)  ; Point at start of "Line 3"
        (let ((original-content (buffer-string)))
          (cj/copy-to-top-of-buffer)
          ;; Buffer should be unchanged
          (should (equal (buffer-string) original-content))
          ;; Kill ring should contain from beginning to point
          (should (equal (car kill-ring) "Line 1\nLine 2\n"))))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-normal-single-line-copies-partial ()
  "Should copy partial line content from beginning to middle of line."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello World")
        (goto-char (point-min))
        (forward-char 5)  ; Point after "Hello"
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) "Hello World"))
        (should (equal (car kill-ring) "Hello")))
    (test-copy-to-top-teardown)))

;;; Boundary Cases

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-point-at-end-copies-all ()
  "Should copy entire buffer when point at end."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-max))
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) "Line 1\nLine 2\nLine 3"))
        (should (equal (car kill-ring) "Line 1\nLine 2\nLine 3")))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-point-at-beginning-copies-empty ()
  "Should copy empty string when point at beginning."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) "Line 1\nLine 2\nLine 3"))
        (should (equal (car kill-ring) "")))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-empty-buffer-copies-empty ()
  "Should copy empty string in empty buffer."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) ""))
        (should (equal (car kill-ring) "")))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-point-at-second-char-copies-one ()
  "Should copy first character when point at second character."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello")
        (goto-char (1+ (point-min)))  ; After 'H'
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) "Hello"))
        (should (equal (car kill-ring) "H")))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-unicode-content-copies-correctly ()
  "Should handle unicode content correctly."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello 👋\nمرحبا\nWorld")
        (goto-char (point-min))
        (forward-line 2)  ; Point at start of "World"
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) "Hello 👋\nمرحبا\nWorld"))
        (should (equal (car kill-ring) "Hello 👋\nمرحبا\n")))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-narrowed-buffer-respects-narrowing ()
  "Should respect narrowing and only copy within narrowed region."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3\nLine 4")
        (goto-char (point-min))
        (forward-line 1)
        (let ((start (point)))
          (forward-line 2)
          (narrow-to-region start (point))
          (goto-char (point-max))  ; Point at end of narrowed region
          (cj/copy-to-top-of-buffer)
          (should (equal (buffer-string) "Line 2\nLine 3\n"))
          (should (equal (car kill-ring) "Line 2\nLine 3\n"))))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-whitespace-only-copies-whitespace ()
  "Should copy whitespace-only content."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \n\t\t\n  ")
        (goto-char (point-min))
        (forward-char 7)  ; After second newline
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) "   \n\t\t\n  "))
        (should (equal (car kill-ring) "   \n\t\t\n")))
    (test-copy-to-top-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-boundary-single-character-copies-char ()
  "Should copy single character buffer."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "x")
        (goto-char (point-max))
        (cj/copy-to-top-of-buffer)
        (should (equal (buffer-string) "x"))
        (should (equal (car kill-ring) "x")))
    (test-copy-to-top-teardown)))

;;; Error Cases

(ert-deftest test-custom-buffer-file-copy-to-top-of-buffer-error-read-only-buffer-succeeds ()
  "Should work in read-only buffer since it doesn't modify content."
  (test-copy-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Read-only content")
        (goto-char (point-max))
        (read-only-mode 1)
        (cj/copy-to-top-of-buffer)
        (should (equal (car kill-ring) "Read-only content")))
    (test-copy-to-top-teardown)))

(provide 'test-custom-buffer-file-copy-to-top-of-buffer)
;;; test-custom-buffer-file-copy-to-top-of-buffer.el ends here
