;;; test-custom-buffer-file-copy-to-bottom-of-buffer.el --- Tests for cj/copy-to-bottom-of-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/copy-to-bottom-of-buffer function from custom-buffer-file.el
;;
;; This function copies all text from point to the end of the current buffer
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

(defun test-copy-to-bottom-setup ()
  "Set up test environment."
  (setq kill-ring nil))

(defun test-copy-to-bottom-teardown ()
  "Clean up test environment."
  (setq kill-ring nil))

;;; Normal Cases

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-normal-point-in-middle-copies-to-end ()
  "Should copy from point to end when point in middle."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (forward-line 1)  ; Point at start of "Line 2"
        (let ((original-content (buffer-string)))
          (cj/copy-to-bottom-of-buffer)
          ;; Buffer should be unchanged
          (should (equal (buffer-string) original-content))
          ;; Kill ring should contain from point to end
          (should (equal (car kill-ring) "Line 2\nLine 3"))))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-normal-single-line-copies-partial ()
  "Should copy partial line content from middle of line."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello World")
        (goto-char (point-min))
        (forward-char 6)  ; Point after "Hello "
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) "Hello World"))
        (should (equal (car kill-ring) "World")))
    (test-copy-to-bottom-teardown)))

;;; Boundary Cases

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-point-at-beginning-copies-all ()
  "Should copy entire buffer when point at beginning."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) "Line 1\nLine 2\nLine 3"))
        (should (equal (car kill-ring) "Line 1\nLine 2\nLine 3")))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-point-at-end-copies-empty ()
  "Should copy empty string when point at end."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-max))
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) "Line 1\nLine 2\nLine 3"))
        (should (equal (car kill-ring) "")))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-empty-buffer-copies-empty ()
  "Should copy empty string in empty buffer."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) ""))
        (should (equal (car kill-ring) "")))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-point-second-to-last-char-copies-one ()
  "Should copy last character when point at second-to-last."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello")
        (goto-char (1- (point-max)))  ; Before 'o'
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) "Hello"))
        (should (equal (car kill-ring) "o")))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-unicode-content-copies-correctly ()
  "Should handle unicode content correctly."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello 👋\nمرحبا\nWorld")
        (goto-char (point-min))
        (forward-line 1)
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) "Hello 👋\nمرحبا\nWorld"))
        (should (equal (car kill-ring) "مرحبا\nWorld")))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-narrowed-buffer-respects-narrowing ()
  "Should respect narrowing and only copy within narrowed region."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3\nLine 4")
        (goto-char (point-min))
        (forward-line 1)
        (let ((start (point)))
          (forward-line 2)
          (narrow-to-region start (point))
          (goto-char (point-min))
          (forward-line 1)  ; Point at "Line 3"
          (cj/copy-to-bottom-of-buffer)
          (should (equal (buffer-string) "Line 2\nLine 3\n"))
          (should (equal (car kill-ring) "Line 3\n"))))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-whitespace-only-copies-whitespace ()
  "Should copy whitespace-only content."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \n\t\t\n  ")
        (goto-char (point-min))
        (forward-char 4)  ; After first newline
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) "   \n\t\t\n  "))
        (should (equal (car kill-ring) "\t\t\n  ")))
    (test-copy-to-bottom-teardown)))

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-boundary-single-character-copies-char ()
  "Should copy single character buffer."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "x")
        (goto-char (point-min))
        (cj/copy-to-bottom-of-buffer)
        (should (equal (buffer-string) "x"))
        (should (equal (car kill-ring) "x")))
    (test-copy-to-bottom-teardown)))

;;; Error Cases

(ert-deftest test-custom-buffer-file-copy-to-bottom-of-buffer-error-read-only-buffer-succeeds ()
  "Should work in read-only buffer since it doesn't modify content."
  (test-copy-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Read-only content")
        (read-only-mode 1)
        (goto-char (point-min))
        (cj/copy-to-bottom-of-buffer)
        (should (equal (car kill-ring) "Read-only content")))
    (test-copy-to-bottom-teardown)))

(provide 'test-custom-buffer-file-copy-to-bottom-of-buffer)
;;; test-custom-buffer-file-copy-to-bottom-of-buffer.el ends here
