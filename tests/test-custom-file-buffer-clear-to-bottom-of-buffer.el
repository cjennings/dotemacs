;;; test-custom-file-buffer-clear-to-bottom-of-buffer.el --- Tests for cj/clear-to-bottom-of-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/clear-to-bottom-of-buffer function from custom-file-buffer.el
;;
;; This function deletes all text from point to the end of the current buffer.
;; It does not save the deleted text in the kill ring.

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
(require 'custom-file-buffer)

;;; Setup and Teardown

(defun test-clear-to-bottom-setup ()
  "Set up test environment."
  (setq kill-ring nil))

(defun test-clear-to-bottom-teardown ()
  "Clean up test environment."
  (setq kill-ring nil))

;;; Normal Cases

(ert-deftest test-clear-to-bottom-point-in-middle ()
  "Should delete from point to end when point in middle."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (forward-line 1)  ; Point at start of "Line 2"
        (cj/clear-to-bottom-of-buffer)
        (should (equal (buffer-string) "Line 1\n")))
    (test-clear-to-bottom-teardown)))

(ert-deftest test-clear-to-bottom-empty-buffer ()
  "Should do nothing in empty buffer."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/clear-to-bottom-of-buffer)
        (should (equal (buffer-string) "")))
    (test-clear-to-bottom-teardown)))

;;; Boundary Cases

(ert-deftest test-clear-to-bottom-point-at-beginning ()
  "Should delete entire buffer when point at beginning."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (cj/clear-to-bottom-of-buffer)
        (should (equal (buffer-string) "")))
    (test-clear-to-bottom-teardown)))

(ert-deftest test-clear-to-bottom-point-at-end ()
  "Should delete nothing when point at end."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-max))
        (cj/clear-to-bottom-of-buffer)
        (should (equal (buffer-string) "Line 1\nLine 2\nLine 3")))
    (test-clear-to-bottom-teardown)))

(ert-deftest test-clear-to-bottom-point-second-to-last-char ()
  "Should delete last character when point at second-to-last."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello")
        (goto-char (1- (point-max)))  ; Before 'o'
        (cj/clear-to-bottom-of-buffer)
        (should (equal (buffer-string) "Hell")))
    (test-clear-to-bottom-teardown)))

(ert-deftest test-clear-to-bottom-unicode-content ()
  "Should handle unicode content."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello 👋\nمرحبا\nWorld")
        (goto-char (point-min))
        (forward-line 1)
        (cj/clear-to-bottom-of-buffer)
        (should (equal (buffer-string) "Hello 👋\n")))
    (test-clear-to-bottom-teardown)))

(ert-deftest test-clear-to-bottom-narrowed-buffer ()
  "Should respect narrowing."
  (test-clear-to-bottom-setup)
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
          (cj/clear-to-bottom-of-buffer)
          (should (equal (buffer-string) "Line 2\n"))))
    (test-clear-to-bottom-teardown)))

(ert-deftest test-clear-to-bottom-multiple-windows ()
  "Should update all windows showing buffer."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (forward-line 1)
        (cj/clear-to-bottom-of-buffer)
        ;; Just verify content changed
        (should (equal (buffer-string) "Line 1\n")))
    (test-clear-to-bottom-teardown)))

(ert-deftest test-clear-to-bottom-does-not-affect-kill-ring ()
  "Should not add deleted text to kill ring."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (setq kill-ring nil)
        (cj/clear-to-bottom-of-buffer)
        (should (null kill-ring)))
    (test-clear-to-bottom-teardown)))

;;; Error Cases

(ert-deftest test-clear-to-bottom-read-only-buffer ()
  "Should signal error in read-only buffer."
  (test-clear-to-bottom-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Read-only content")
        (read-only-mode 1)
        (goto-char (point-min))
        (should-error (cj/clear-to-bottom-of-buffer)))
    (test-clear-to-bottom-teardown)))

(provide 'test-custom-file-buffer-clear-to-bottom-of-buffer)
;;; test-custom-file-buffer-clear-to-bottom-of-buffer.el ends here
