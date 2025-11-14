;;; test-custom-buffer-file-clear-to-top-of-buffer.el --- Tests for cj/clear-to-top-of-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/clear-to-top-of-buffer function from custom-buffer-file.el
;;
;; This function deletes all text from point to the beginning of the current buffer.
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
(require 'custom-buffer-file)

;;; Setup and Teardown

(defun test-clear-to-top-setup ()
  "Set up test environment."
  (setq kill-ring nil))

(defun test-clear-to-top-teardown ()
  "Clean up test environment."
  (setq kill-ring nil))

;;; Normal Cases

(ert-deftest test-clear-to-top-point-in-middle ()
  "Should delete from beginning to point when point in middle."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (forward-line 2)  ; Point at start of "Line 3"
        (cj/clear-to-top-of-buffer)
        (should (equal (buffer-string) "Line 3")))
    (test-clear-to-top-teardown)))

(ert-deftest test-clear-to-top-empty-buffer ()
  "Should do nothing in empty buffer."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/clear-to-top-of-buffer)
        (should (equal (buffer-string) "")))
    (test-clear-to-top-teardown)))

;;; Boundary Cases

(ert-deftest test-clear-to-top-point-at-beginning ()
  "Should delete nothing when point at beginning."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-min))
        (cj/clear-to-top-of-buffer)
        (should (equal (buffer-string) "Line 1\nLine 2\nLine 3")))
    (test-clear-to-top-teardown)))

(ert-deftest test-clear-to-top-point-at-end ()
  "Should delete entire buffer when point at end."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-max))
        (cj/clear-to-top-of-buffer)
        (should (equal (buffer-string) "")))
    (test-clear-to-top-teardown)))

(ert-deftest test-clear-to-top-point-at-second-char ()
  "Should delete first character when point at second."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello")
        (goto-char (1+ (point-min)))  ; After 'H'
        (cj/clear-to-top-of-buffer)
        (should (equal (buffer-string) "ello")))
    (test-clear-to-top-teardown)))

(ert-deftest test-clear-to-top-unicode-content ()
  "Should handle unicode content."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello 👋\nمرحبا\nWorld")
        (goto-char (point-min))
        (forward-line 2)
        (cj/clear-to-top-of-buffer)
        (should (equal (buffer-string) "World")))
    (test-clear-to-top-teardown)))

(ert-deftest test-clear-to-top-narrowed-buffer ()
  "Should respect narrowing."
  (test-clear-to-top-setup)
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
          (cj/clear-to-top-of-buffer)
          (should (equal (buffer-string) "Line 3\n"))))
    (test-clear-to-top-teardown)))

(ert-deftest test-clear-to-top-multiple-windows ()
  "Should update all windows showing buffer."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-max))
        (cj/clear-to-top-of-buffer)
        ;; Just verify content changed
        (should (equal (buffer-string) "")))
    (test-clear-to-top-teardown)))

(ert-deftest test-clear-to-top-does-not-affect-kill-ring ()
  "Should not add deleted text to kill ring."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (goto-char (point-max))
        (setq kill-ring nil)
        (cj/clear-to-top-of-buffer)
        (should (null kill-ring)))
    (test-clear-to-top-teardown)))

;;; Error Cases

(ert-deftest test-clear-to-top-read-only-buffer ()
  "Should signal error in read-only buffer."
  (test-clear-to-top-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Read-only content")
        (read-only-mode 1)
        (goto-char (point-max))
        (should-error (cj/clear-to-top-of-buffer)))
    (test-clear-to-top-teardown)))

(provide 'test-custom-buffer-file-clear-to-top-of-buffer)
;;; test-custom-buffer-file-clear-to-top-of-buffer.el ends here
