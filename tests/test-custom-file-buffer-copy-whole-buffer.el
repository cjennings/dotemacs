;;; test-custom-file-buffer-copy-whole-buffer.el --- Tests for cj/copy-whole-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/copy-whole-buffer function from custom-file-buffer.el
;;
;; This function copies the entire contents of the current buffer to the kill ring.
;; Point and mark are left exactly where they were. No transient region is created.

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

(defun test-copy-whole-buffer-setup ()
  "Set up test environment."
  (setq kill-ring nil))

(defun test-copy-whole-buffer-teardown ()
  "Clean up test environment."
  (setq kill-ring nil))

;;; Normal Cases

(ert-deftest test-copy-whole-buffer-simple-text ()
  "Should copy simple text content to kill ring."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "Hello, world!")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-preserves-point ()
  "Should preserve point position."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        (goto-char 7)  ; Position in middle
        (cj/copy-whole-buffer)
        (should (= (point) 7)))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-preserves-mark ()
  "Should preserve mark position."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello, world!")
        (push-mark 5)
        (goto-char 10)
        (cj/copy-whole-buffer)
        (should (= (mark) 5))
        (should (= (point) 10)))
    (test-copy-whole-buffer-teardown)))

;;; Boundary Cases

(ert-deftest test-copy-whole-buffer-empty ()
  "Should handle empty buffer."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-large ()
  "Should handle very large buffer."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (let ((large-content (make-string 100000 ?x)))
          (insert large-content)
          (cj/copy-whole-buffer)
          (should (equal (car kill-ring) large-content))))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-unicode ()
  "Should handle unicode content (emoji, RTL text)."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Hello 👋 مرحبا")
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "Hello 👋 مرحبا")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-binary ()
  "Should handle binary content."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert (string 0 1 2 255))
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) (string 0 1 2 255))))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-only-whitespace ()
  "Should handle buffer with only whitespace."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "   \t\n  ")
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "   \t\n  ")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-newlines-at-boundaries ()
  "Should handle newlines at start/end."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "\n\nHello\n\n")
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "\n\nHello\n\n")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-narrowed ()
  "Should copy only visible region in narrowed buffer."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3\n")
        (goto-char (point-min))
        (forward-line 1)
        (narrow-to-region (point) (progn (forward-line 1) (point)))
        (cj/copy-whole-buffer)
        ;; Should copy only the narrowed region
        (should (equal (car kill-ring) "Line 2\n")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-read-only ()
  "Should work in read-only buffer."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Read-only content")
        (read-only-mode 1)
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "Read-only content")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-kill-ring-has-content ()
  "Should add to kill ring when it already has content."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "New content")
        (kill-new "existing content")
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "New content"))
        (should (equal (cadr kill-ring) "existing content")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-multiline ()
  "Should preserve multiline content."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "Line 1\nLine 2\nLine 3")
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "Line 1\nLine 2\nLine 3")))
    (test-copy-whole-buffer-teardown)))

(ert-deftest test-copy-whole-buffer-no-properties ()
  "Should strip text properties."
  (test-copy-whole-buffer-setup)
  (unwind-protect
      (with-temp-buffer
        (insert (propertize "Hello" 'face 'bold))
        (cj/copy-whole-buffer)
        (should (equal (car kill-ring) "Hello"))
        (should (null (text-properties-at 0 (car kill-ring)))))
    (test-copy-whole-buffer-teardown)))

(provide 'test-custom-file-buffer-copy-whole-buffer)
;;; test-custom-file-buffer-copy-whole-buffer.el ends here
