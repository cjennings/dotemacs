;;; test-gptel-tools-read-buffer.el --- Tests for read_buffer gptel tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/read-buffer--get-content', the testable helper that
;; backs the read_buffer gptel tool.

;;; Code:

(require 'ert)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "gptel-tools" user-emacs-directory))
  (setq load-prefer-newer t)
  (unless (featurep 'gptel)
    (defvar gptel-tools nil)
    (defun gptel-make-tool (&rest _args) nil)
    (defun gptel-get-tool (&rest _args) nil)
    (provide 'gptel)))

(require 'read_buffer)

(ert-deftest test-gptel-tools-read-buffer-normal ()
  "Normal: returns the contents of an existing buffer."
  (with-temp-buffer
    (rename-buffer "test-gptel-tools-read-buffer-normal" t)
    (insert "hello world")
    (should (equal (cj/read-buffer--get-content (buffer-name)) "hello world"))))

(ert-deftest test-gptel-tools-read-buffer-boundary-empty-buffer ()
  "Boundary: empty buffer returns the empty string."
  (with-temp-buffer
    (rename-buffer "test-gptel-tools-read-buffer-empty" t)
    (should (equal (cj/read-buffer--get-content (buffer-name)) ""))))

(ert-deftest test-gptel-tools-read-buffer-boundary-buffer-object ()
  "Boundary: accepts a buffer object as well as a name string."
  (with-temp-buffer
    (insert "from buffer object")
    (should (equal (cj/read-buffer--get-content (current-buffer))
                   "from buffer object"))))

(ert-deftest test-gptel-tools-read-buffer-boundary-widened-content ()
  "Boundary: returns the whole buffer even when the buffer is narrowed."
  (with-temp-buffer
    (insert "visible\nhidden\n")
    (narrow-to-region (point-min) (line-end-position))
    (should (equal (cj/read-buffer--get-content (current-buffer))
                   "visible\nhidden\n"))))

(ert-deftest test-gptel-tools-read-buffer-boundary-strips-text-properties ()
  "Boundary: the returned string has no text properties."
  (with-temp-buffer
    (rename-buffer "test-gptel-tools-read-buffer-props" t)
    (insert (propertize "fontified" 'face 'bold))
    (let ((content (cj/read-buffer--get-content (buffer-name))))
      (should (equal content "fontified"))
      (should-not (text-properties-at 0 content)))))

(ert-deftest test-gptel-tools-read-buffer-error-missing-buffer ()
  "Error: nonexistent buffer name signals."
  (when (get-buffer "test-gptel-tools-read-buffer-absent")
    (kill-buffer "test-gptel-tools-read-buffer-absent"))
  (should-error (cj/read-buffer--get-content
                 "test-gptel-tools-read-buffer-absent")))

(ert-deftest test-gptel-tools-read-buffer-error-killed-buffer-object ()
  "Error: a killed buffer object signals clearly."
  (let ((buffer (generate-new-buffer "test-gptel-tools-read-buffer-killed")))
    (kill-buffer buffer)
    (should-error (cj/read-buffer--get-content buffer))))

(provide 'test-gptel-tools-read-buffer)
;;; test-gptel-tools-read-buffer.el ends here
