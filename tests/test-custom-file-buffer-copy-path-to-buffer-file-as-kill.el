;;; test-custom-file-buffer-copy-path-to-buffer-file-as-kill.el --- Tests for cj/copy-path-to-buffer-file-as-kill -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/copy-path-to-buffer-file-as-kill function from custom-file-buffer.el
;;
;; This function copies the full path of the current buffer's file to the kill ring
;; and returns the path. It signals an error if the buffer is not visiting a file.

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

(defun test-copy-path-setup ()
  "Set up test environment."
  (setq kill-ring nil))

(defun test-copy-path-teardown ()
  "Clean up test environment."
  ;; Kill all buffers visiting files in the test directory
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (when (string-prefix-p cj/test-base-dir (buffer-file-name buf))
        (with-current-buffer buf
          (set-buffer-modified-p nil)
          (kill-buffer buf)))))
  (cj/delete-test-base-dir)
  (setq kill-ring nil))

;;; Normal Cases

(ert-deftest test-copy-path-simple-file ()
  "Should copy absolute path for simple file buffer."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (let ((result (cj/copy-path-to-buffer-file-as-kill)))
            (should (equal result test-file))
            (should (equal (car kill-ring) test-file)))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-returns-path ()
  "Should return the path value."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (let ((result (cj/copy-path-to-buffer-file-as-kill)))
            (should (stringp result))
            (should (equal result test-file)))))
    (test-copy-path-teardown)))

;;; Boundary Cases

(ert-deftest test-copy-path-unicode-filename ()
  "Should handle unicode in filename."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "café.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) test-file))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-spaces-in-filename ()
  "Should handle spaces in filename."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "my file.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) test-file))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-special-chars-filename ()
  "Should handle special characters in filename."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "[test]-(1).txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) test-file))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-very-long-path ()
  "Should handle very long path."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (long-name (make-string 200 ?x))
             (test-file (expand-file-name (concat long-name ".txt") test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) test-file))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-hidden-file ()
  "Should handle hidden file."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name ".hidden" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) test-file))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-no-extension ()
  "Should handle file with no extension."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "README" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) test-file))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-symlink-file ()
  "Should use buffer's filename for symlink."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (target-file (expand-file-name "target.txt" test-dir))
             (link-file (expand-file-name "link.txt" test-dir)))
        (with-temp-file target-file
          (insert "content"))
        (make-symbolic-link target-file link-file)
        (with-current-buffer (find-file link-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) (buffer-file-name)))))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-kill-ring-has-content ()
  "Should add to kill ring when it already has content."
  (test-copy-path-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (kill-new "existing content")
        (with-current-buffer (find-file test-file)
          (cj/copy-path-to-buffer-file-as-kill)
          (should (equal (car kill-ring) test-file))
          (should (equal (cadr kill-ring) "existing content"))))
    (test-copy-path-teardown)))

;;; Error Cases

(ert-deftest test-copy-path-non-file-buffer ()
  "Should signal user-error for non-file buffer."
  (test-copy-path-setup)
  (unwind-protect
      (with-temp-buffer
        (should-error (cj/copy-path-to-buffer-file-as-kill) :type 'user-error))
    (test-copy-path-teardown)))

(ert-deftest test-copy-path-scratch-buffer ()
  "Should signal user-error for *scratch* buffer."
  (test-copy-path-setup)
  (unwind-protect
      (with-current-buffer "*scratch*"
        (should-error (cj/copy-path-to-buffer-file-as-kill) :type 'user-error))
    (test-copy-path-teardown)))

(provide 'test-custom-file-buffer-copy-path-to-buffer-file-as-kill)
;;; test-custom-file-buffer-copy-path-to-buffer-file-as-kill.el ends here
