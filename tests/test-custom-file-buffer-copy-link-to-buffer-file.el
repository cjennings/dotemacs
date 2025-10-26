;;; test-custom-file-buffer-copy-link-to-buffer-file.el --- Tests for cj/copy-link-to-buffer-file -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/copy-link-to-buffer-file function from custom-file-buffer.el
;;
;; This function copies the full file:// path of the current buffer's file to
;; the kill ring. For non-file buffers, it does nothing (no error).

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

(defun test-copy-link-setup ()
  "Set up test environment."
  (setq kill-ring nil))

(defun test-copy-link-teardown ()
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

(ert-deftest test-copy-link-simple-file ()
  "Should copy file:// link for simple file buffer."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-non-file-buffer ()
  "Should do nothing for non-file buffer without error."
  (test-copy-link-setup)
  (unwind-protect
      (with-temp-buffer
        (setq kill-ring nil)
        (cj/copy-link-to-buffer-file)
        (should (null kill-ring)))
    (test-copy-link-teardown)))

;;; Boundary Cases

(ert-deftest test-copy-link-unicode-filename ()
  "Should handle unicode in filename."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "café.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-spaces-in-filename ()
  "Should handle spaces in filename."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "my file.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-special-chars-filename ()
  "Should handle special characters in filename."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "[test]-(1).txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-very-long-path ()
  "Should handle very long path."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (long-name (make-string 200 ?x))
             (test-file (expand-file-name (concat long-name ".txt") test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-hidden-file ()
  "Should handle hidden file."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name ".hidden" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-no-extension ()
  "Should handle file with no extension."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "README" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-symlink-file ()
  "Should use buffer's filename for symlink."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (target-file (expand-file-name "target.txt" test-dir))
             (link-file (expand-file-name "link.txt" test-dir)))
        (with-temp-file target-file
          (insert "content"))
        (make-symbolic-link target-file link-file)
        (with-current-buffer (find-file link-file)
          (cj/copy-link-to-buffer-file)
          ;; Should use the link name (what buffer-file-name returns)
          (should (equal (car kill-ring) (concat "file://" (buffer-file-name))))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-kill-ring-has-content ()
  "Should add to kill ring when it already has content."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (kill-new "existing content")
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))
          (should (equal (cadr kill-ring) "existing content"))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-empty-kill-ring ()
  "Should populate empty kill ring."
  (test-copy-link-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (setq kill-ring nil)
        (with-current-buffer (find-file test-file)
          (cj/copy-link-to-buffer-file)
          (should (equal (car kill-ring) (concat "file://" test-file)))
          (should (= (length kill-ring) 1))))
    (test-copy-link-teardown)))

(ert-deftest test-copy-link-scratch-buffer ()
  "Should do nothing for *scratch* buffer."
  (test-copy-link-setup)
  (unwind-protect
      (progn
        (setq kill-ring nil)
        (with-current-buffer "*scratch*"
          (cj/copy-link-to-buffer-file)
          (should (null kill-ring))))
    (test-copy-link-teardown)))

(provide 'test-custom-file-buffer-copy-link-to-buffer-file)
;;; test-custom-file-buffer-copy-link-to-buffer-file.el ends here
