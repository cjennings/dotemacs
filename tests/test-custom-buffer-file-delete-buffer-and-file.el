;;; test-custom-buffer-file-delete-buffer-and-file.el --- Tests for cj/delete-buffer-and-file -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/delete-buffer-and-file function from custom-buffer-file.el
;;
;; This function deletes both the current buffer and the file it visits.
;; It uses vc-delete-file for version-controlled files and delete-file
;; for non-version-controlled files.
;;
;; Testing Strategy:
;; - We test OUR code's behavior, not the underlying delete-file/vc-delete-file
;;   implementations
;; - We verify our code correctly:
;;   1. Detects VC vs non-VC files (via vc-backend)
;;   2. Calls the appropriate deletion function (vc-delete-file or delete-file)
;;   3. Passes the trash flag (t) to delete-file
;;   4. Propagates errors from the deletion functions
;;
;; Why We Mock delete-file Errors:
;; - Tests like "already deleted file" and "no delete permission" are testing
;;   system/environment behavior, not our code
;; - The trash system handles these cases in environment-specific ways:
;;   - Missing files may not error (trash handles gracefully)
;;   - File permissions may not matter (directory permissions for moving to trash)
;; - To make tests deterministic and portable, we mock delete-file to throw
;;   specific errors, then verify our code propagates them correctly
;; - This tests our contract: "when delete-file fails, we let the error through"

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

(defun test-delete-buffer-and-file-setup ()
  "Setup for delete-buffer-and-file tests."
  (cj/create-test-base-dir))

(defun test-delete-buffer-and-file-teardown ()
  "Teardown for delete-buffer-and-file tests."
  ;; Kill all buffers visiting files in test directory
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (when (string-prefix-p cj/test-base-dir (buffer-file-name buf))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf))))
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-delete-buffer-and-file-simple-delete ()
  "Should delete file and kill buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (let ((buf (current-buffer)))
          ;; Mock vc-backend to return nil (non-VC file)
          (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file)
            (should-not (file-exists-p test-file))
            (should-not (buffer-live-p buf)))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-removes-file-from-disk ()
  "Should remove file from disk."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-kills-buffer ()
  "Should kill the buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (let ((buf (current-buffer)))
          (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file)
            (should-not (buffer-live-p buf)))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-calls-delete-file-with-trash-flag ()
  "Should call delete-file with trash flag set to t."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir))
             (delete-file-args nil))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil))
                  ((symbol-function 'delete-file)
                   (lambda (file trash)
                     (setq delete-file-args (list file trash)))))
          (cj/delete-buffer-and-file)
          (should (equal delete-file-args (list test-file t)))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-shows-message ()
  "Should display message for non-VC deletes."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir))
             (message-output nil))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-output (apply #'format fmt args)))))
          (cj/delete-buffer-and-file)
          (should (string-match-p "Deleted file.*test.txt" message-output))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-vc-file-uses-vc-delete ()
  "Should call vc-delete-file for VC files."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir))
             (vc-delete-called nil))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) 'Git))
                  ((symbol-function 'vc-delete-file)
                   (lambda (file)
                     (setq vc-delete-called file)
                     ;; Simulate vc-delete-file killing the buffer
                     (when (get-file-buffer file)
                       (kill-buffer (get-file-buffer file)))
                     ;; Actually delete the file for test cleanup
                     (delete-file file t))))
          (cj/delete-buffer-and-file)
          (should (string= vc-delete-called test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-non-vc-file-uses-delete-file ()
  "Should call delete-file for non-VC files."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir))
             (delete-file-called nil))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil))
                  ((symbol-function 'delete-file)
                   (lambda (file trash)
                     (setq delete-file-called file))))
          (cj/delete-buffer-and-file)
          (should (string= delete-file-called test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-returns-implicitly ()
  "Should return result of last expression."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (let ((result (cj/delete-buffer-and-file)))
            ;; kill-buffer returns t, so result should be t
            (should (eq result t)))))
    (test-delete-buffer-and-file-teardown)))

;;; Boundary Cases - File Content

(ert-deftest test-delete-buffer-and-file-empty-file ()
  "Should delete empty file."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "empty.txt" test-dir)))
        (with-temp-file test-file)
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-large-file ()
  "Should delete large file."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "large.txt" test-dir))
             (large-content (make-string 100000 ?x)))
        (with-temp-file test-file
          (insert large-content))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-binary-file ()
  "Should delete binary file."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "binary.dat" test-dir))
             (binary-content (string 0 1 2 3 255 254 253)))
        (with-temp-file test-file
          (set-buffer-multibyte nil)
          (insert binary-content))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-with-unicode-content ()
  "Should delete file with Unicode content."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "unicode.txt" test-dir))
             (content "Hello 世界 مرحبا Привет"))
        (with-temp-file test-file
          (insert content))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

;;; Boundary Cases - File Naming

(ert-deftest test-delete-buffer-and-file-unicode-filename ()
  "Should delete file with Unicode filename."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "café.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-spaces-in-filename ()
  "Should delete file with spaces in name."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "my file.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-special-chars-filename ()
  "Should delete file with special characters."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "[test]-(1).txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-hidden-file ()
  "Should delete hidden file."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name ".hidden" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-no-extension ()
  "Should delete file without extension."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "README" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-very-long-filename ()
  "Should delete file with very long name."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (long-name (concat (make-string 200 ?x) ".txt"))
             (test-file (expand-file-name long-name test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

;;; Boundary Cases - Buffer State

(ert-deftest test-delete-buffer-and-file-with-unsaved-changes ()
  "Should handle buffer with unsaved changes."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "original"))
        (find-file test-file)
        (insert " modified")
        (should (buffer-modified-p))
        (let ((buf (current-buffer)))
          (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file)
            (should-not (file-exists-p test-file))
            (should-not (buffer-live-p buf)))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-read-only-buffer ()
  "Should handle read-only buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (read-only-mode 1)
        (let ((buf (current-buffer)))
          (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file)
            (should-not (file-exists-p test-file))
            (should-not (buffer-live-p buf)))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-multiple-windows ()
  "Should work when buffer displayed in multiple windows."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (delete-other-windows)
        (split-window)
        (other-window 1)
        (switch-to-buffer (get-file-buffer test-file))
        (let ((buf (current-buffer)))
          (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file)
            (should-not (file-exists-p test-file))
            (should-not (buffer-live-p buf))))
        (delete-other-windows))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-buffer-not-current ()
  "Should only operate on current buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (file1 (expand-file-name "file1.txt" test-dir))
             (file2 (expand-file-name "file2.txt" test-dir)))
        (with-temp-file file1
          (insert "content1"))
        (with-temp-file file2
          (insert "content2"))
        (find-file file1)
        (find-file file2)
        ;; Current buffer is file2
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          ;; file2 should be deleted, file1 should still exist
          (should-not (file-exists-p file2))
          (should (file-exists-p file1)))
        (kill-buffer (get-file-buffer file1)))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-narrowed-buffer ()
  "Should work with narrowed buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "Line 1\nLine 2\nLine 3"))
        (find-file test-file)
        (goto-char (point-min))
        (forward-line 1)
        (narrow-to-region (point) (line-end-position))
        (let ((buf (current-buffer)))
          (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file)
            (should-not (file-exists-p test-file))
            (should-not (buffer-live-p buf)))))
    (test-delete-buffer-and-file-teardown)))

;;; Error Cases - Buffer Issues

(ert-deftest test-delete-buffer-and-file-non-file-buffer-does-nothing ()
  "Should do nothing if buffer not visiting file."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (with-temp-buffer
        (rename-buffer "non-file-buffer" t)
        (let ((buf (current-buffer)))
          (cj/delete-buffer-and-file)
          ;; Buffer should still be alive
          (should (buffer-live-p buf))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-scratch-buffer-does-nothing ()
  "Should do nothing for scratch buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (with-current-buffer "*scratch*"
        (cj/delete-buffer-and-file)
        ;; Scratch buffer should still exist
        (should (get-buffer "*scratch*")))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-already-killed-buffer ()
  "Should error when operating on killed buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir))
             (buf nil))
        (with-temp-file test-file
          (insert "content"))
        (setq buf (find-file test-file))
        (kill-buffer buf)
        (should-error
         (with-current-buffer buf
           (cj/delete-buffer-and-file))))
    (test-delete-buffer-and-file-teardown)))

;;; Error Cases - File Issues

(ert-deftest test-delete-buffer-and-file-already-deleted-file ()
  "Should propagate error when delete-file fails on missing file."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil))
                  ((symbol-function 'delete-file)
                   (lambda (file &optional _trash)
                     (signal 'file-missing (list "Removing old name" "No such file or directory" file)))))
          ;; Should propagate error from delete-file
          (should-error (cj/delete-buffer-and-file) :type 'file-missing)))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-no-delete-permission ()
  "Should propagate error when delete-file fails due to permissions."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil))
                  ((symbol-function 'delete-file)
                   (lambda (file &optional _trash)
                     (signal 'file-error (list "Removing old name" "Permission denied" file)))))
          ;; Should propagate error from delete-file
          (should-error (cj/delete-buffer-and-file) :type 'file-error)))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-no-write-permission-directory ()
  "Should error if directory not writable."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (set-file-modes test-dir #o555)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (should-error (cj/delete-buffer-and-file))
          (set-file-modes test-dir #o755)))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-open-in-other-buffer ()
  "Should handle file open in another buffer."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (let ((buf1 (current-buffer)))
          (find-file test-file)
          (let ((buf2 (current-buffer)))
            ;; Both buffers visiting same file
            (should (eq buf1 buf2))
            (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
              (cj/delete-buffer-and-file)
              (should-not (file-exists-p test-file))
              (should-not (buffer-live-p buf1))))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-symlink-file ()
  "Should handle symlink files."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (real-file (expand-file-name "real.txt" test-dir))
             (symlink (expand-file-name "link.txt" test-dir)))
        (with-temp-file real-file
          (insert "content"))
        (make-symbolic-link real-file symlink)
        (find-file symlink)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          ;; Symlink should be deleted, real file should remain
          (should-not (file-exists-p symlink))
          (should (file-exists-p real-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-symlink-directory ()
  "Should handle files in symlinked directories."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((real-dir (cj/create-test-subdirectory "real"))
             (link-dir (expand-file-name "link" cj/test-base-dir))
             (test-file (expand-file-name "test.txt" real-dir)))
        (with-temp-file test-file
          (insert "content"))
        (make-symbolic-link real-dir link-dir)
        (let ((file-via-link (expand-file-name "test.txt" link-dir)))
          (find-file file-via-link)
          (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file)
            ;; File should be deleted
            (should-not (file-exists-p test-file)))))
    (test-delete-buffer-and-file-teardown)))

;;; Edge Cases - Version Control

(ert-deftest test-delete-buffer-and-file-git-tracked-file ()
  "Should use vc-delete-file for git files."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir))
             (vc-delete-called nil))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) 'Git))
                  ((symbol-function 'vc-delete-file)
                   (lambda (file)
                     (setq vc-delete-called t)
                     (when (get-file-buffer file)
                       (kill-buffer (get-file-buffer file)))
                     (delete-file file t))))
          (cj/delete-buffer-and-file)
          (should vc-delete-called)))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-untracked-in-vc-repo ()
  "Should use delete-file for untracked files in VC repo."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "untracked.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        ;; vc-backend returns nil for untracked files
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) nil)))
          (cj/delete-buffer-and-file)
          (should-not (file-exists-p test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-vc-backend-detection ()
  "Should correctly detect VC backend."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir))
             (backend-checked nil))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend)
                   (lambda (file)
                     (setq backend-checked file)
                     nil)))
          (cj/delete-buffer-and-file)
          (should (string= backend-checked test-file))))
    (test-delete-buffer-and-file-teardown)))

(ert-deftest test-delete-buffer-and-file-vc-delete-fails ()
  "Should propagate vc-delete-file errors."
  (test-delete-buffer-and-file-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "test"))
             (test-file (expand-file-name "test.txt" test-dir)))
        (with-temp-file test-file
          (insert "content"))
        (find-file test-file)
        (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) 'Git))
                  ((symbol-function 'vc-delete-file)
                   (lambda (file)
                     (error "VC operation failed"))))
          (should-error (cj/delete-buffer-and-file))))
    (test-delete-buffer-and-file-teardown)))

(provide 'test-custom-buffer-file-delete-buffer-and-file)
;;; test-custom-buffer-file-delete-buffer-and-file.el ends here
