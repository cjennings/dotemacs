;;; test-undead-buffers-undead-buffer-p.el --- Tests for cj/undead-buffer-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/undead-buffer-p function from undead-buffers.el

;;; Code:

(require 'ert)
(require 'undead-buffers)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-undead-buffer-p-setup ()
  "Setup for undead-buffer-p tests."
  (cj/create-test-base-dir))

(defun test-undead-buffer-p-teardown ()
  "Teardown for undead-buffer-p tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-undead-buffer-p-modified-file-buffer-should-return-true ()
  "A modified file-backed buffer not in undead list should return t."
  (test-undead-buffer-p-setup)
  (unwind-protect
      (let* ((file (cj/create-temp-test-file-with-content "test content"))
             (buf (find-file-noselect file)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert "more content")
                (should (cj/undead-buffer-p))))
          (when (buffer-live-p buf)
            (set-buffer-modified-p nil)
            (kill-buffer buf))))
    (test-undead-buffer-p-teardown)))

(ert-deftest test-undead-buffer-p-undead-modified-file-buffer-should-return-nil ()
  "A modified file-backed undead buffer should return nil."
  (test-undead-buffer-p-setup)
  (unwind-protect
      (let* ((orig (copy-sequence cj/undead-buffer-list))
             (file (cj/create-temp-test-file-with-content "test content"))
             (buf (find-file-noselect file)))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list (buffer-name buf))
              (with-current-buffer buf
                (insert "more content")
                (should-not (cj/undead-buffer-p))))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf)
            (set-buffer-modified-p nil)
            (kill-buffer buf))))
    (test-undead-buffer-p-teardown)))

(ert-deftest test-undead-buffer-p-scratch-buffer-should-return-nil ()
  "The *scratch* buffer should return nil (it's undead)."
  (test-undead-buffer-p-setup)
  (unwind-protect
      (with-current-buffer "*scratch*"
        (should-not (cj/undead-buffer-p)))
    (test-undead-buffer-p-teardown)))

;;; Boundary Cases

(ert-deftest test-undead-buffer-p-unmodified-file-buffer-should-return-nil ()
  "An unmodified file buffer should return nil."
  (test-undead-buffer-p-setup)
  (unwind-protect
      (let* ((file (cj/create-temp-test-file-with-content "test content"))
             (buf (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer buf
              (should-not (cj/undead-buffer-p)))
          (when (buffer-live-p buf)
            (kill-buffer buf))))
    (test-undead-buffer-p-teardown)))

(ert-deftest test-undead-buffer-p-modified-buffer-without-file-should-return-nil ()
  "A modified buffer without a backing file should return nil."
  (test-undead-buffer-p-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-no-file*")))
        (unwind-protect
            (with-current-buffer buf
              (insert "content")
              (set-buffer-modified-p t)
              (should-not (cj/undead-buffer-p)))
          (when (buffer-live-p buf)
            (set-buffer-modified-p nil)
            (kill-buffer buf))))
    (test-undead-buffer-p-teardown)))

(ert-deftest test-undead-buffer-p-temporary-buffer-should-return-nil ()
  "A temporary buffer should return nil."
  (test-undead-buffer-p-setup)
  (unwind-protect
      (with-temp-buffer
        (should-not (cj/undead-buffer-p)))
    (test-undead-buffer-p-teardown)))

(provide 'test-undead-buffers-undead-buffer-p)
;;; test-undead-buffers-undead-buffer-p.el ends here
