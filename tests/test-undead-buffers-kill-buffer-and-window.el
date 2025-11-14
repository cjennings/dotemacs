;;; test-undead-buffers-kill-buffer-and-window.el --- Tests for cj/kill-buffer-and-window -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/kill-buffer-and-window function from undead-buffers.el

;;; Code:

(require 'ert)
(require 'undead-buffers)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-kill-buffer-and-window-setup ()
  "Setup for kill-buffer-and-window tests."
  (cj/create-test-base-dir)
  (delete-other-windows))

(defun test-kill-buffer-and-window-teardown ()
  "Teardown for kill-buffer-and-window tests."
  (delete-other-windows)
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-kill-buffer-and-window-multiple-windows-should-delete-window-and-kill-buffer ()
  "With multiple windows, should delete window and kill buffer."
  (test-kill-buffer-and-window-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-multi*")))
        (unwind-protect
            (progn
              (split-window)
              (switch-to-buffer buf)
              (let ((win (selected-window)))
                (cj/kill-buffer-and-window)
                (should-not (window-live-p win))
                (should-not (buffer-live-p buf))))
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-buffer-and-window-teardown)))

(ert-deftest test-kill-buffer-and-window-multiple-windows-undead-buffer-should-delete-window-and-bury ()
  "With multiple windows, undead buffer should be buried and window deleted."
  (test-kill-buffer-and-window-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (buf (generate-new-buffer "*test-undead-multi*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-undead-multi*")
              (split-window)
              (switch-to-buffer buf)
              (let ((win (selected-window)))
                (cj/kill-buffer-and-window)
                (should-not (window-live-p win))
                (should (buffer-live-p buf))))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-buffer-and-window-teardown)))

;;; Boundary Cases

(ert-deftest test-kill-buffer-and-window-single-window-should-only-kill-buffer ()
  "With single window, should only kill buffer, not delete window."
  (test-kill-buffer-and-window-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-single*")))
        (unwind-protect
            (progn
              (switch-to-buffer buf)
              (should (one-window-p))
              (cj/kill-buffer-and-window)
              (should (one-window-p))
              (should-not (buffer-live-p buf)))
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-buffer-and-window-teardown)))

(ert-deftest test-kill-buffer-and-window-single-window-undead-buffer-should-only-bury ()
  "With single window, undead buffer should only be buried."
  (test-kill-buffer-and-window-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (buf (generate-new-buffer "*test-undead-single*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-undead-single*")
              (switch-to-buffer buf)
              (should (one-window-p))
              (cj/kill-buffer-and-window)
              (should (one-window-p))
              (should (buffer-live-p buf)))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-buffer-and-window-teardown)))

(ert-deftest test-kill-buffer-and-window-two-windows-should-leave-one ()
  "With two windows, should leave one window after deletion."
  (test-kill-buffer-and-window-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-two*")))
        (unwind-protect
            (progn
              (split-window)
              (set-window-buffer (selected-window) buf)
              (should (= 2 (length (window-list))))
              (cj/kill-buffer-and-window)
              (should (= 1 (length (window-list)))))
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-buffer-and-window-teardown)))

(provide 'test-undead-buffers-kill-buffer-and-window)
;;; test-undead-buffers-kill-buffer-and-window.el ends here
