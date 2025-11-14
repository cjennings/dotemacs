;;; test-undead-buffers-kill-other-window.el --- Tests for cj/kill-other-window -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/kill-other-window function from undead-buffers.el

;;; Code:

(require 'ert)
(require 'undead-buffers)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-kill-other-window-setup ()
  "Setup for kill-other-window tests."
  (cj/create-test-base-dir)
  (delete-other-windows))

(defun test-kill-other-window-teardown ()
  "Teardown for kill-other-window tests."
  (delete-other-windows)
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-kill-other-window-two-windows-should-delete-other-and-kill-buffer ()
  "With two windows, should delete other window and kill its buffer."
  (test-kill-other-window-setup)
  (unwind-protect
      (let ((buf1 (current-buffer))
            (buf2 (generate-new-buffer "*test-other*")))
        (unwind-protect
            (progn
              (split-window)
              (let ((win1 (selected-window))
                    (win2 (next-window)))
                (set-window-buffer win2 buf2)
                (select-window win1)
                (cj/kill-other-window)
                (should-not (window-live-p win2))
                (should-not (buffer-live-p buf2))))
          (when (buffer-live-p buf2) (kill-buffer buf2))))
    (test-kill-other-window-teardown)))

(ert-deftest test-kill-other-window-two-windows-undead-buffer-should-delete-other-and-bury ()
  "With two windows, undead buffer in other window should be buried."
  (test-kill-other-window-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (buf1 (current-buffer))
            (buf2 (generate-new-buffer "*test-undead-other*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-undead-other*")
              (split-window)
              (let ((win1 (selected-window))
                    (win2 (next-window)))
                (set-window-buffer win2 buf2)
                (select-window win1)
                (cj/kill-other-window)
                (should-not (window-live-p win2))
                (should (buffer-live-p buf2))))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf2) (kill-buffer buf2))))
    (test-kill-other-window-teardown)))

;;; Boundary Cases

(ert-deftest test-kill-other-window-single-window-should-only-kill-buffer ()
  "With single window, should only kill the current buffer."
  (test-kill-other-window-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-single-other*")))
        (unwind-protect
            (progn
              (switch-to-buffer buf)
              (should (one-window-p))
              (cj/kill-other-window)
              (should (one-window-p))
              (should-not (buffer-live-p buf)))
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-other-window-teardown)))

(ert-deftest test-kill-other-window-three-windows-should-delete-one ()
  "With three windows, should delete one window."
  (test-kill-other-window-setup)
  (unwind-protect
      (let ((buf1 (current-buffer))
            (buf2 (generate-new-buffer "*test-three-1*"))
            (buf3 (generate-new-buffer "*test-three-2*")))
        (unwind-protect
            (progn
              (split-window)
              (split-window)
              (set-window-buffer (nth 1 (window-list)) buf2)
              (set-window-buffer (nth 2 (window-list)) buf3)
              (select-window (car (window-list)))
              (should (= 3 (length (window-list))))
              (cj/kill-other-window)
              (should (= 2 (length (window-list)))))
          (when (buffer-live-p buf2) (kill-buffer buf2))
          (when (buffer-live-p buf3) (kill-buffer buf3))))
    (test-kill-other-window-teardown)))

(ert-deftest test-kill-other-window-wraps-to-first-window-correctly ()
  "Should correctly cycle through windows with other-window."
  (test-kill-other-window-setup)
  (unwind-protect
      (let ((buf1 (current-buffer))
            (buf2 (generate-new-buffer "*test-wrap*")))
        (unwind-protect
            (progn
              (split-window)
              (let ((win2 (next-window)))
                (set-window-buffer win2 buf2)
                (select-window (car (window-list)))
                (cj/kill-other-window)
                (should-not (window-live-p win2))))
          (when (buffer-live-p buf2) (kill-buffer buf2))))
    (test-kill-other-window-teardown)))

(provide 'test-undead-buffers-kill-other-window)
;;; test-undead-buffers-kill-other-window.el ends here
