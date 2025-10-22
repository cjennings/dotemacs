;;; test-undead-buffers-kill-all-other-buffers-and-windows.el --- Tests for cj/kill-all-other-buffers-and-windows -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/kill-all-other-buffers-and-windows function from undead-buffers.el

;;; Code:

(require 'ert)
(require 'undead-buffers)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-kill-all-other-buffers-and-windows-setup ()
  "Setup for kill-all-other-buffers-and-windows tests."
  (cj/create-test-base-dir)
  (delete-other-windows))

(defun test-kill-all-other-buffers-and-windows-teardown ()
  "Teardown for kill-all-other-buffers-and-windows tests."
  (delete-other-windows)
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-kill-all-other-buffers-and-windows-should-kill-regular-buffers ()
  "Should kill all regular buffers except current."
  (test-kill-all-other-buffers-and-windows-setup)
  (unwind-protect
      (let ((main (current-buffer))
            (buf1 (generate-new-buffer "*test-regular-1*"))
            (buf2 (generate-new-buffer "*test-regular-2*")))
        (unwind-protect
            (progn
              (should (buffer-live-p buf1))
              (should (buffer-live-p buf2))
              (cj/kill-all-other-buffers-and-windows)
              (should (buffer-live-p main))
              (should-not (buffer-live-p buf1))
              (should-not (buffer-live-p buf2)))
          (when (buffer-live-p buf1) (kill-buffer buf1))
          (when (buffer-live-p buf2) (kill-buffer buf2))))
    (test-kill-all-other-buffers-and-windows-teardown)))

(ert-deftest test-kill-all-other-buffers-and-windows-should-bury-undead-buffers ()
  "Should bury undead buffers instead of killing them."
  (test-kill-all-other-buffers-and-windows-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (main (current-buffer))
            (buf1 (generate-new-buffer "*test-undead-1*"))
            (buf2 (generate-new-buffer "*test-undead-2*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-undead-1*")
              (add-to-list 'cj/undead-buffer-list "*test-undead-2*")
              (cj/kill-all-other-buffers-and-windows)
              (should (buffer-live-p main))
              (should (buffer-live-p buf1))
              (should (buffer-live-p buf2)))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf1) (kill-buffer buf1))
          (when (buffer-live-p buf2) (kill-buffer buf2))))
    (test-kill-all-other-buffers-and-windows-teardown)))

(ert-deftest test-kill-all-other-buffers-and-windows-should-keep-current-buffer ()
  "Should always keep the current buffer alive."
  (test-kill-all-other-buffers-and-windows-setup)
  (unwind-protect
      (let ((main (current-buffer)))
        (cj/kill-all-other-buffers-and-windows)
        (should (buffer-live-p main))
        (should (eq main (current-buffer))))
    (test-kill-all-other-buffers-and-windows-teardown)))

(ert-deftest test-kill-all-other-buffers-and-windows-should-delete-all-other-windows ()
  "Should delete all windows except current."
  (test-kill-all-other-buffers-and-windows-setup)
  (unwind-protect
      (progn
        (split-window)
        (split-window)
        (should (> (length (window-list)) 1))
        (cj/kill-all-other-buffers-and-windows)
        (should (one-window-p)))
    (test-kill-all-other-buffers-and-windows-teardown)))

;;; Boundary Cases

(ert-deftest test-kill-all-other-buffers-and-windows-mixed-undead-and-regular-buffers ()
  "With mix of undead and regular buffers, should handle both correctly."
  (test-kill-all-other-buffers-and-windows-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (main (current-buffer))
            (regular (generate-new-buffer "*test-regular*"))
            (undead (generate-new-buffer "*test-undead*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-undead*")
              (cj/kill-all-other-buffers-and-windows)
              (should (buffer-live-p main))
              (should-not (buffer-live-p regular))
              (should (buffer-live-p undead)))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p regular) (kill-buffer regular))
          (when (buffer-live-p undead) (kill-buffer undead))))
    (test-kill-all-other-buffers-and-windows-teardown)))

(ert-deftest test-kill-all-other-buffers-and-windows-all-undead-buffers-should-bury-all ()
  "When all other buffers are undead, should bury all of them."
  (test-kill-all-other-buffers-and-windows-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (main (current-buffer))
            (undead1 (generate-new-buffer "*test-all-undead-1*"))
            (undead2 (generate-new-buffer "*test-all-undead-2*"))
            (undead3 (generate-new-buffer "*test-all-undead-3*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-all-undead-1*")
              (add-to-list 'cj/undead-buffer-list "*test-all-undead-2*")
              (add-to-list 'cj/undead-buffer-list "*test-all-undead-3*")
              (cj/kill-all-other-buffers-and-windows)
              (should (buffer-live-p main))
              (should (buffer-live-p undead1))
              (should (buffer-live-p undead2))
              (should (buffer-live-p undead3)))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p undead1) (kill-buffer undead1))
          (when (buffer-live-p undead2) (kill-buffer undead2))
          (when (buffer-live-p undead3) (kill-buffer undead3))))
    (test-kill-all-other-buffers-and-windows-teardown)))

(ert-deftest test-kill-all-other-buffers-and-windows-should-prompt-for-modified-buffers ()
  "Should call cj/save-some-buffers to handle modified buffers."
  (test-kill-all-other-buffers-and-windows-setup)
  (unwind-protect
      (let ((main (current-buffer))
            (file (cj/create-temp-test-file-with-content "original"))
            save-called)
        ;; Mock cj/save-some-buffers to track if it's called
        (cl-letf (((symbol-function 'cj/save-some-buffers)
                   (lambda (&optional arg)
                     (setq save-called t))))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (progn
                  (with-current-buffer buf
                    (insert "modified"))
                  (cj/kill-all-other-buffers-and-windows)
                  (should save-called))
              (when (buffer-live-p buf)
                (set-buffer-modified-p nil)
                (kill-buffer buf))))))
    (test-kill-all-other-buffers-and-windows-teardown)))

(provide 'test-undead-buffers-kill-all-other-buffers-and-windows)
;;; test-undead-buffers-kill-all-other-buffers-and-windows.el ends here
