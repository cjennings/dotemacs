;;; test-undead-buffers-kill-buffer-or-bury-alive.el --- Tests for cj/kill-buffer-or-bury-alive -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/kill-buffer-or-bury-alive function from undead-buffers.el

;;; Code:

(require 'ert)
(require 'undead-buffers)
(require 'testutil-general)

;;; Setup and Teardown

(defun test-kill-buffer-or-bury-alive-setup ()
  "Setup for kill-buffer-or-bury-alive tests."
  (cj/create-test-base-dir))

(defun test-kill-buffer-or-bury-alive-teardown ()
  "Teardown for kill-buffer-or-bury-alive tests."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-kill-buffer-or-bury-alive-regular-buffer-should-kill ()
  "Killing a regular buffer not in undead list should kill it."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-regular*")))
        (should (buffer-live-p buf))
        (cj/kill-buffer-or-bury-alive buf)
        (should-not (buffer-live-p buf)))
    (test-kill-buffer-or-bury-alive-teardown)))

(ert-deftest test-kill-buffer-or-bury-alive-undead-buffer-should-bury ()
  "Killing an undead buffer should bury it instead."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (buf (generate-new-buffer "*test-undead*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-undead*")
              (should (buffer-live-p buf))
              (cj/kill-buffer-or-bury-alive buf)
              (should (buffer-live-p buf)))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-buffer-or-bury-alive-teardown)))

(ert-deftest test-kill-buffer-or-bury-alive-with-prefix-arg-should-add-to-undead-list ()
  "Calling with prefix arg should add buffer to undead list."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (buf (generate-new-buffer "*test-prefix*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (let ((current-prefix-arg '(4)))
                  (cj/kill-buffer-or-bury-alive buf)))
              (should (member "*test-prefix*" cj/undead-buffer-list))
              (should (buffer-live-p buf)))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf) (kill-buffer buf))))
    (test-kill-buffer-or-bury-alive-teardown)))

(ert-deftest test-kill-buffer-or-bury-alive-scratch-buffer-should-bury ()
  "The *scratch* buffer (in default list) should be buried."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((scratch (get-buffer-create "*scratch*")))
        (should (buffer-live-p scratch))
        (cj/kill-buffer-or-bury-alive scratch)
        (should (buffer-live-p scratch)))
    (test-kill-buffer-or-bury-alive-teardown)))

;;; Boundary Cases

(ert-deftest test-kill-buffer-or-bury-alive-buffer-by-name-string-should-work ()
  "Passing buffer name as string should work."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-string*")))
        (should (buffer-live-p buf))
        (cj/kill-buffer-or-bury-alive "*test-string*")
        (should-not (buffer-live-p buf)))
    (test-kill-buffer-or-bury-alive-teardown)))

(ert-deftest test-kill-buffer-or-bury-alive-buffer-by-buffer-object-should-work ()
  "Passing buffer object should work."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-object*")))
        (should (buffer-live-p buf))
        (cj/kill-buffer-or-bury-alive buf)
        (should-not (buffer-live-p buf)))
    (test-kill-buffer-or-bury-alive-teardown)))

(ert-deftest test-kill-buffer-or-bury-alive-modified-undead-buffer-should-bury-without-prompt ()
  "Modified undead buffer should be buried without save prompt."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((orig (copy-sequence cj/undead-buffer-list))
            (buf (generate-new-buffer "*test-modified*")))
        (unwind-protect
            (progn
              (add-to-list 'cj/undead-buffer-list "*test-modified*")
              (with-current-buffer buf
                (insert "some text")
                (set-buffer-modified-p t))
              (cj/kill-buffer-or-bury-alive buf)
              (should (buffer-live-p buf)))
          (setq cj/undead-buffer-list orig)
          (when (buffer-live-p buf)
            (set-buffer-modified-p nil)
            (kill-buffer buf))))
    (test-kill-buffer-or-bury-alive-teardown)))

;;; Error Cases

(ert-deftest test-kill-buffer-or-bury-alive-nonexistent-buffer-should-error ()
  "Passing a non-existent buffer name should error."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (should-error (cj/kill-buffer-or-bury-alive "*nonexistent-buffer-xyz*"))
    (test-kill-buffer-or-bury-alive-teardown)))

(ert-deftest test-kill-buffer-or-bury-alive-killed-buffer-object-should-error ()
  "Passing a killed buffer object should error."
  (test-kill-buffer-or-bury-alive-setup)
  (unwind-protect
      (let ((buf (generate-new-buffer "*test-killed*")))
        (kill-buffer buf)
        (should-error (cj/kill-buffer-or-bury-alive buf)))
    (test-kill-buffer-or-bury-alive-teardown)))

(provide 'test-undead-buffers-kill-buffer-or-bury-alive)
;;; test-undead-buffers-kill-buffer-or-bury-alive.el ends here
