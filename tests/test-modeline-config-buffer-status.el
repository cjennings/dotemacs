;;; test-modeline-config-buffer-status.el --- buffer-status segment -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--modeline-buffer-status': the modified / read-only
;; indicator.  Read-only wins over modified; the modified dot shows only
;; for file-visiting buffers (special buffers are perpetually modified
;; and would be noise); clean file buffers show nothing.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'modeline-config)

(ert-deftest test-modeline-config-buffer-status-modified-file-shows-dot ()
  "Normal: a modified file-visiting buffer returns the warning-faced dot."
  (let ((file (make-temp-file "modeline-status-" nil ".txt")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "x")
          (let ((status (cj/--modeline-buffer-status)))
            (should (stringp status))
            (should (string-match-p "●" status))
            (should (eq (get-text-property
                         (string-match "●" status) 'face status)
                        'warning)))
          (set-buffer-modified-p nil)
          (kill-buffer))
      (delete-file file))))

(ert-deftest test-modeline-config-buffer-status-clean-file-nil ()
  "Normal: an unmodified file-visiting buffer returns nil."
  (let ((file (make-temp-file "modeline-status-" nil ".txt")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (should-not (cj/--modeline-buffer-status))
          (kill-buffer))
      (delete-file file))))

(ert-deftest test-modeline-config-buffer-status-read-only-shows-lock ()
  "Normal: a read-only buffer returns the read-only indicator."
  (with-temp-buffer
    (setq buffer-read-only t)
    (let ((status (cj/--modeline-buffer-status)))
      (should (stringp status))
      (should (> (length status) 0)))))

(ert-deftest test-modeline-config-buffer-status-read-only-wins-over-modified ()
  "Boundary: read-only + modified shows the read-only indicator, not the dot."
  (let ((file (make-temp-file "modeline-status-" nil ".txt")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "x")
          (setq buffer-read-only t)
          (let ((status (cj/--modeline-buffer-status)))
            (should (stringp status))
            (should-not (string-match-p "●" status)))
          (setq buffer-read-only nil)
          (set-buffer-modified-p nil)
          (kill-buffer))
      (delete-file file))))

(ert-deftest test-modeline-config-buffer-status-modified-non-file-nil ()
  "Boundary: a modified non-file buffer (scratch-like) returns nil."
  (with-temp-buffer
    (insert "x")
    (should (buffer-modified-p))
    (should-not (cj/--modeline-buffer-status))))

(provide 'test-modeline-config-buffer-status)
;;; test-modeline-config-buffer-status.el ends here
