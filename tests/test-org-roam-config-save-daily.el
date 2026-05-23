;;; test-org-roam-config-save-daily.el --- Tests for the daily-save helper -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--org-roam-save-daily' saves the daily's visiting buffer when it has
;; unsaved changes, so a freshly-copied completed task is never lost to a
;; crash and shutdown never prompts about an unsaved journal buffer.  The save
;; runs whether or not the copy needed an `org-refile' (the previous version
;; only saved in the refile branch).  `save-buffer' is stubbed (file-I/O
;; boundary) so the tests assert the modified-guard, not actual disk writes.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-roam-config)

(ert-deftest test-org-roam-save-daily-saves-modified-buffer ()
  "Normal: a modified visiting buffer is saved."
  (let ((f (make-temp-file "test-daily" nil ".org")) (saved 0))
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect f)
            (insert "x") (set-buffer-modified-p t))
          (cl-letf (((symbol-function 'save-buffer)
                     (lambda (&rest _) (cl-incf saved) (set-buffer-modified-p nil))))
            (cj/--org-roam-save-daily f))
          (should (= 1 saved)))
      (when (find-buffer-visiting f) (kill-buffer (find-buffer-visiting f)))
      (delete-file f))))

(ert-deftest test-org-roam-save-daily-skips-unmodified-buffer ()
  "Boundary: an unmodified visiting buffer is not saved."
  (let ((f (make-temp-file "test-daily" nil ".org")) (saved 0))
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect f) (set-buffer-modified-p nil))
          (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (cl-incf saved))))
            (cj/--org-roam-save-daily f))
          (should (= 0 saved)))
      (when (find-buffer-visiting f) (kill-buffer (find-buffer-visiting f)))
      (delete-file f))))

(ert-deftest test-org-roam-save-daily-no-visiting-buffer-noop ()
  "Boundary: a file with no visiting buffer is a safe no-op."
  (let ((saved 0))
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (cl-incf saved))))
      (cj/--org-roam-save-daily "/nonexistent/no-buffer-here.org"))
    (should (= 0 saved))))

(provide 'test-org-roam-config-save-daily)
;;; test-org-roam-config-save-daily.el ends here
