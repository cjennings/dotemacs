;;; test-dwim-shell-config-password-file.el --- Tests for password-file lifetime -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the password-file helpers in dwim-shell-config: the on-completion
;; cleanup callback factory and the run-with-password-file wrapper.  The point
;; of these helpers is that the password temp file is deleted only after the
;; spawned async process exits (success or failure), not when it is launched.
;; The async `dwim-shell-command-on-marked-files' call is stubbed so no external
;; process runs.

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'cl-lib)
(require 'dwim-shell-config)

;; ---------------------------------------------------------------------------
;;; cj/dwim-shell--password-cleanup-callback
;; ---------------------------------------------------------------------------

(ert-deftest test-dwim-password-cleanup-deletes-on-success ()
  "Normal: callback deletes the temp file when the process exits cleanly."
  (let ((temp (make-temp-file "dwim-pass-test-"))
        (pbuf (generate-new-buffer " *test-proc*")))
    (unwind-protect
        (cl-letf (((symbol-function 'processp) (lambda (_) t))
                  ((symbol-function 'process-exit-status) (lambda (_) 0)))
          (funcall (cj/dwim-shell--password-cleanup-callback temp) pbuf 'fake-proc)
          (should-not (file-exists-p temp)))
      (when (file-exists-p temp) (delete-file temp))
      (when (buffer-live-p pbuf) (kill-buffer pbuf)))))

(ert-deftest test-dwim-password-cleanup-deletes-on-error ()
  "Error: callback still deletes the temp file when the process exits non-zero."
  (let ((temp (make-temp-file "dwim-pass-test-"))
        (pbuf (generate-new-buffer " *test-proc*")))
    (unwind-protect
        (cl-letf (((symbol-function 'processp) (lambda (_) t))
                  ((symbol-function 'process-exit-status) (lambda (_) 1))
                  ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
          (funcall (cj/dwim-shell--password-cleanup-callback temp) pbuf 'fake-proc)
          (should-not (file-exists-p temp)))
      (when (file-exists-p temp) (delete-file temp))
      (when (buffer-live-p pbuf) (kill-buffer pbuf)))))

(ert-deftest test-dwim-password-cleanup-missing-file-no-error ()
  "Boundary: callback does not error when the temp file is already gone."
  (let ((temp (make-temp-file "dwim-pass-test-"))
        (pbuf (generate-new-buffer " *test-proc*")))
    (delete-file temp)
    (unwind-protect
        (cl-letf (((symbol-function 'processp) (lambda (_) t))
                  ((symbol-function 'process-exit-status) (lambda (_) 0)))
          (funcall (cj/dwim-shell--password-cleanup-callback temp) pbuf 'fake-proc)
          (should-not (file-exists-p temp)))
      (when (buffer-live-p pbuf) (kill-buffer pbuf)))))

;; ---------------------------------------------------------------------------
;;; cj/dwim-shell--run-with-password-file
;; ---------------------------------------------------------------------------

(ert-deftest test-dwim-run-with-password-file-writes-and-defers-cleanup ()
  "Normal: writes a mode-600 temp file, passes :on-completion, keeps the file
alive after a successful launch (the callback owns deletion)."
  (let (captured-script captured-keys)
    (cl-letf (((symbol-function 'dwim-shell-command-on-marked-files)
               (lambda (_buffer-name script &rest keys)
                 (setq captured-script script
                       captured-keys keys))))
      (cj/dwim-shell--run-with-password-file
       "secret-pw" "Test op"
       (lambda (temp-file) (format "tool --password-file='%s' '<<f>>'" temp-file))
       :utils "tool")
      ;; the script embeds a real temp path, and that file still exists
      (should (string-match "--password-file='\\([^']*\\)'" captured-script))
      (let ((temp (match-string 1 captured-script)))
        (unwind-protect
            (progn
              (should (file-exists-p temp))
              (should (string= "secret-pw"
                               (with-temp-buffer (insert-file-contents temp)
                                                 (buffer-string))))
              (should (eq #o600 (file-modes temp)))
              (should (functionp (plist-get captured-keys :on-completion)))
              (should (equal "tool" (plist-get captured-keys :utils))))
          (when (file-exists-p temp) (delete-file temp)))))))

(ert-deftest test-dwim-run-with-password-file-cleans-up-on-launch-failure ()
  "Error: if the async launch throws before the process starts, the temp file
is cleaned up synchronously (no orphaned password file)."
  (let (leaked-temp)
    (cl-letf (((symbol-function 'dwim-shell-command-on-marked-files)
               (lambda (_buffer-name script &rest _keys)
                 ;; capture the temp path the script was built with, then throw
                 (when (string-match "FILE='\\(.*\\)'" script)
                   (setq leaked-temp (match-string 1 script)))
                 (error "simulated launch failure"))))
      (should-error
       (cj/dwim-shell--run-with-password-file
        "secret-pw" "Test op"
        (lambda (temp-file) (format "tool FILE='%s'" temp-file))
        :utils "tool"))
      (should leaked-temp)
      (should-not (file-exists-p leaked-temp)))))

(provide 'test-dwim-shell-config-password-file)
;;; test-dwim-shell-config-password-file.el ends here
