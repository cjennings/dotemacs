;;; test-config-utilities--info-commands-smoke.el --- Smoke tests for the cj/info-* family + reload-init-file -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke tests for the simple interactive commands in config-utilities.el:
;; cj/info-emacs-build, cj/info-loaded-packages, cj/info-loaded-features,
;; cj/reload-init-file, and cj/org-alert-list-timers.  Each is exercised
;; with `pop-to-buffer' and `load-file' mocked so the test doesn't open
;; real windows or reload init.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(defmacro test-config-utilities--with-display-mocks (&rest body)
  "Run BODY with `pop-to-buffer' replaced so it doesn't open a window."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'pop-to-buffer)
              (lambda (buf &rest _) (set-buffer buf) (current-buffer))))
     ,@body))

(ert-deftest test-config-utilities-info-emacs-build-creates-summary-buffer ()
  "Smoke: `cj/info-emacs-build' creates *Emacs-Build-Summary* with content."
  (let ((buf (get-buffer "*Emacs-Build-Summary*")))
    (when buf (let ((kill-buffer-query-functions nil)) (kill-buffer buf))))
  (test-config-utilities--with-display-mocks
    (cj/info-emacs-build)
    (let ((buf (get-buffer "*Emacs-Build-Summary*")))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (string-match-p "Version:" (buffer-string))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest test-config-utilities-info-loaded-packages-creates-buffer ()
  "Smoke: `cj/info-loaded-packages' creates *loaded-packages* with a count line."
  (let ((buf (get-buffer "*loaded-packages*")))
    (when buf (let ((kill-buffer-query-functions nil)) (kill-buffer buf))))
  (test-config-utilities--with-display-mocks
    (cj/info-loaded-packages)
    (let ((buf (get-buffer "*loaded-packages*")))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (string-match-p "total packages currently loaded"
                                (buffer-string))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest test-config-utilities-info-loaded-features-creates-buffer ()
  "Smoke: `cj/info-loaded-features' creates *loaded-features* with a count line."
  (let ((buf (get-buffer "*loaded-features*")))
    (when buf (let ((kill-buffer-query-functions nil)) (kill-buffer buf))))
  (test-config-utilities--with-display-mocks
    (cj/info-loaded-features)
    (let ((buf (get-buffer "*loaded-features*")))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (string-match-p "features currently loaded"
                                (buffer-string))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest test-config-utilities-reload-init-file-loads-user-init-file ()
  "Smoke: `cj/reload-init-file' calls `load-file' on `user-init-file'."
  (let (called-with)
    (cl-letf (((symbol-function 'load-file)
               (lambda (file) (setq called-with file))))
      (cj/reload-init-file))
    (should (equal called-with user-init-file))))

(ert-deftest test-config-utilities-org-alert-list-timers-no-timers-messages ()
  "Boundary: `cj/org-alert-list-timers' with no matching timers emits a
\"none found\" message."
  (let (captured)
    (cl-letf ((timer-list nil)
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured (apply #'format fmt args)))))
      (cj/org-alert-list-timers))
    (should (string-match-p "No org-alert-check timers" captured))))

(provide 'test-config-utilities--info-commands-smoke)
;;; test-config-utilities--info-commands-smoke.el ends here
