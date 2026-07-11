;;; test-ui-theme-persistence.el --- Tests for UI theme persistence -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke and unit coverage for ui-theme.el persistence behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'ui-theme)

(ert-deftest test-ui-theme-default-theme-file-is-under-persist ()
  "The default theme file should live under `persist/' inside `user-emacs-directory'."
  (should (equal theme-file
                 (expand-file-name "persist/emacs-theme" user-emacs-directory))))

(ert-deftest test-ui-theme-read-missing-file-returns-nil ()
  "Reading a missing theme file should return nil."
  (let ((theme-file (expand-file-name "missing-theme" temporary-file-directory)))
    (should-not (cj/theme-read-file-contents theme-file))))

(ert-deftest test-ui-theme-write-file-contents-writes-content ()
  "Writing theme content should persist exactly that content."
  (let ((file (make-temp-file "ui-theme-write-")))
    (unwind-protect
        (progn
          (should (cj/theme-write-file-contents "modus-vivendi" file))
          (should (equal (cj/theme-read-file-contents file)
                         "modus-vivendi")))
      (delete-file file))))

(ert-deftest test-ui-theme-write-file-contents-creates-missing-parent-dir ()
  "Boundary: writing into a not-yet-existing directory creates it first.
On a fresh machine `persist/' does not exist, and `file-writable-p' returns nil
for a file inside a missing directory, so the write must create the parent."
  (let* ((sandbox (make-temp-file "ui-theme-sandbox-" t))
         (file (expand-file-name "persist/emacs-theme" sandbox)))
    (unwind-protect
        (progn
          (should-not (file-directory-p (file-name-directory file)))
          (should (cj/theme-write-file-contents "modus-vivendi" file))
          (should (equal (cj/theme-read-file-contents file) "modus-vivendi")))
      (delete-directory sandbox t))))

(ert-deftest test-ui-theme-write-file-contents-uses-write-region ()
  "Theme persistence should write directly instead of visiting the file."
  (let ((file (make-temp-file "ui-theme-write-region-"))
        write-region-args
        write-file-called)
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
               (lambda (&rest args)
                 (setq write-region-args args)
                 nil))
              ((symbol-function 'write-file)
               (lambda (&rest _args)
                 (setq write-file-called t)
                 (error "write-file should not be used"))))
          (should (cj/theme-write-file-contents "modus-vivendi" file)))
      (delete-file file))
    (should (equal (list (car write-region-args)
                         (cadr write-region-args)
                         (nth 2 write-region-args))
                   (list "modus-vivendi" nil file)))
    (should-not write-file-called)))

(ert-deftest test-ui-theme-load-valid-persisted-theme ()
  "A valid persisted theme should disable current themes and load that theme."
  (let ((file (make-temp-file "ui-theme-valid-"))
        (custom-enabled-themes '(old-theme))
        disabled
        loaded)
    (unwind-protect
        (progn
          (cj/theme-write-file-contents "modus-vivendi" file)
          (let ((theme-file file))
            (cl-letf (((symbol-function 'disable-theme)
                       (lambda (theme) (push theme disabled)))
                      ((symbol-function 'load-theme)
                       (lambda (theme &optional _no-confirm _no-enable)
                         (push theme loaded))))
              (cj/load-theme-from-file)))
          (should (equal disabled '(old-theme)))
          (should (equal loaded '(modus-vivendi))))
      (delete-file file))))

(ert-deftest test-ui-theme-load-invalid-theme-falls-back ()
  "An invalid persisted theme should disable current themes and load fallback."
  (let ((file (make-temp-file "ui-theme-invalid-"))
        (fallback-theme-name "modus-vivendi")
        (custom-enabled-themes '(old-theme))
        disabled
        loaded)
    (unwind-protect
        (progn
          (cj/theme-write-file-contents "missing-theme" file)
          (let ((theme-file file))
            (cl-letf (((symbol-function 'disable-theme)
                       (lambda (theme) (push theme disabled)))
                      ((symbol-function 'load-theme)
                       (lambda (theme &optional _no-confirm _no-enable)
                         (push theme loaded)
                         (when (eq theme 'missing-theme)
                           (error "missing theme")))))
              (cj/load-theme-from-file)))
          (should (equal disabled '(old-theme)))
          (should (equal loaded '(modus-vivendi missing-theme))))
      (delete-file file))))

(ert-deftest test-ui-theme-load-missing-file-loads-fallback ()
  "A missing theme file should disable current themes and load fallback."
  (let ((theme-file (expand-file-name "missing-theme" temporary-file-directory))
        (fallback-theme-name "modus-vivendi")
        (custom-enabled-themes '(old-theme))
        disabled
        loaded)
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled)))
              ((symbol-function 'load-theme)
               (lambda (theme &optional _no-confirm _no-enable)
                 (push theme loaded))))
      (cj/load-theme-from-file))
    (should (equal disabled '(old-theme)))
    (should (equal loaded '(modus-vivendi)))))

(ert-deftest test-ui-theme-load-literal-nil-disables-themes ()
  "A persisted literal nil should disable current themes and load nothing."
  (let ((file (make-temp-file "ui-theme-nil-"))
        (custom-enabled-themes '(old-theme newer-theme))
        disabled
        loaded)
    (unwind-protect
        (progn
          (cj/theme-write-file-contents "nil" file)
          (let ((theme-file file))
            (cl-letf (((symbol-function 'disable-theme)
                       (lambda (theme) (push theme disabled)))
                      ((symbol-function 'load-theme)
                       (lambda (theme &optional _no-confirm _no-enable)
                         (push theme loaded))))
              (cj/load-theme-from-file)))
          (should (equal disabled '(newer-theme old-theme)))
          (should-not loaded))
      (delete-file file))))

(provide 'test-ui-theme-persistence)
;;; test-ui-theme-persistence.el ends here
