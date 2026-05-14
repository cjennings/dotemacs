;;; test-ui-theme-commands.el --- Tests for ui-theme command wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-ui-theme-persistence.el' covers the read/write/load
;; pipeline.  This file covers the four remaining helpers:
;;
;;   cj/switch-themes
;;   cj/save-theme-to-file
;;   cj/get-active-theme-name
;;   cj/load-fallback-theme

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ui-theme)

;; Top-level defvars so let-bindings reach the dynamic var under
;; lexical scope.
(defvar custom-enabled-themes nil)

;;; cj/get-active-theme-name

(ert-deftest test-ui-theme-get-active-name-returns-first-enabled ()
  "Normal: with an enabled theme, the name of the first one is returned."
  (let ((custom-enabled-themes '(modus-vivendi tango)))
    (should (equal "modus-vivendi" (cj/get-active-theme-name)))))

(ert-deftest test-ui-theme-get-active-name-falls-back-when-none ()
  "Boundary: with no enabled themes, fallback-theme-name is returned."
  (let ((custom-enabled-themes nil)
        (fallback-theme-name "modus-vivendi"))
    (should (equal "modus-vivendi" (cj/get-active-theme-name)))))

;;; cj/save-theme-to-file

(ert-deftest test-ui-theme-save-writes-active-name-to-file ()
  "Normal: save-to-file writes the active theme name into theme-file."
  (let ((file (make-temp-file "ui-theme-save-"))
        (custom-enabled-themes '(tango-dark)))
    (unwind-protect
        (let ((theme-file file))
          (cl-letf (((symbol-function 'message) #'ignore))
            (cj/save-theme-to-file))
          (should (equal "tango-dark" (cj/theme-read-file-contents file))))
      (delete-file file))))

(ert-deftest test-ui-theme-save-reports-when-unwriteable ()
  "Error: when write fails, save-to-file reports via `message' and
does not raise."
  (let ((custom-enabled-themes '(tango-dark))
        (messaged nil))
    (cl-letf (((symbol-function 'cj/theme-write-file-contents)
               (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq messaged (apply #'format fmt args)))))
      (cj/save-theme-to-file))
    (should (string-match-p "Cannot save theme" messaged))))

;;; cj/load-fallback-theme

(ert-deftest test-ui-theme-load-fallback-disables-then-loads ()
  "Normal: load-fallback-theme disables all then loads the fallback."
  (let ((fallback-theme-name "modus-vivendi")
        (custom-enabled-themes '(old-one old-two))
        disabled loaded)
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled)))
              ((symbol-function 'load-theme)
               (lambda (theme &optional _no-confirm _no-enable)
                 (push theme loaded)))
              ((symbol-function 'message) #'ignore))
      (cj/load-fallback-theme "boom"))
    (should (equal (sort (copy-sequence disabled) #'string<) '(old-one old-two)))
    (should (equal loaded '(modus-vivendi)))))

;;; cj/switch-themes

(ert-deftest test-ui-theme-switch-disables-loads-then-saves ()
  "Normal: switch-themes routes through completing-read, disables enabled
themes, loads the chosen one, and saves the name to disk."
  (let ((file (make-temp-file "ui-theme-switch-"))
        (custom-enabled-themes '(currently-on))
        disabled loaded)
    (unwind-protect
        (let ((theme-file file))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "modus-vivendi"))
                    ((symbol-function 'custom-available-themes)
                     (lambda () '(modus-vivendi tango)))
                    ((symbol-function 'disable-theme)
                     (lambda (theme) (push theme disabled)))
                    ((symbol-function 'load-theme)
                     (lambda (theme &optional _no-confirm _no-enable)
                       (push theme loaded)
                       ;; Pretend the load updated the enabled list so
                       ;; `cj/get-active-theme-name' sees the new theme.
                       (setq custom-enabled-themes (list theme))))
                    ((symbol-function 'message) #'ignore))
            (cj/switch-themes))
          (should (equal disabled '(currently-on)))
          (should (equal loaded '(modus-vivendi)))
          (should (equal (cj/theme-read-file-contents file) "modus-vivendi")))
      (delete-file file))))

(provide 'test-ui-theme-commands)
;;; test-ui-theme-commands.el ends here
