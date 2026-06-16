;;; test-dashboard-config-font-lock.el --- dashboard-mode excluded from global font-lock -*- lexical-binding: t; -*-

;;; Commentary:
;; `global-font-lock-mode' fontifies the *dashboard* buffer and strips the
;; manually-applied `face' text properties dashboard puts on the banner title
;; (`dashboard-banner-logo-title') and the section headings
;; (`dashboard-heading'), so they render in the default face instead of the
;; theme colors.  dashboard-config excludes dashboard-mode from global
;; font-lock so those text-property faces survive.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))

;; Stub package-level deps dashboard-config pulls transitively.
(unless (fboundp 'cj/kill-all-other-buffers-and-windows)
  (defun cj/kill-all-other-buffers-and-windows () nil))
(unless (fboundp 'cj/make-buffer-undead)
  (defun cj/make-buffer-undead (_name) nil))

(require 'dashboard-config)

(ert-deftest test-dashboard-config-excludes-dashboard-mode-from-global-font-lock ()
  "Normal: dashboard-mode is excluded from `font-lock-global-modes'.
Global font-lock must not run in the dashboard buffer, or it strips the
manual face text properties dashboard applies to the banner and headings."
  (should (consp font-lock-global-modes))
  (should (eq (car font-lock-global-modes) 'not))
  (should (memq 'dashboard-mode (cdr font-lock-global-modes))))

(provide 'test-dashboard-config-font-lock)
;;; test-dashboard-config-font-lock.el ends here
