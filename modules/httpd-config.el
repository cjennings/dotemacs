;;; httpd-config.el --- Setup for a Simple HTTP Server -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: deferred.
;; Defer reason: impatient-mode requires simple-httpd on demand; nothing
;; needs the server (or its www/ root) at startup.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none.
;; Direct test load: yes.
;;
;;; Code:


;;;; -------------------------- Simple-Httpd -------------------------

(use-package simple-httpd
  :defer t
  :preface
  (defconst cj/httpd-wwwdir (concat user-emacs-directory "www"))
  (defun cj/httpd-check-or-create-wwwdir ()
    (unless (file-exists-p cj/httpd-wwwdir)
      (make-directory cj/httpd-wwwdir)))
  :config
  ;; Create the doc root only when the server package actually loads.
  (cj/httpd-check-or-create-wwwdir)
  (setq httpd-root cj/httpd-wwwdir)
  (setq httpd-show-backtrace-when-error t)
  (setq httpd-serve-files t))


(provide 'httpd-config)
;;; httpd-config.el ends here
