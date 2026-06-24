;;; httpd-config --- Setup for a Simple HTTP Server -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; local web server, a command-loaded deferral candidate.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none.
;; Direct test load: yes.
;;
;;; Code:


;;;; -------------------------- Simple-Httpd -------------------------

(use-package simple-httpd
  :defer 1
  :preface
  (defconst cj/httpd-wwwdir (concat user-emacs-directory "www"))
  (defun cj/httpd-check-or-create-wwwdir ()
    (unless (file-exists-p cj/httpd-wwwdir)
      (make-directory cj/httpd-wwwdir)))
  :init (cj/httpd-check-or-create-wwwdir)
  :config
  (setq httpd-root cj/httpd-wwwdir)
  (setq httpd-show-backtrace-when-error t)
  (setq httpd-serve-files t))


(provide 'httpd-config)
;;; httpd-config.el ends here
