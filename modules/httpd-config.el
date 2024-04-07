;;; httpd-config --- Setup for a Simple HTTP Server -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:


;;;; -------------------------- Simple-Httpd -------------------------

(use-package simple-httpd
  :defer 1
  :preface
  (defconst wwwdir (concat user-emacs-directory "www"))
  (defun check-or-create-wwwdir ()
    (unless (file-exists-p wwwdir)
      (make-directory wwwdir)))
  :init (check-or-create-wwwdir)
  :config
  (setq httpd-root wwwdir)
  (setq httpd-show-backtrace-when-error t)
  (setq httpd-serve-files t))


(provide 'httpd-config)
;;; httpd-config.el ends here
