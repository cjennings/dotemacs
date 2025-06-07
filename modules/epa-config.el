;;; epa-config.el --- EasyPG Configuration -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;

;;; Code:

;; -------------------------------- Auth Sources -------------------------------
;; auth sources settings

(use-package auth-source
  :ensure nil ;; built in
  :demand t ;; load this package early
  :config
  (setq auth-sources `(,authinfo-file))
  (setenv "GPG_AGENT_INFO" nil) ;; emacs use internal prompt, not gpg agent
  (setq auth-source-debug t))    ;; echo debug info to Messages

;; ----------------------------- Easy PG Assistant -----------------------------
;; Key management, cryptographic operations on regions and files, dired
;; integration, and automatic encryption/decryption of *.gpg files.

(use-package epa
  :ensure nil ;; built-in
  :defer .5
  :config
  (setq epg-gpg-program "gpg2")) ;; force use gpg2 (not gpg v.1)

(provide 'epa-config)
;;; epa-config.el ends here.
