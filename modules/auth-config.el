;; auth-config.el --- Configuration for Authentication Utilities -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Configuration for Emacs authentication and GPG integration:

;; • auth-source
;;   – Forces use of your default authinfo file
;;   – Disable external GPG agent in favor of Emacs’s own prompt
;;   – Enable auth-source debug messages

;; • Easy PG Assistant (epa)
;;   – Force using the ‘gpg2’ executable for encryption/decryption operations

;;; Code:

(require 'user-constants) ;; defines authinfo-file location

;; -------------------------------- Auth Sources -------------------------------
;; auth sources settings

(use-package auth-source
  :ensure nil                           ;; built in
  :demand t                             ;; load this package immediately
  :config
  (setenv "GPG_AGENT_INFO" nil)         ;; disassociate with external gpg agent
  (setq auth-sources `(,authinfo-file)) ;; use authinfo.gpg (see user-constants.el)
  (setq auth-source-debug t))           ;; echo debug info to Messages

;; ----------------------------- Easy PG Assistant -----------------------------
;; Key management, cryptographic operations on regions and files, dired
;; integration, and automatic encryption/decryption of *.gpg files.

(use-package epa
  :ensure nil ;; built-in
  :demand t
  :config
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)  ;; emacs request passwords in minibuffer
  (setq epg-gpg-program "gpg2"))  ;; force use gpg2 (not gpg v.1)


(provide 'auth-config)
;;; auth-config.el ends here.
