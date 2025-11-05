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

(eval-when-compile (require 'user-constants)) ;; defines authinfo-file location

;; -------------------------------- Auth Sources -------------------------------
;; auth sources settings

(use-package auth-source
  :ensure nil                           ;; built in
  :demand t                             ;; load this package immediately
  :config
  ;; USE gpg-agent for passphrase caching (400-day cache from gpg-agent.conf)
  ;; (setenv "GPG_AGENT_INFO" nil)      ;; DISABLED: was preventing gpg-agent cache
  (setq auth-sources `(,authinfo-file))  ;; use authinfo.gpg (see user-constants.el)
  (setq auth-source-debug t)             ;; echo debug info to Messages
  (setq auth-source-cache-expiry 86400)) ;; cache decrypted credentials for 24 hours

;; ----------------------------- Easy PG Assistant -----------------------------
;; Key management, cryptographic operations on regions and files, dired
;; integration, and automatic encryption/decryption of *.gpg files.

(use-package epa
  :ensure nil ;; built-in
  :demand t
  :config
  (epa-file-enable)
  ;; (setq epa-pinentry-mode 'loopback)  ;; emacs request passwords in minibuffer
  (setq epg-gpg-program "gpg2"))  ;; force use gpg2 (not gpg v.1)

;; ---------------------------------- Plstore ----------------------------------
;; Encrypted storage used by oauth2-auto for Google Calendar tokens.
;; CRITICAL: Enable passphrase caching to prevent password prompts every 10 min.

(use-package plstore
  :ensure nil ;; built-in
  :demand t
  :config
  ;; Cache passphrase indefinitely (relies on gpg-agent for actual caching)
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  ;; Allow gpg-agent to cache the passphrase (400 days per gpg-agent.conf)
  (setq plstore-encrypt-to nil)) ;; Use symmetric encryption, not key-based

(provide 'auth-config)
;;; auth-config.el ends here.
