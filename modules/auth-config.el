;; auth-config.el --- Configuration for Authentication Utilities -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Configuration for Emacs authentication and GPG integration:

;; • auth-source
;;   – Forces use of your default authinfo file
;;   – Disable external GPG agent in favor of Emacs's own prompt
;;   – Enable auth-source debug messages

;; • Easy PG Assistant (epa)
;;   – Force using the 'gpg2' executable for encryption/decryption operations

;; • oauth2-auto cache fix (via advice)
;;   – oauth2-auto version 20250624.1919 has caching bug on line 206
;;   – Function oauth2-auto--plstore-read has `or nil` disabling cache
;;   – This caused GPG passphrase prompts every ~15 minutes during gcal-sync
;;   – Fix: Advice to enable hash-table cache without modifying package
;;   – Works across package updates
;;   – Fixed 2025-11-11

;;; Code:

(require 'system-lib)
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
  (setq epg-gpg-program "gpg2")  ;; force use gpg2 (not gpg v.1)

  ;; Update gpg-agent with current DISPLAY environment
  ;; This ensures pinentry can open GUI windows when Emacs starts
  (call-process "gpg-connect-agent" nil nil nil "updatestartuptty" "/bye"))

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

;; ----------------------------- oauth2-auto Cache Fix -----------------------------
;; Fix oauth2-auto caching bug that causes repeated GPG passphrase prompts.
;; The package has `or nil` on line 206 that disables its internal cache.
;; This advice overrides the buggy function to enable caching properly.

(defun cj/oauth2-auto--plstore-read-fixed (username provider)
  "Fixed version of oauth2-auto--plstore-read that enables caching.

This is a workaround for oauth2-auto.el bug where line 206 has:
  (or nil ;(gethash id oauth2-auto--plstore-cache)
which completely disables the internal hash-table cache.

This function re-implements the intended behavior with cache enabled."
  (require 'oauth2-auto)  ; Ensure package is loaded
  (let ((id (oauth2-auto--compute-id username provider)))
    ;; Check cache FIRST (this is what the original should do)
    (or (gethash id oauth2-auto--plstore-cache)
        ;; Cache miss - read from plstore and cache the result
        (let ((plstore (plstore-open oauth2-auto-plstore)))
          (unwind-protect
              (puthash id
                       (cdr (plstore-get plstore id))
                       oauth2-auto--plstore-cache)
            (plstore-close plstore))))))

;; Apply the fix via advice (survives package updates)
(with-eval-after-load 'oauth2-auto
  (advice-add 'oauth2-auto--plstore-read :override #'cj/oauth2-auto--plstore-read-fixed)
  (cj/log-silently "✓ oauth2-auto cache fix applied via advice"))

;; ------------------------ Authentication Reset Utility -----------------------

(defun cj/reset-auth-cache (&optional include-gpg-agent)
  "Reset authentication caches when wrong password was entered.

By default, only clears Emacs-side caches (auth-source, EPA file
handler) and leaves gpg-agent's long-term cache intact.  This preserves
your 400-day cache for GPG and SSH passphrases.

With prefix argument INCLUDE-GPG-AGENT (\\[universal-argument]), also
clears gpg-agent's password cache.  Use this when gpg-agent itself has
cached an incorrect password.

Clears:
1. auth-source cache (Emacs-level credential cache)
2. EPA file handler cache (encrypted file cache)
3. gpg-agent cache (only if INCLUDE-GPG-AGENT is non-nil)

Use this when you see errors like:
  - \"Bad session key\"
  - \"Decryption failed\"
  - GPG repeatedly using wrong cached password"
  (interactive "P")
  (message "Resetting authentication caches...")

  ;; Clear auth-source cache (Emacs credential cache)
  (auth-source-forget-all-cached)

  ;; Clear EPA file handler cache
  (when (fboundp 'epa-file-clear-cache)
    (epa-file-clear-cache))

  ;; Only clear gpg-agent cache if explicitly requested
  (if include-gpg-agent
      (let ((result (shell-command "echo RELOADAGENT | gpg-connect-agent")))
        (if (zerop result)
            (message "✓ Emacs and gpg-agent caches cleared. Next access will prompt for password.")
          (message "⚠ Warning: Failed to clear gpg-agent cache")))
    (message "✓ Emacs caches cleared. GPG/SSH passphrases preserved for session.")))

(defun cj/kill-gpg-agent ()
  "Force kill gpg-agent (it will restart automatically on next use).

This is a more aggressive reset than `cj/reset-auth-cache'.  Use this
when gpg-agent is stuck or behaving incorrectly.

The gpg-agent will automatically restart on the next GPG operation."
  (interactive)
  (let ((result (shell-command "gpgconf --kill gpg-agent")))
    (if (zerop result)
        (message "✓ gpg-agent killed. It will restart automatically on next use.")
      (message "⚠ Warning: Failed to kill gpg-agent"))))

(defun cj/clear-oauth2-auto-cache ()
  "Clear the oauth2-auto in-memory token cache.

This forces oauth2-auto to re-read tokens from oauth2-auto.plist on next
access.  Useful when OAuth tokens have been manually updated or after
re-authentication.

Note: This only clears Emacs's in-memory cache.  The oauth2-auto.plist
file on disk is not modified."
  (interactive)
  (if (boundp 'oauth2-auto--plstore-cache)
      (progn
        (clrhash oauth2-auto--plstore-cache)
        (message "✓ oauth2-auto token cache cleared"))
    (message "⚠ oauth2-auto not loaded yet")))

;; Keybindings
(with-eval-after-load 'keybindings
  (keymap-set cj/custom-keymap "A" #'cj/reset-auth-cache))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; A" "reset auth cache"))

(provide 'auth-config)
;;; auth-config.el ends here.
