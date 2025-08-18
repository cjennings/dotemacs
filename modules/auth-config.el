;; auth-config.el --- Configuration for Authentication Utilities -*- lexical-binding: t; -*-
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
  :ensure nil                   ;; built in
  :demand t                     ;; load this package immediately
  :config
  (setq auth-sources `(,authinfo-file))
  (setenv "GPG_AGENT_INFO" nil) ;; emacs use internal prompt, not gpg agent
  (setq auth-source-debug t))   ;; echo debug info to Messages

;; ----------------------------- Easy PG Assistant -----------------------------
;; Key management, cryptographic operations on regions and files, dired
;; integration, and automatic encryption/decryption of *.gpg files.

(use-package epa
  :ensure nil                    ;; built-in
  :defer .5
  :config
  (setq epg-gpg-program "gpg2")) ;; force use gpg2 (not gpg v.1)

;; ----------------------------- Ensure-Auth-Before ----------------------------

(defun cj/ensure-auth-before (&rest _args)
  "Ensure .authinfo.gpg is unlocked before calling the real function."
  (cj/ensure-auth))

(defun cj/ensure-auth ()
  "Make sure .authinfo.gpg is decrypted (loops on failure)."
  (interactive)
  (auth-source-search :max 1))

(with-eval-after-load 'auth-source
  (defun cj/auth-source-search-retry (orig-fun &rest args)
    "Advice around `auth-source-search' to loop until we get non-nil."
    (let (res)
      (while (not (setq res (apply orig-fun args)))
        ;; user hit RET or wrong passphrase → kill agent & retry
        (message "Auth failed or cancelled; killing gpg-agent and retrying…")
        (start-process "gpgconf-kill-gpg-agent" nil
                       "gpgconf" "--kill" "gpg-agent")
        (sleep-for 0.5))
      res))

  (advice-add 'auth-source-search :around #'cj/auth-source-search-retry))

;; Example: run it before your GPT toggle
;;(advice-add 'cj/toggle-gptel :before #'cj/ensure-auth-before)

;; Example: before mu4e actually sends a message
;;(advice-add 'smtpmail-send-it    ; or `mu4e~proc-send` if you prefer
;;            :before #'cj/ensure-auth-before)

;; Example: before Tramp prompts for a password
;; (advice-add 'tramp-read-passwd   ; wherever Tramp reads your passphrase
;;             :before #'cj/ensure-auth-before)

;; ;; Example: before Dirvish opens a remote directory
;; (advice-add 'dirvish-find-file    ; or the exact entry-point you use
;;             :before #'cj/ensure-auth-before)


(provide 'auth-config)
;;; auth-config.el ends here.

;; --------------------------------- ERT Tests ---------------------------------
;; Run these tests with M-x ert RET t RET

(require 'ert)
(require 'cl-lib)

(ert-deftest auth-config/authinfo-file-exists ()
  "Verify that `authinfo-file` actually exists on disk."
  (should (and (stringp authinfo-file)
               (file-exists-p authinfo-file))))

(ert-deftest auth-config/gpg2-is-on-path ()
  "Verify that the `gpg2` executable is on the user’s PATH."
  (should (executable-find "gpg2")))
