;;; org-gcal-config.el --- Google Calendar synchronization for Org-mode -*- lexical-binding: t; coding: utf-8; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Bidirectional synchronization between Google Calendar and Org-mode using org-gcal.
;; - Credential management via authinfo.gpg
;; - Automatic archival of past events
;; - Automatic removal of cancelled events, but with TODOs added for visibility
;; - System timezone configuration via functions in host-environment
;; - No notifications on syncing
;; - Initial automatic sync post Emacs startup. No auto resync'ing.
;;   (my calendar doesn't change hourly and I want fewer distractions and slowdowns).
;;   if you need it: https://github.com/kidd/org-gcal.el?tab=readme-ov-file#sync-automatically-at-regular-times
;; - Validates existing oath2-auto.plist file or creates it to avoid the issue mentioned here:
;;   https://github.com/kidd/org-gcal.el?tab=readme-ov-file#note
;;
;; Prerequisites:
;; 1. Create OAuth 2.0 credentials in Google Cloud Console
;;    See: https://github.com/kidd/org-gcal.el?tab=readme-ov-file#installation
;; 2. Store credentials in ~/.authinfo.gpg with this format:
;;    machine org-gcal login YOUR_CLIENT_ID password YOUR_CLIENT_SECRET
;; 3. Define `gcal-file' in user-constants (location of org file to hold sync'd events).
;;
;; Usage:
;; - Manual sync: C-; g (or M-x org-gcal-sync)
;;
;; Note:
;; This configuration creates oauth2-auto.plist on first run to prevent sync errors.
;; Passphrase caching is enabled.
;;
;;; Code:

(require 'host-environment)
(require 'user-constants)

(defun cj/org-gcal-clear-sync-lock ()
  "Clear the org-gcal sync lock.
Useful when a sync fails and leaves the lock in place, preventing future syncs."
  (interactive)
  (setq org-gcal--sync-lock nil)
  (message "org-gcal sync lock cleared"))

(use-package org-gcal
  :defer t ;; unless idle timer is set below
  :bind (("C-; g" . org-gcal-sync)
         ("C-; G" . cj/org-gcal-clear-sync-lock))

  :init
  ;; Retrieve credentials from authinfo.gpg BEFORE package loads
  ;; This is critical - org-gcal checks these variables at load time
  (require 'auth-source)
  (let ((credentials (car (auth-source-search :host "org-gcal" :require '(:user :secret)))))
	(when credentials
	  (setq org-gcal-client-id (plist-get credentials :user))
	  ;; The secret might be a function, so we need to handle that
	  (let ((secret (plist-get credentials :secret)))
		(setq org-gcal-client-secret
			  (if (functionp secret)
				  (funcall secret)
				secret)))))

  ;; identify calendar to sync and it's destination
  (setq org-gcal-fetch-file-alist `(("craigmartinjennings@gmail.com" . ,gcal-file)))

  (setq org-gcal-up-days 30)                           ;; Look 30 days back
  (setq org-gcal-down-days 60)                         ;; Look 60 days forward
  (setq org-gcal-auto-archive t)                       ;; auto-archive old events
  (setq org-gcal-notify-p nil)                         ;; nil disables; t enables notifications
  (setq org-gcal-remove-api-cancelled-events t)        ;; auto-remove cancelled events
  (setq  org-gcal-update-cancelled-events-with-todo t) ;; todo cancelled events for visibility

  :config
  ;; Enable plstore passphrase caching after org-gcal loads
  (require 'plstore)
  (setq plstore-cache-passphrase-for-symmetric-encryption t)

  ;; set org-gcal timezone based on system timezone
  (setq org-gcal-local-timezone (cj/detect-system-timezone))

  ;; Reload client credentials (should already be loaded by org-gcal, but ensure it's set)
  (org-gcal-reload-client-id-secret))

;; Set up automatic initial sync on boot with error handling
;;(run-with-idle-timer
;; 2 nil
;; (lambda ()
;;   (condition-case err
;;	   (org-gcal-sync)
;;	 (error (message "org-gcal: Initial sync failed: %s" err)))))

(provide 'org-gcal-config)
;;; org-gcal-config.el ends here
