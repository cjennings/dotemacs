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
;; - Events are managed by Org (changes in org file push back to Google Calendar)
;;   This is controlled by org-gcal-managed-newly-fetched-mode and
;;   org-gcal-managed-update-existing-mode set to "org"
;; - Automatic sync timer (configurable via cj/org-gcal-sync-interval-minutes)
;;   Default: 30 minutes, set to nil to disable
;;   See: https://github.com/kidd/org-gcal.el?tab=readme-ov-file#sync-automatically-at-regular-times
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
;; - Manual sync: C-; g s (or M-x org-gcal-sync)
;; - Toggle auto-sync on/off: C-; g t
;; - Restart auto-sync (e.g., after changing interval): C-; g r
;; - Clear sync lock (if sync gets stuck): C-; g c
;;
;; Note:
;; This configuration creates oauth2-auto.plist on first run to prevent sync errors.
;; Passphrase caching is enabled.
;;
;;; Code:

(require 'host-environment)
(require 'user-constants)

;; Forward declare org-gcal internal variables and functions
(eval-when-compile
  (defvar org-gcal--sync-lock))
(declare-function org-gcal-reload-client-id-secret "org-gcal")

;; User configurable sync interval
(defvar cj/org-gcal-sync-interval-minutes 30
  "Interval in minutes for automatic Google Calendar sync.
Set to nil to disable automatic syncing.
Changes take effect after calling `cj/org-gcal-restart-auto-sync'.")

;; Internal timer object
(defvar cj/org-gcal-sync-timer nil
  "Timer object for automatic org-gcal sync.
Use `cj/org-gcal-start-auto-sync' and `cj/org-gcal-stop-auto-sync' to control.")

(defun cj/org-gcal-clear-sync-lock ()
  "Clear the org-gcal sync lock.
Useful when a sync fails and leaves the lock in place, preventing future syncs."
  (interactive)
  (setq org-gcal--sync-lock nil)
  (message "org-gcal sync lock cleared"))

(defun cj/org-gcal-convert-all-to-org-managed ()
  "Convert all org-gcal events in current buffer to Org-managed.

Changes all events with org-gcal-managed property from `gcal' to `org',
enabling bidirectional sync so changes push back to Google Calendar."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^:org-gcal-managed: gcal$" nil t)
        (replace-match ":org-gcal-managed: org")
        (setq count (1+ count))))
    (when (> count 0)
      (save-buffer))
    (message "Converted %d event(s) to Org-managed" count)))

(defun cj/org-gcal-start-auto-sync ()
  "Start automatic Google Calendar sync timer.
Uses the interval specified in `cj/org-gcal-sync-interval-minutes'.
Does nothing if interval is nil or timer is already running."
  (interactive)
  (when (and cj/org-gcal-sync-interval-minutes
             (not (and cj/org-gcal-sync-timer
                       (memq cj/org-gcal-sync-timer timer-list))))
    (let ((interval-seconds (* cj/org-gcal-sync-interval-minutes 60)))
      (setq cj/org-gcal-sync-timer
            (run-with-timer
             120 ;; Initial delay: 2 minutes after startup
             interval-seconds
             (lambda ()
               (condition-case err
                   (org-gcal-sync)
                 (error (message "org-gcal: Auto-sync failed: %s" err))))))
      (message "org-gcal: Auto-sync started (every %d minutes)"
               cj/org-gcal-sync-interval-minutes))))

(defun cj/org-gcal-stop-auto-sync ()
  "Stop automatic Google Calendar sync timer."
  (interactive)
  (when (and cj/org-gcal-sync-timer
             (memq cj/org-gcal-sync-timer timer-list))
    (cancel-timer cj/org-gcal-sync-timer)
    (setq cj/org-gcal-sync-timer nil)
    (message "org-gcal: Auto-sync stopped")))

(defun cj/org-gcal-toggle-auto-sync ()
  "Toggle automatic Google Calendar sync timer on/off."
  (interactive)
  (if (and cj/org-gcal-sync-timer
           (memq cj/org-gcal-sync-timer timer-list))
      (cj/org-gcal-stop-auto-sync)
    (cj/org-gcal-start-auto-sync)))

(defun cj/org-gcal-restart-auto-sync ()
  "Restart automatic Google Calendar sync timer.
Useful after changing `cj/org-gcal-sync-interval-minutes'."
  (interactive)
  (cj/org-gcal-stop-auto-sync)
  (cj/org-gcal-start-auto-sync))

;; Deferred library required by org-gcal
(use-package deferred
  :ensure t)

;; OAuth2 authentication library required by org-gcal
(use-package oauth2-auto
  :ensure t)

(use-package org-gcal
  :vc (:url "https://github.com/cjennings/org-gcal" :rev :newest)
  :defer t ;; unless idle timer is set below

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

  ;; Google Calendar is authoritative - avoids sync conflicts
  (setq org-gcal-managed-newly-fetched-mode "gcal")    ;; New events from GCal stay GCal-managed
  (setq org-gcal-managed-update-existing-mode "gcal")  ;; GCal wins on conflicts

  :config
  ;; Plstore caching is now configured globally in auth-config.el
  ;; to ensure it loads before org-gcal needs it

  ;; set org-gcal timezone based on system timezone
  (setq org-gcal-local-timezone (cj/detect-system-timezone))

  ;; Reload client credentials (should already be loaded by org-gcal, but ensure it's set)
  (org-gcal-reload-client-id-secret)

  ;; Auto-save gcal files after sync completes
  (defun cj/org-gcal-save-files-after-sync (&rest _)
    "Save all org-gcal files after sync completes."
    (dolist (entry org-gcal-fetch-file-alist)
      (let* ((file (cdr entry))
             (buffer (get-file-buffer file)))
        (when (and buffer (buffer-modified-p buffer))
          (with-current-buffer buffer
            (save-buffer)
            (message "Saved %s after org-gcal sync" (file-name-nondirectory file)))))))

  ;; Advise org-gcal--sync-unlock which is called when sync completes
  (advice-add 'org-gcal--sync-unlock :after #'cj/org-gcal-save-files-after-sync))

;; Start automatic sync timer based on user configuration
;; Set cj/org-gcal-sync-interval-minutes to nil to disable
(cj/org-gcal-start-auto-sync)

;; Google Calendar keymap and keybindings
(defvar-keymap cj/gcal-map
  :doc "Keymap for Google Calendar operations"
  "s" #'org-gcal-sync
  "t" #'cj/org-gcal-toggle-auto-sync
  "r" #'cj/org-gcal-restart-auto-sync
  "c" #'cj/org-gcal-clear-sync-lock)
(keymap-set cj/custom-keymap "g" cj/gcal-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; g" "gcal menu"
    "C-; g s" "sync"
    "C-; g t" "toggle auto-sync"
    "C-; g r" "restart auto-sync"
    "C-; g c" "clear sync lock"))

(provide 'org-gcal-config)
;;; org-gcal-config.el ends here
