;;; calendar-sync-source.el --- Feed fetch, state, and conversion workers -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-16

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/S.
;; Load shape: library.
;; Top-level side effects: none (defuns plus internal state defvars).
;; Runtime requires: subr-x, system-lib, calendar-sync-ics.
;; Direct test load: yes (requires calendar-sync-ics explicitly).
;;
;; Source layer of calendar-sync: per-calendar sync state and its on-disk
;; persistence, asynchronous .ics fetching via curl, the batch Emacs
;; conversion worker, and the Google Calendar API fetch path.  Drives a
;; single calendar from either its .ics feed or the API helper.
;;
;; The batch worker loads the top calendar-sync module (whose path is held
;; in `calendar-sync--module-file') and there calls `calendar-sync--parse-ics'
;; and `calendar-sync--write-file'.  Those live in the top and org modules
;; respectively, which require this one, so they are forward-declared here
;; rather than required (the worker has the full graph loaded, and these
;; functions are only ever invoked inside it).

;;; Code:

(require 'subr-x)
(require 'system-lib)  ;; provides cj/auth-source-secret-value
(require 'calendar-sync-ics)

;; Owned by calendar-sync.el (config) / calendar-sync-org.el (output);
;; forward-declared so this module compiles and reads them without a cycle.
(defvar calendar-sync-calendars)
(defvar calendar-sync-fetch-timeout)
(defvar calendar-sync-python-command)
(defvar calendar-sync-past-months)
(defvar calendar-sync-future-months)
(defvar calendar-sync-user-emails)
(defvar calendar-sync-skip-declined)
(defvar calendar-sync-private-config-file)
(defvar calendar-sync--module-file)
(declare-function calendar-sync--parse-ics "calendar-sync" (ics-content))
(declare-function calendar-sync--write-file "calendar-sync-org" (content file))

;;; Internal state

(defvar calendar-sync--calendar-states (make-hash-table :test 'equal)
  "Per-calendar sync state.
Hash table mapping calendar name (string) to state plist with:
  :last-sync  - Time of last successful sync
  :status     - Symbol: ok, error, or syncing
  :last-error - Error message string, or nil")

(defvar calendar-sync--state-file
  (expand-file-name "persist/calendar-sync-state.el" user-emacs-directory)
  "File to persist sync state across Emacs sessions.")

;;; State Persistence

(defun calendar-sync--save-state ()
  "Save sync state to disk for persistence across sessions."
  (let* ((calendar-states-alist
          (let ((result '()))
            (maphash (lambda (name state)
                       (push (cons name state) result))
                     calendar-sync--calendar-states)
            result))
         (state `((timezone-offset . ,calendar-sync--last-timezone-offset)
                  (calendar-states . ,calendar-states-alist)))
         (dir (file-name-directory calendar-sync--state-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((tmp (make-temp-file (expand-file-name ".calendar-sync-state-" dir))))
      (with-temp-file tmp
        (prin1 state (current-buffer)))
      (rename-file tmp calendar-sync--state-file t))))

(defun calendar-sync--load-state ()
  "Load sync state from disk."
  (when (file-exists-p calendar-sync--state-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents calendar-sync--state-file)
          (let ((state (read (current-buffer))))
            (setq calendar-sync--last-timezone-offset
                  (alist-get 'timezone-offset state))
            ;; Load per-calendar states
            (let ((cal-states (alist-get 'calendar-states state)))
              (clrhash calendar-sync--calendar-states)
              (dolist (entry cal-states)
                (let ((st (cdr entry)))
                  ;; A persisted `syncing' status is stale in a fresh process
                  ;; (no sync is actually running), so reset it; otherwise the
                  ;; in-flight guard would skip that calendar forever.
                  (when (eq (plist-get st :status) 'syncing)
                    (setq st (plist-put (copy-sequence st) :status 'never)))
                  (puthash (car entry) st calendar-sync--calendar-states))))))
      (error
       (calendar-sync--log-silently "calendar-sync: Error loading state: %s" (error-message-string err))))))

(defun calendar-sync--get-calendar-state (calendar-name)
  "Get state plist for CALENDAR-NAME, or nil if not found."
  (gethash calendar-name calendar-sync--calendar-states))

(defun calendar-sync--syncing-p (calendar-name)
  "Return non-nil when CALENDAR-NAME has an in-flight sync.
Used to skip an overlapping sync when a timer tick fires while the previous
sync for the same calendar is still running."
  (eq (plist-get (calendar-sync--get-calendar-state calendar-name) :status)
      'syncing))

(defun calendar-sync--set-calendar-state (calendar-name state)
  "Set STATE plist for CALENDAR-NAME."
  (puthash calendar-name state calendar-sync--calendar-states))

;;; Debug Logging

(defun calendar-sync--debug-p ()
  "Return non-nil if calendar-sync debug logging is enabled.
Checks `cj/debug-modules' for symbol `calendar-sync' or t (all)."
  (and (boundp 'cj/debug-modules)
       (or (eq cj/debug-modules t)
           (memq 'calendar-sync cj/debug-modules))))

;;; Private Config

(defun calendar-sync--load-private-config ()
  "Load private calendar-sync configuration when available."
  (when (file-readable-p calendar-sync-private-config-file)
    (condition-case err
        (load calendar-sync-private-config-file nil t)
      (error
       (message "calendar-sync: Failed to load private config %s: %s"
                (abbreviate-file-name calendar-sync-private-config-file)
                (error-message-string err))))))

;;; .ics Fetch

(defun calendar-sync--fetch-ics (url callback)
  "Fetch .ics file from URL asynchronously using curl.
Calls CALLBACK with the .ics content as string (normalized to Unix line endings)
or nil on error. CALLBACK signature: (lambda (content) ...).

The fetch happens asynchronously and doesn't block Emacs. The callback is
invoked when the fetch completes, either successfully or with an error."
  (condition-case err
      (let ((buffer (generate-new-buffer " *calendar-sync-curl*")))
        (make-process
         :name "calendar-sync-curl"
         :buffer buffer
         :command (list "curl" "-s" "-L" "--fail"
                        "--connect-timeout" "10"
                        "--max-time" (number-to-string calendar-sync-fetch-timeout)
                        url)
         :sentinel
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (let ((buf (process-buffer process)))
               (when (buffer-live-p buf)
                 (let ((content
                        (with-current-buffer buf
                          (if (and (eq (process-status process) 'exit)
                                   (= (process-exit-status process) 0))
                              (calendar-sync--normalize-line-endings (buffer-string))
                            (calendar-sync--log-silently "calendar-sync: Fetch error: curl failed: %s" (string-trim event))
                            nil))))
                   (kill-buffer buf)
                   (funcall callback content))))))))
    (error
     (calendar-sync--log-silently "calendar-sync: Fetch error: %s" (error-message-string err))
     (funcall callback nil))))

(defun calendar-sync--fetch-ics-file (url callback)
  "Fetch .ics from URL to a temp file asynchronously.
Calls CALLBACK with the temp file path on success, or nil on error.  The caller
owns deleting the temp file after a successful callback."
  (condition-case err
      (let ((buffer (generate-new-buffer " *calendar-sync-curl*"))
            (temp-file (make-temp-file "calendar-sync-" nil ".ics")))
        (make-process
         :name "calendar-sync-curl"
         :buffer buffer
         :command (list "curl" "-s" "-L" "--fail"
                        "--connect-timeout" "10"
                        "--max-time" (number-to-string calendar-sync-fetch-timeout)
                        "-o" temp-file
                        url)
         :sentinel
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (let ((buf (process-buffer process))
                   (success (and (eq (process-status process) 'exit)
                                 (= (process-exit-status process) 0))))
               (when (buffer-live-p buf)
                 (unless success
                   (calendar-sync--log-silently "calendar-sync: Fetch error: curl failed: %s"
                                                (string-trim event)))
                 (kill-buffer buf))
               (if success
                   (funcall callback temp-file)
                 (when (file-exists-p temp-file)
                   (delete-file temp-file))
                 (funcall callback nil)))))))
    (error
     (calendar-sync--log-silently "calendar-sync: Fetch error: %s" (error-message-string err))
     (funcall callback nil))))

;;; Batch Conversion Worker

(defun calendar-sync--emacs-binary ()
  "Return the Emacs executable to use for calendar conversion workers."
  (let ((candidate (expand-file-name invocation-name invocation-directory)))
    (if (file-executable-p candidate)
        candidate
      invocation-name)))

(defun calendar-sync--batch-convert-file (ics-file output-file past-months future-months user-emails)
  "Convert ICS-FILE to Org format and write OUTPUT-FILE.
PAST-MONTHS, FUTURE-MONTHS, and USER-EMAILS mirror the interactive session's
calendar conversion settings.  This is intended for noninteractive worker
processes, not direct interactive use."
  (setq calendar-sync-past-months past-months
        calendar-sync-future-months future-months
        calendar-sync-user-emails user-emails)
  (let* ((ics-content
          (with-temp-buffer
            (insert-file-contents ics-file)
            (calendar-sync--normalize-line-endings (buffer-string))))
         (org-content (calendar-sync--parse-ics ics-content)))
    (unless org-content
      (error "calendar-sync: parse failed"))
    (calendar-sync--write-file org-content output-file)))

(defun calendar-sync--worker-command (ics-file output-file)
  "Build the batch Emacs command that converts ICS-FILE to OUTPUT-FILE."
  (let ((module-dir (file-name-directory calendar-sync--module-file))
        (private-config-file
         (make-temp-name (expand-file-name "calendar-sync-worker-config-"
                                           temporary-file-directory)))
        (state-file
         (make-temp-name (expand-file-name "calendar-sync-worker-state-"
                                           temporary-file-directory))))
    (list (calendar-sync--emacs-binary)
          "--batch"
          "--no-site-file"
          "--no-site-lisp"
          "--eval" (format "(setq load-prefer-newer t calendar-sync-auto-start nil calendar-sync-private-config-file %S calendar-sync--state-file %S)"
                           private-config-file state-file)
          "-L" module-dir
          "-l" calendar-sync--module-file
          "--eval" (format "(calendar-sync--batch-convert-file %S %S %S %S '%S)"
                           ics-file
                           output-file
                           calendar-sync-past-months
                           calendar-sync-future-months
                           calendar-sync-user-emails))))

(defun calendar-sync--convert-ics-file-async (ics-file output-file callback)
  "Convert ICS-FILE to OUTPUT-FILE in a batch Emacs worker.
Calls CALLBACK as (CALLBACK SUCCESS ERROR-MESSAGE).  Deletes ICS-FILE after the
worker exits."
  (condition-case err
      (let ((buffer (generate-new-buffer " *calendar-sync-worker*")))
        (make-process
         :name "calendar-sync-worker"
         :buffer buffer
         :command (calendar-sync--worker-command ics-file output-file)
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (let* ((buf (process-buffer process))
                    (success (and (eq (process-status process) 'exit)
                                  (= (process-exit-status process) 0)))
                    (error-message
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (string-trim (buffer-string))))))
               (when (file-exists-p ics-file)
                 (delete-file ics-file))
               (when (buffer-live-p buf)
                 (kill-buffer buf))
               (funcall callback success error-message))))))
    (error
     (when (file-exists-p ics-file)
       (delete-file ics-file))
     (funcall callback nil (error-message-string err)))))

(defun calendar-sync--mark-sync-failed (name reason)
  "Record failed sync state for calendar NAME with REASON."
  (calendar-sync--set-calendar-state
   name
   (list :status 'error
         :last-sync (plist-get (calendar-sync--get-calendar-state name) :last-sync)
         :last-error reason))
  (calendar-sync--save-state)
  (message "calendar-sync: [%s] Sync failed (see *Messages*)" name))

;;; Google Calendar API Fetch Path

(defun calendar-sync--api-script ()
  "Return the absolute path to the Google Calendar API helper script.
Resolved relative to this module so batch workers and tests don't depend
on `user-emacs-directory'."
  (let ((module-dir (file-name-directory calendar-sync--module-file)))
    (expand-file-name "calendar_sync_api.py"
                      (expand-file-name "scripts"
                                        (file-name-parent-directory module-dir)))))

(defun calendar-sync--api-command (account calendar-id output-file)
  "Build the command list that runs the API helper.
ACCOUNT and CALENDAR-ID select the OAuth account and calendar; OUTPUT-FILE
is where the helper writes rendered org content.  The past/future window
mirrors the .ics path's `calendar-sync-past-months' /
`calendar-sync-future-months'.  When `calendar-sync-skip-declined' is nil,
passes --keep-declined so the API path honors the same toggle."
  (append
   (list calendar-sync-python-command
         (calendar-sync--api-script)
         "--account" account
         "--calendar-id" calendar-id
         "--output" output-file
         "--past-months" (number-to-string calendar-sync-past-months)
         "--future-months" (number-to-string calendar-sync-future-months))
   (unless calendar-sync-skip-declined
     (list "--keep-declined"))))

(defun calendar-sync--sync-calendar-api (calendar)
  "Sync a single Google CALENDAR via the API helper script.
CALENDAR is a plist with :name, :account, :calendar-id, and :file keys.
The helper fetches, filters, and renders org in one pass and writes :file
directly, so it runs in a single external process off the interactive thread."
  (let* ((name (plist-get calendar :name))
         (account (plist-get calendar :account))
         (calendar-id (plist-get calendar :calendar-id))
         (file (plist-get calendar :file))
         (fetch-start (float-time)))
    (calendar-sync--set-calendar-state name '(:status syncing))
    (calendar-sync--log-silently "calendar-sync: [%s] Syncing (API)..." name)
    (condition-case err
        (let ((buffer (generate-new-buffer " *calendar-sync-api*")))
          (make-process
           :name "calendar-sync-api"
           :buffer buffer
           :command (calendar-sync--api-command account calendar-id file)
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let* ((buf (process-buffer process))
                      (success (and (eq (process-status process) 'exit)
                                    (= (process-exit-status process) 0)))
                      (output (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (string-trim (buffer-string))))))
                 (when (buffer-live-p buf)
                   (kill-buffer buf))
                 (if (not success)
                     (calendar-sync--mark-sync-failed
                      name (if (or (null output) (string-empty-p output))
                               "API helper failed"
                             output))
                   (calendar-sync--set-calendar-state
                    name
                    (list :status 'ok
                          :last-sync (current-time)
                          :last-error nil))
                   (setq calendar-sync--last-timezone-offset
                         (calendar-sync--current-timezone-offset))
                   (calendar-sync--save-state)
                   (let ((total-elapsed (- (float-time) fetch-start)))
                     (message "calendar-sync: [%s] Sync complete (%.1fs total) → %s"
                              name total-elapsed file))))))))
      (error
       (calendar-sync--log-silently "calendar-sync: [%s] API helper error: %s"
                                    name (error-message-string err))
       (calendar-sync--mark-sync-failed name (error-message-string err))))))

;;; .ics Sync Path

(defun calendar-sync--calendar-url (calendar)
  "Return the .ics feed URL for CALENDAR, or nil if none is configured.
An explicit :url wins.  Otherwise :secret-host names an auth-source host
whose stored secret is the URL (kept in auth-source because the .ics URL
is itself a token)."
  (or (plist-get calendar :url)
      (when-let* ((host (plist-get calendar :secret-host)))
        (cj/auth-source-secret-value host))))

(defun calendar-sync--sync-calendar-ics (calendar)
  "Sync a single CALENDAR from its .ics feed asynchronously.
CALENDAR is a plist with :name, :file, and a feed URL resolved by
`calendar-sync--calendar-url' (an explicit :url, or a :secret-host
looked up in auth-source)."
  (let ((name (plist-get calendar :name))
        (url (calendar-sync--calendar-url calendar))
        (file (plist-get calendar :file))
        (fetch-start (float-time)))
    (calendar-sync--set-calendar-state name '(:status syncing))
    (calendar-sync--log-silently "calendar-sync: [%s] Syncing..." name)
    (calendar-sync--fetch-ics-file
     url
     (lambda (ics-file)
       (let ((fetch-elapsed (- (float-time) fetch-start)))
         (if (null ics-file)
             (progn
               (calendar-sync--log-silently "calendar-sync: [%s] Fetch failed" name)
               (calendar-sync--mark-sync-failed name "Fetch failed"))
           (when (calendar-sync--debug-p)
             (calendar-sync--log-silently "calendar-sync: [%s] Fetched in %.1fs"
                                          name fetch-elapsed))
           (calendar-sync--convert-ics-file-async
            ics-file
            file
            (lambda (success error-message)
              (if (not success)
                  (progn
                    (calendar-sync--log-silently "calendar-sync: [%s] Conversion failed: %s"
                                                name error-message)
                    (calendar-sync--mark-sync-failed
                     name
                     (if (or (null error-message)
                             (string-empty-p error-message))
                         "Conversion failed"
                       error-message)))
                (calendar-sync--set-calendar-state
                 name
                 (list :status 'ok
                       :last-sync (current-time)
                       :last-error nil))
                (setq calendar-sync--last-timezone-offset
                      (calendar-sync--current-timezone-offset))
                (calendar-sync--save-state)
                (let ((total-elapsed (- (float-time) fetch-start)))
                  (message "calendar-sync: [%s] Sync complete (%.1fs total) → %s"
                           name total-elapsed file)))))))))))

(provide 'calendar-sync-source)
;;; calendar-sync-source.el ends here
