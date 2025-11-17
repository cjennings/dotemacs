;;; calendar-sync.el --- Simple Google Calendar sync via .ics  -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-16

;;; Commentary:

;; Simple, reliable one-way sync from Google Calendar to Org mode.
;; Downloads .ics file from Google Calendar private URL and converts
;; to Org format. No OAuth, no API complexity, just file conversion.
;;
;; Features:
;; - Pure Emacs Lisp .ics parser (no external dependencies)
;; - Timer-based automatic sync (every 15 minutes, configurable)
;; - Self-contained in .emacs.d (no cron, portable across machines)
;; - Read-only (can't corrupt Google Calendar)
;; - Works with chime.el for event notifications
;;
;; Setup:
;; 1. Get your Google Calendar private .ics URL:
;;    - Open Google Calendar → Settings → Your Calendar → Integrate calendar
;;    - Copy the "Secret address in iCal format" URL
;;
;; 2. Configure in your init.el:
;;    (setq calendar-sync-ics-url "https://calendar.google.com/calendar/ical/YOUR_PRIVATE_URL/basic.ics")
;;    (require 'calendar-sync)
;;    (calendar-sync-start)
;;
;; 3. Add to org-agenda (optional):
;;    (add-to-list 'org-agenda-files calendar-sync-file)
;;
;; Usage:
;; - M-x calendar-sync-now    ; Manual sync
;; - M-x calendar-sync-start  ; Start auto-sync
;; - M-x calendar-sync-stop   ; Stop auto-sync
;; - M-x calendar-sync-toggle ; Toggle auto-sync

;;; Code:

(require 'org)
(require 'user-constants)  ; For gcal-file path

;;; Configuration

(defvar calendar-sync-ics-url nil
  "Google Calendar private .ics URL.
Get this from Google Calendar Settings → Integrate calendar → Secret address in iCal format.")

(defvar calendar-sync-interval-minutes 60
  "Sync interval in minutes.
Default: 60 minutes (1 hour).")

(defvar calendar-sync-file gcal-file
  "Location of synced calendar file.
Defaults to gcal-file from user-constants.")

(defvar calendar-sync-auto-start t
  "Whether to automatically start calendar sync when module loads.
If non-nil, sync starts automatically when calendar-sync is loaded.
If nil, user must manually call `calendar-sync-start'.")

;;; Internal state

(defvar calendar-sync--timer nil
  "Timer object for automatic syncing.")

(defvar calendar-sync--last-sync-time nil
  "Time of last successful sync.")

(defvar calendar-sync--last-error nil
  "Last sync error message, if any.")

(defvar calendar-sync--last-timezone-offset nil
  "Timezone offset in seconds from UTC at last sync.
Used to detect timezone changes (e.g., when traveling).")

(defvar calendar-sync--state-file
  (expand-file-name "data/calendar-sync-state.el" user-emacs-directory)
  "File to persist sync state across Emacs sessions.")

;;; Timezone Detection

(defun calendar-sync--current-timezone-offset ()
  "Get current timezone offset in seconds from UTC.
Returns negative for west of UTC, positive for east.
Example: -21600 for CST (UTC-6), -28800 for PST (UTC-8)."
  (car (current-time-zone)))

(defun calendar-sync--timezone-name ()
  "Get human-readable timezone name.
Returns string like 'CST' or 'PST'."
  (cadr (current-time-zone)))

(defun calendar-sync--format-timezone-offset (offset)
  "Format timezone OFFSET (in seconds) as human-readable string.
Example: -21600 → 'UTC-6' or 'UTC-6:00'."
  (if (null offset)
      "unknown"
    (let* ((hours (/ offset 3600))
           (minutes (abs (mod (/ offset 60) 60)))
           (sign (if (>= hours 0) "+" "-"))
           (abs-hours (abs hours)))
      (if (= minutes 0)
          (format "UTC%s%d" sign abs-hours)
        (format "UTC%s%d:%02d" sign abs-hours minutes)))))

(defun calendar-sync--timezone-changed-p ()
  "Return t if timezone has changed since last sync."
  (and calendar-sync--last-timezone-offset
       (not (= (calendar-sync--current-timezone-offset)
               calendar-sync--last-timezone-offset))))

;;; State Persistence

(defun calendar-sync--save-state ()
  "Save sync state to disk for persistence across sessions."
  (let ((state `((timezone-offset . ,calendar-sync--last-timezone-offset)
                 (last-sync-time . ,calendar-sync--last-sync-time)))
        (dir (file-name-directory calendar-sync--state-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (with-temp-file calendar-sync--state-file
      (prin1 state (current-buffer)))))

(defun calendar-sync--load-state ()
  "Load sync state from disk."
  (when (file-exists-p calendar-sync--state-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents calendar-sync--state-file)
          (let ((state (read (current-buffer))))
            (setq calendar-sync--last-timezone-offset
                  (alist-get 'timezone-offset state))
            (setq calendar-sync--last-sync-time
                  (alist-get 'last-sync-time state))))
      (error
       (message "calendar-sync: Error loading state: %s" (error-message-string err))))))

;;; Line Ending Normalization

(defun calendar-sync--normalize-line-endings (content)
  "Normalize line endings in CONTENT to Unix format (LF only).
Removes all carriage return characters (\\r) from CONTENT.
The iCalendar format (RFC 5545) uses CRLF line endings, but Emacs
and org-mode expect LF only. This function ensures consistent line
endings throughout the parsing pipeline.

Returns CONTENT with all \\r characters removed."
  (if (not (stringp content))
      content
    (replace-regexp-in-string "\r" "" content)))

;;; .ics Parsing

(defun calendar-sync--split-events (ics-content)
  "Split ICS-CONTENT into individual VEVENT blocks.
Returns list of strings, each containing one VEVENT block."
  (let ((events '())
        (start 0))
    (while (string-match "BEGIN:VEVENT\\(.\\|\n\\)*?END:VEVENT" ics-content start)
      (push (match-string 0 ics-content) events)
      (setq start (match-end 0)))
    (nreverse events)))

(defun calendar-sync--get-property (event property)
  "Extract PROPERTY value from EVENT string.
Returns nil if property not found."
  (when (string-match (format "^%s:\\(.*\\)$" property) event)
    (match-string 1 event)))

(defun calendar-sync--convert-utc-to-local (year month day hour minute second)
  "Convert UTC datetime to local time.
Returns list (year month day hour minute) in local timezone."
  (let* ((utc-time (encode-time second minute hour day month year 0))
         (local-time (decode-time utc-time)))
    (list (nth 5 local-time)  ; year
          (nth 4 local-time)  ; month
          (nth 3 local-time)  ; day
          (nth 2 local-time)  ; hour
          (nth 1 local-time)))) ; minute

(defun calendar-sync--parse-timestamp (timestamp-str)
  "Parse iCal timestamp string TIMESTAMP-STR.
Returns (year month day hour minute) or (year month day) for all-day events.
Converts UTC times (ending in Z) to local time.
Returns nil if parsing fails."
  (cond
   ;; DateTime format: 20251116T140000Z or 20251116T140000
   ((string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(Z\\)?" timestamp-str)
    (let* ((year (string-to-number (match-string 1 timestamp-str)))
           (month (string-to-number (match-string 2 timestamp-str)))
           (day (string-to-number (match-string 3 timestamp-str)))
           (hour (string-to-number (match-string 4 timestamp-str)))
           (minute (string-to-number (match-string 5 timestamp-str)))
           (second (string-to-number (match-string 6 timestamp-str)))
           (is-utc (match-string 7 timestamp-str)))
      (if is-utc
          (calendar-sync--convert-utc-to-local year month day hour minute second)
        (list year month day hour minute))))
   ;; Date format: 20251116
   ((string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" timestamp-str)
    (list (string-to-number (match-string 1 timestamp-str))
          (string-to-number (match-string 2 timestamp-str))
          (string-to-number (match-string 3 timestamp-str))))
   (t nil)))

(defun calendar-sync--format-timestamp (start end)
  "Format START and END timestamps as org timestamp.
START and END are lists from `calendar-sync--parse-timestamp'.
Returns string like '<2025-11-16 Sun 14:00-15:00>' or '<2025-11-16 Sun>'."
  (let* ((year (nth 0 start))
         (month (nth 1 start))
         (day (nth 2 start))
         (start-hour (nth 3 start))
         (start-min (nth 4 start))
         (end-hour (and end (nth 3 end)))
         (end-min (and end (nth 4 end)))
         (date-str (format-time-string
                    "<%Y-%m-%d %a"
                    (encode-time 0 0 0 day month year)))
         (time-str (when (and start-hour end-hour)
                     (format " %02d:%02d-%02d:%02d"
                             start-hour start-min end-hour end-min))))
    (concat date-str time-str ">")))

(defun calendar-sync--parse-event (event-str)
  "Parse single VEVENT string EVENT-STR into plist.
Returns plist with :summary :description :location :start :end.
Returns nil if event lacks required fields (DTSTART, SUMMARY)."
  (let ((summary (calendar-sync--get-property event-str "SUMMARY"))
        (description (calendar-sync--get-property event-str "DESCRIPTION"))
        (location (calendar-sync--get-property event-str "LOCATION"))
        (dtstart (calendar-sync--get-property event-str "DTSTART"))
        (dtend (calendar-sync--get-property event-str "DTEND")))
    (when (and summary dtstart)
      (let ((start-parsed (calendar-sync--parse-timestamp dtstart))
            (end-parsed (and dtend (calendar-sync--parse-timestamp dtend))))
        (when start-parsed
          (list :summary summary
                :description description
                :location location
                :start start-parsed
                :end end-parsed))))))

(defun calendar-sync--event-to-org (event)
  "Convert parsed EVENT plist to org entry string."
  (let* ((summary (plist-get event :summary))
         (description (plist-get event :description))
         (location (plist-get event :location))
         (start (plist-get event :start))
         (end (plist-get event :end))
         (timestamp (calendar-sync--format-timestamp start end))
         (parts (list (format "* %s" summary))))
    (push timestamp parts)
    (when description
      (push description parts))
    (when location
      (push (format "Location: %s" location) parts))
    (string-join (nreverse parts) "\n")))

(defun calendar-sync--event-start-time (event)
  "Extract comparable start time from EVENT plist.
Returns time value suitable for comparison, or 0 if no start time."
  (let ((start (plist-get event :start)))
    (if start
        (apply #'encode-time
               0  ; second
               (or (nth 4 start) 0)  ; minute
               (or (nth 3 start) 0)  ; hour
               (nth 2 start)  ; day
               (nth 1 start)  ; month
               (nth 0 start)  ; year
               nil)
      0)))

(defun calendar-sync--parse-ics (ics-content)
  "Parse ICS-CONTENT and return org-formatted string.
Returns nil if parsing fails.
Events are sorted chronologically by start time."
  (condition-case err
      (let* ((events (calendar-sync--split-events ics-content))
             (parsed-events (delq nil (mapcar #'calendar-sync--parse-event events)))
             (sorted-events (sort parsed-events
                                  (lambda (a b)
                                    (time-less-p (calendar-sync--event-start-time a)
                                                 (calendar-sync--event-start-time b)))))
             (org-entries (mapcar #'calendar-sync--event-to-org sorted-events)))
        (if org-entries
            (concat "# Google Calendar Events\n\n"
                    (string-join org-entries "\n\n")
                    "\n")
          nil))
    (error
     (setq calendar-sync--last-error (error-message-string err))
     (message "calendar-sync: Parse error: %s" calendar-sync--last-error)
     nil)))

;;; Sync functions

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
         :command (list "curl" "-s" "-L" "-m" "30" url)
         :sentinel
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (with-current-buffer (process-buffer process)
               (let ((content
                      (if (and (eq (process-status process) 'exit)
                               (= (process-exit-status process) 0))
                          (calendar-sync--normalize-line-endings (buffer-string))
                        (setq calendar-sync--last-error
                              (format "curl failed: %s" (string-trim event)))
                        (message "calendar-sync: Fetch error: %s" calendar-sync--last-error)
                        nil)))
                 (kill-buffer (process-buffer process))
                 (funcall callback content)))))))
    (error
     (setq calendar-sync--last-error (error-message-string err))
     (message "calendar-sync: Fetch error: %s" calendar-sync--last-error)
     (funcall callback nil))))

(defun calendar-sync--write-file (content)
  "Write CONTENT to `calendar-sync-file'.
Creates parent directories if needed."
  (let ((dir (file-name-directory calendar-sync-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file calendar-sync-file
    (insert content))
  (message "calendar-sync: Updated %s" calendar-sync-file))

;;;###autoload
(defun calendar-sync-now ()
  "Sync Google Calendar now asynchronously.
Downloads .ics file and updates org file without blocking Emacs.
Tracks timezone for automatic re-sync on timezone changes."
  (interactive)
  (if (not calendar-sync-ics-url)
      (message "calendar-sync: Please set calendar-sync-ics-url")
    (message "calendar-sync: Syncing...")
    (calendar-sync--fetch-ics
     calendar-sync-ics-url
     (lambda (ics-content)
       (let ((org-content (and ics-content (calendar-sync--parse-ics ics-content))))
         (if org-content
             (progn
               (calendar-sync--write-file org-content)
               (setq calendar-sync--last-sync-time (current-time))
               (setq calendar-sync--last-timezone-offset (calendar-sync--current-timezone-offset))
               (setq calendar-sync--last-error nil)
               (calendar-sync--save-state)
               (message "calendar-sync: Sync complete"))
           (message "calendar-sync: Sync failed (see *Messages* for details)")))))))

;;; Timer management

(defun calendar-sync--sync-timer-function ()
  "Function called by sync timer.
Checks for timezone changes and triggers re-sync if detected."
  (when (calendar-sync--timezone-changed-p)
    (let ((old-tz (calendar-sync--format-timezone-offset
                   calendar-sync--last-timezone-offset))
          (new-tz (calendar-sync--format-timezone-offset
                   (calendar-sync--current-timezone-offset))))
      (message "calendar-sync: Timezone change detected (%s → %s), re-syncing..."
               old-tz new-tz)))
  (calendar-sync-now))

;;;###autoload
(defun calendar-sync-start ()
  "Start automatic calendar syncing.
Syncs immediately, then every `calendar-sync-interval-minutes' minutes."
  (interactive)
  (when calendar-sync--timer
    (cancel-timer calendar-sync--timer))
  (if (not calendar-sync-ics-url)
      (message "calendar-sync: Please set calendar-sync-ics-url before starting")
    ;; Sync immediately
    (calendar-sync-now)
    ;; Start timer for future syncs (convert minutes to seconds)
    (let ((interval-seconds (* calendar-sync-interval-minutes 60)))
      (setq calendar-sync--timer
            (run-at-time interval-seconds
                         interval-seconds
                         #'calendar-sync--sync-timer-function)))
    (message "calendar-sync: Auto-sync started (every %d minutes)"
             calendar-sync-interval-minutes)))

;;;###autoload
(defun calendar-sync-stop ()
  "Stop automatic calendar syncing."
  (interactive)
  (when calendar-sync--timer
    (cancel-timer calendar-sync--timer)
    (setq calendar-sync--timer nil)
    (message "calendar-sync: Auto-sync stopped")))

;;;###autoload
(defun calendar-sync-toggle ()
  "Toggle automatic calendar syncing on/off."
  (interactive)
  (if calendar-sync--timer
      (calendar-sync-stop)
    (calendar-sync-start)))

;;; Keybindings

;; Calendar sync prefix and keymap
(defvar-keymap cj/calendar-map
  :doc "Keymap for calendar synchronization operations"
  "s" #'calendar-sync-now
  "t" #'calendar-sync-toggle
  "S" #'calendar-sync-start
  "x" #'calendar-sync-stop)

;; Only set up keybindings if cj/custom-keymap exists (not in test environment)
(when (boundp 'cj/custom-keymap)
  (keymap-set cj/custom-keymap "g" cj/calendar-map)

  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-; g" "calendar sync menu"
      "C-; g s" "sync now"
      "C-; g t" "toggle auto-sync"
      "C-; g S" "start auto-sync"
      "C-; g x" "stop auto-sync")))

;;; Initialization

;; Load saved state from previous session
(calendar-sync--load-state)

;; Check for timezone change on startup
(when (and calendar-sync-ics-url
           (calendar-sync--timezone-changed-p))
  (let ((old-tz (calendar-sync--format-timezone-offset
                 calendar-sync--last-timezone-offset))
        (new-tz (calendar-sync--format-timezone-offset
                 (calendar-sync--current-timezone-offset))))
    (message "calendar-sync: Timezone changed since last session (%s → %s)"
             old-tz new-tz)
    (message "calendar-sync: Will sync on next timer tick")
    ;; Note: We don't auto-sync here to avoid blocking Emacs startup
    ;; User can manually sync or it will happen on next timer tick if auto-sync is enabled
    ))

;; Start auto-sync if enabled and URL is configured
;; Syncs immediately then every calendar-sync-interval-minutes (default: 60 minutes)
(when (and calendar-sync-auto-start calendar-sync-ics-url)
  (calendar-sync-start))

(provide 'calendar-sync)
;;; calendar-sync.el ends here
