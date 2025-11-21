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
;; - Recurring event support (RRULE expansion)
;; - Timer-based automatic sync (every 60 minutes, configurable)
;; - Self-contained in .emacs.d (no cron, portable across machines)
;; - Read-only (can't corrupt Google Calendar)
;; - Works with chime.el for event notifications
;;
;; Recurring Events (RRULE):
;;
;; Google Calendar recurring events are defined once with an RRULE
;; (recurrence rule) rather than as individual event instances. This
;; module expands recurring events into individual org entries.
;;
;; Expansion uses a rolling window approach:
;; - Past: 3 months before today
;; - Future: 12 months after today
;;
;; Every sync regenerates the entire file based on the current date,
;; so the window automatically advances as time passes. Old events
;; naturally fall off after 3 months, and new future events appear
;; as you approach them.
;;
;; Example: If today is 2025-11-18, events are expanded from
;; 2025-08-18 to 2026-11-18. When you sync on 2026-01-01, the
;; window shifts to 2025-10-01 to 2027-01-01 automatically.
;;
;; This approach requires no state tracking and naturally handles
;; the "year boundary" problem - there is no boundary to cross,
;; the window just moves forward with each sync.
;;
;; Supported RRULE patterns:
;; - FREQ=DAILY: Daily events
;; - FREQ=WEEKLY;BYDAY=MO,WE,FR: Weekly on specific days
;; - FREQ=MONTHLY: Monthly events (same day each month)
;; - FREQ=YEARLY: Yearly events (anniversaries, birthdays)
;; - INTERVAL: Repeat every N periods (e.g., every 2 weeks)
;; - UNTIL: End date for recurrence
;; - COUNT: Maximum occurrences (combined with date range limit)
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

(defvar calendar-sync-past-months 3
  "Number of months in the past to include when expanding recurring events.
Default: 3 months. This keeps recent history visible in org-agenda.")

(defvar calendar-sync-future-months 12
  "Number of months in the future to include when expanding recurring events.
Default: 12 months. This provides a full year of future events.")

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
       (cj/log-silently "calendar-sync: Error loading state: %s" (error-message-string err))))))

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

;;; Date Utilities

(defun calendar-sync--add-months (date months)
  "Add MONTHS to DATE.
DATE is (year month day), returns new (year month day)."
  (let* ((year (nth 0 date))
         (month (nth 1 date))
         (day (nth 2 date))
         (total-months (+ (* year 12) month -1 months))
         (new-year (/ total-months 12))
         (new-month (1+ (mod total-months 12))))
    (list new-year new-month day)))

(defun calendar-sync--get-date-range ()
  "Get date range for event expansion as (start-time end-time).
Returns time values for -3 months and +12 months from today."
  (let* ((now (decode-time))
         (today (list (nth 5 now) (nth 4 now) (nth 3 now)))
         (start-date (calendar-sync--add-months today (- calendar-sync-past-months)))
         (end-date (calendar-sync--add-months today calendar-sync-future-months))
         (start-time (apply #'encode-time 0 0 0 (reverse start-date)))
         (end-time (apply #'encode-time 0 0 0 (reverse end-date))))
    (list start-time end-time)))

(defun calendar-sync--date-in-range-p (date range)
  "Check if DATE is within RANGE.
DATE is (year month day hour minute), RANGE is (start-time end-time)."
  (let* ((year (nth 0 date))
         (month (nth 1 date))
         (day (nth 2 date))
         (date-time (encode-time 0 0 0 day month year))
         (start-time (nth 0 range))
         (end-time (nth 1 range)))
    (and (time-less-p start-time date-time)
         (time-less-p date-time end-time))))

(defun calendar-sync--weekday-to-number (weekday)
  "Convert WEEKDAY string (MO, TU, etc.) to number (1-7).
Monday = 1, Sunday = 7."
  (pcase weekday
    ("MO" 1)
    ("TU" 2)
    ("WE" 3)
    ("TH" 4)
    ("FR" 5)
    ("SA" 6)
    ("SU" 7)
    (_ nil)))

(defun calendar-sync--date-weekday (date)
  "Get weekday number for DATE (year month day).
Monday = 1, Sunday = 7."
  (let* ((year (nth 0 date))
         (month (nth 1 date))
         (day (nth 2 date))
         (time (encode-time 0 0 0 day month year))
         (decoded (decode-time time))
         (dow (nth 6 decoded)))  ; 0 = Sunday, 1 = Monday, etc.
    (if (= dow 0) 7 dow)))  ; Convert to 1-7 with Monday=1

(defun calendar-sync--add-days (date days)
  "Add DAYS to DATE (year month day).
Returns new (year month day)."
  (let* ((year (nth 0 date))
         (month (nth 1 date))
         (day (nth 2 date))
         (time (encode-time 0 0 0 day month year))
         (new-time (time-add time (days-to-time days)))
         (decoded (decode-time new-time)))
    (list (nth 5 decoded) (nth 4 decoded) (nth 3 decoded))))

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
Handles property parameters (e.g., DTSTART;TZID=America/Chicago:value).
Handles multi-line values (lines starting with space).
Returns nil if property not found."
  (when (string-match (format "^%s[^:\n]*:\\(.*\\)$" (regexp-quote property)) event)
    (let ((value (match-string 1 event))
          (start (match-end 0)))
      ;; Handle continuation lines (start with space or tab)
      (while (and (< start (length event))
                  (string-match "^\n[ \t]\\(.*\\)$" event start))
        (setq value (concat value (match-string 1 event)))
        (setq start (match-end 0)))
      value)))

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

;;; RRULE Parsing and Expansion

;;; Helper Functions

(defun calendar-sync--date-to-time (date)
  "Convert DATE (year month day) to time value for comparison.
DATE should be a list like (year month day)."
  (apply #'encode-time 0 0 0 (reverse date)))

(defun calendar-sync--before-date-p (date1 date2)
  "Return t if DATE1 is before DATE2.
Both dates should be lists like (year month day)."
  (time-less-p (calendar-sync--date-to-time date1)
               (calendar-sync--date-to-time date2)))

(defun calendar-sync--create-occurrence (base-event occurrence-date)
  "Create an occurrence from BASE-EVENT with OCCURRENCE-DATE.
OCCURRENCE-DATE should be a list (year month day hour minute second)."
  (let* ((occurrence (copy-sequence base-event))
         (end (plist-get base-event :end)))
    (plist-put occurrence :start occurrence-date)
    (when end
      ;; Use the date from occurrence-date but keep the time from the original end
      (let ((date-only (list (nth 0 occurrence-date)
                             (nth 1 occurrence-date)
                             (nth 2 occurrence-date))))
        (plist-put occurrence :end (append date-only (nthcdr 3 end)))))
    occurrence))

(defun calendar-sync--parse-rrule (rrule-str)
  "Parse RRULE string into plist.
Returns plist with :freq :interval :byday :until :count."
  (let ((parts (split-string rrule-str ";"))
        (result '()))
    (dolist (part parts)
      (when (string-match "\\([^=]+\\)=\\(.+\\)" part)
        (let ((key (match-string 1 part))
              (value (match-string 2 part)))
          (pcase key
            ("FREQ" (setq result (plist-put result :freq (intern (downcase value)))))
            ("INTERVAL" (setq result (plist-put result :interval (string-to-number value))))
            ("BYDAY" (setq result (plist-put result :byday (split-string value ","))))
            ("UNTIL" (setq result (plist-put result :until (calendar-sync--parse-timestamp value))))
            ("COUNT" (setq result (plist-put result :count (string-to-number value))))))))
    ;; Set defaults
    (unless (plist-get result :interval)
      (setq result (plist-put result :interval 1)))
    result))

(defun calendar-sync--expand-daily (base-event rrule range)
  "Expand daily recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range."
  (let* ((start (plist-get base-event :start))
         (interval (plist-get rrule :interval))
         (until (plist-get rrule :until))
         (count (plist-get rrule :count))
         (occurrences '())
         (current-date (list (nth 0 start) (nth 1 start) (nth 2 start)))
         (num-generated 0)
         (range-end-time (cadr range)))
    ;; For infinite recurrence (no COUNT/UNTIL), stop at range-end for performance
    ;; For COUNT, generate all occurrences from start regardless of range
    (while (and (or count until (time-less-p (calendar-sync--date-to-time current-date) range-end-time))
                (or (not until) (calendar-sync--before-date-p current-date until))
                (or (not count) (< num-generated count)))
      (let ((occurrence-datetime (append current-date (nthcdr 3 start))))
        ;; Check UNTIL date first
        (when (or (not until) (calendar-sync--before-date-p current-date until))
          ;; Check COUNT - increment BEFORE range check so COUNT is absolute from start
          (when (or (not count) (< num-generated count))
            (setq num-generated (1+ num-generated))
            ;; Only add to output if within date range
            (when (calendar-sync--date-in-range-p occurrence-datetime range)
              (push (calendar-sync--create-occurrence base-event occurrence-datetime)
                    occurrences)))))
      (setq current-date (calendar-sync--add-days current-date interval)))
    (nreverse occurrences)))

(defun calendar-sync--expand-weekly (base-event rrule range)
  "Expand weekly recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range."
  (let* ((start (plist-get base-event :start))
         (end (plist-get base-event :end))
         (interval (plist-get rrule :interval))
         (byday (plist-get rrule :byday))
         (until (plist-get rrule :until))
         (count (plist-get rrule :count))
         (occurrences '())
         (current-date (list (nth 0 start) (nth 1 start) (nth 2 start)))
         (num-generated 0)
         (range-end-time (cadr range))
         (max-iterations 1000) ;; Safety: prevent infinite loops
         (iterations 0)
         (weekdays (if byday
                       (mapcar #'calendar-sync--weekday-to-number byday)
                     (list (calendar-sync--date-weekday current-date)))))
    ;; Validate interval
    (when (<= interval 0)
      (error "Invalid RRULE interval: %s (must be > 0)" interval))
    ;; Start from the first week
    ;; For infinite recurrence (no COUNT/UNTIL), stop at range-end for performance
    ;; For COUNT, generate all occurrences from start regardless of range
    (while (and (< iterations max-iterations)
                (or count until (time-less-p (calendar-sync--date-to-time current-date) range-end-time))
                (or (not count) (< num-generated count))
                (or (not until) (calendar-sync--before-date-p current-date until)))
      (setq iterations (1+ iterations))
      ;; Generate occurrences for each weekday in this week
      (dolist (weekday weekdays)
        (let* ((current-weekday (calendar-sync--date-weekday current-date))
               (days-ahead (mod (- weekday current-weekday) 7))
               (occurrence-date (calendar-sync--add-days current-date days-ahead))
               (occurrence-datetime (append occurrence-date (nthcdr 3 start))))
          ;; Check UNTIL date first
          (when (or (not until) (calendar-sync--before-date-p occurrence-date until))
            ;; Check COUNT - increment BEFORE range check so COUNT is absolute from start
            (when (or (not count) (< num-generated count))
              (setq num-generated (1+ num-generated))
              ;; Only add to output if within date range
              (when (calendar-sync--date-in-range-p occurrence-datetime range)
                (push (calendar-sync--create-occurrence base-event occurrence-datetime)
                      occurrences))))))
      ;; Move to next interval week
      (setq current-date (calendar-sync--add-days current-date (* 7 interval))))
    (when (>= iterations max-iterations)
      (cj/log-silently "calendar-sync: WARNING: Hit max iterations (%d) expanding weekly event" max-iterations))
    (nreverse occurrences)))

(defun calendar-sync--expand-monthly (base-event rrule range)
  "Expand monthly recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range."
  (let* ((start (plist-get base-event :start))
         (interval (plist-get rrule :interval))
         (until (plist-get rrule :until))
         (count (plist-get rrule :count))
         (occurrences '())
         (current-date (list (nth 0 start) (nth 1 start) (nth 2 start)))
         (num-generated 0)
         (range-end-time (cadr range)))
    ;; For infinite recurrence (no COUNT/UNTIL), stop at range-end for performance
    ;; For COUNT, generate all occurrences from start regardless of range
    (while (and (or count until (time-less-p (calendar-sync--date-to-time current-date) range-end-time))
                (or (not until) (calendar-sync--before-date-p current-date until))
                (or (not count) (< num-generated count)))
      (let ((occurrence-datetime (append current-date (nthcdr 3 start))))
        ;; Check UNTIL date first
        (when (or (not until) (calendar-sync--before-date-p current-date until))
          ;; Check COUNT - increment BEFORE range check so COUNT is absolute from start
          (when (or (not count) (< num-generated count))
            (setq num-generated (1+ num-generated))
            ;; Only add to output if within date range
            (when (calendar-sync--date-in-range-p occurrence-datetime range)
              (push (calendar-sync--create-occurrence base-event occurrence-datetime)
                    occurrences)))))
      (setq current-date (calendar-sync--add-months current-date interval)))
    (nreverse occurrences)))

(defun calendar-sync--expand-yearly (base-event rrule range)
  "Expand yearly recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range."
  (let* ((start (plist-get base-event :start))
         (interval (plist-get rrule :interval))
         (until (plist-get rrule :until))
         (count (plist-get rrule :count))
         (occurrences '())
         (current-date (list (nth 0 start) (nth 1 start) (nth 2 start)))
         (num-generated 0)
         (range-end-time (cadr range)))
    ;; For infinite recurrence (no COUNT/UNTIL), stop at range-end for performance
    ;; For COUNT, generate all occurrences from start regardless of range
    (while (and (or count until (time-less-p (calendar-sync--date-to-time current-date) range-end-time))
                (or (not until) (calendar-sync--before-date-p current-date until))
                (or (not count) (< num-generated count)))
      (let ((occurrence-datetime (append current-date (nthcdr 3 start))))
        ;; Check UNTIL date first
        (when (or (not until) (calendar-sync--before-date-p current-date until))
          ;; Check COUNT - increment BEFORE range check so COUNT is absolute from start
          (when (or (not count) (< num-generated count))
            (setq num-generated (1+ num-generated))
            ;; Only add to output if within date range
            (when (calendar-sync--date-in-range-p occurrence-datetime range)
              (push (calendar-sync--create-occurrence base-event occurrence-datetime)
                    occurrences)))))
      (setq current-date (calendar-sync--add-months current-date (* 12 interval))))
    (nreverse occurrences)))

(defun calendar-sync--expand-recurring-event (event-str range)
  "Expand recurring event EVENT-STR into individual occurrences within RANGE.
Returns list of event plists, or nil if not a recurring event."
  (let ((rrule (calendar-sync--get-property event-str "RRULE")))
    (when rrule
      (let* ((base-event (calendar-sync--parse-event event-str))
             (parsed-rrule (calendar-sync--parse-rrule rrule))
             (freq (plist-get parsed-rrule :freq)))
        (when base-event
          (pcase freq
            ('daily (calendar-sync--expand-daily base-event parsed-rrule range))
            ('weekly (calendar-sync--expand-weekly base-event parsed-rrule range))
            ('monthly (calendar-sync--expand-monthly base-event parsed-rrule range))
            ('yearly (calendar-sync--expand-yearly base-event parsed-rrule range))
            (_ (cj/log-silently "calendar-sync: Unsupported RRULE frequency: %s" freq)
               nil)))))))

(defun calendar-sync--parse-event (event-str)
  "Parse single VEVENT string EVENT-STR into plist.
Returns plist with :summary :description :location :start :end.
Returns nil if event lacks required fields (DTSTART, SUMMARY).
Skips events with RECURRENCE-ID (individual instances of recurring events)."
  ;; Skip individual instances of recurring events (they're handled by RRULE expansion)
  (unless (calendar-sync--get-property event-str "RECURRENCE-ID")
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
                  :end end-parsed)))))))

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
Events are sorted chronologically by start time.
Recurring events are expanded into individual occurrences."
  (condition-case err
      (let* ((range (calendar-sync--get-date-range))
             (events (calendar-sync--split-events ics-content))
             (parsed-events '())
             (max-events 5000)  ; Safety limit to prevent Emacs from hanging
             (events-generated 0))
        ;; Process each event
        (dolist (event-str events)
          (when (< events-generated max-events)
            (let ((expanded (calendar-sync--expand-recurring-event event-str range)))
              (if expanded
                  ;; Recurring event - add all occurrences
                  (progn
                    (setq parsed-events (append parsed-events expanded))
                    (setq events-generated (+ events-generated (length expanded))))
                ;; Non-recurring event - parse normally
                (let ((parsed (calendar-sync--parse-event event-str)))
                  (when (and parsed
                             (calendar-sync--date-in-range-p (plist-get parsed :start) range))
                    (push parsed parsed-events)
                    (setq events-generated (1+ events-generated))))))))
        (when (>= events-generated max-events)
          (cj/log-silently "calendar-sync: WARNING: Hit max events limit (%d), some events may be missing" max-events))
        (cj/log-silently "calendar-sync: Processing %d events..." (length parsed-events))
        ;; Sort and convert to org format
        (let* ((sorted-events (sort parsed-events
                                    (lambda (a b)
                                      (time-less-p (calendar-sync--event-start-time a)
                                                   (calendar-sync--event-start-time b)))))
               (org-entries (mapcar #'calendar-sync--event-to-org sorted-events)))
          (if org-entries
              (concat "# Google Calendar Events\n\n"
                      (string-join org-entries "\n\n")
                      "\n")
            nil)))
    (error
     (setq calendar-sync--last-error (error-message-string err))
     (cj/log-silently "calendar-sync: Parse error: %s" calendar-sync--last-error)
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
             (let ((buf (process-buffer process)))
               (when (buffer-live-p buf)
                 (let ((content
                        (with-current-buffer buf
                          (if (and (eq (process-status process) 'exit)
                                   (= (process-exit-status process) 0))
                              (calendar-sync--normalize-line-endings (buffer-string))
                            (setq calendar-sync--last-error
                                  (format "curl failed: %s" (string-trim event)))
                            (cj/log-silently "calendar-sync: Fetch error: %s" calendar-sync--last-error)
                            nil))))
                   (kill-buffer buf)
                   (funcall callback content))))))))
    (error
     (setq calendar-sync--last-error (error-message-string err))
     (cj/log-silently "calendar-sync: Fetch error: %s" calendar-sync--last-error)
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
