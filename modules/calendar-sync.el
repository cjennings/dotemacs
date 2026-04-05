;;; calendar-sync.el --- Multi-calendar sync via .ics  -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-16

;;; Commentary:

;; Simple, reliable one-way sync from multiple calendars to Org mode.
;; Downloads .ics files from calendar URLs (Google, Proton, etc.) and
;; converts to Org format. No OAuth, no API complexity, just file conversion.
;;
;; Features:
;; - Multi-calendar support (sync multiple calendars to separate files)
;; - Pure Emacs Lisp .ics parser (no external dependencies)
;; - Recurring event support (RRULE expansion)
;; - Timer-based automatic sync (every 60 minutes, configurable)
;; - Self-contained in .emacs.d (no cron, portable across machines)
;; - Read-only (can't corrupt source calendars)
;; - Works with Chime for event notifications
;;
;; Recurring Events (RRULE):
;;
;; Calendar recurring events are defined once with an RRULE
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
;; 1. Configure calendars in your init.el:
;;    (setq calendar-sync-calendars
;;          '((:name "google"
;;             :url "https://calendar.google.com/calendar/ical/.../basic.ics"
;;             :file gcal-file)
;;            (:name "proton"
;;             :url "https://calendar.proton.me/api/calendar/v1/url/.../calendar.ics"
;;             :file pcal-file)))
;;
;; 2. Load and start:
;;    (require 'calendar-sync)
;;    (calendar-sync-start)
;;
;; 3. Add to org-agenda (optional):
;;    (dolist (cal calendar-sync-calendars)
;;      (add-to-list 'org-agenda-files (plist-get cal :file)))
;;
;; Usage:
;; - M-x calendar-sync-now    ; Sync all or select specific calendar
;; - M-x calendar-sync-start  ; Start auto-sync
;; - M-x calendar-sync-stop   ; Stop auto-sync
;; - M-x calendar-sync-toggle ; Toggle auto-sync
;; - M-x calendar-sync-status ; Show sync status for all calendars

;;; Code:

(require 'user-constants)  ; For gcal-file, pcal-file paths

;;; Configuration

(defvar calendar-sync-calendars nil
  "List of calendars to sync.
Each calendar is a plist with the following keys:
  :name - Display name for the calendar (used in logs and prompts)
  :url  - URL to fetch .ics file from
  :file - Output file path for org format

Example:
  (setq calendar-sync-calendars
        \\='((:name \"google\"
           :url \"https://calendar.google.com/calendar/ical/.../basic.ics\"
           :file gcal-file)
          (:name \"proton\"
           :url \"https://calendar.proton.me/api/calendar/v1/url/.../calendar.ics\"
           :file pcal-file)))")

;; Calendar sync (one-way: Google/Proton → Org)
(setq calendar-sync-calendars
      `((:name "google"
         :url "https://calendar.google.com/calendar/ical/craigmartinjennings%40gmail.com/private-1dad154d6a2100e755f76e2d0502f6aa/basic.ics"
         :file ,gcal-file)
        (:name "proton"
         :url "https://calendar.proton.me/api/calendar/v1/url/MpLtuwsUNoygyA_60GvJE5cz0hbREbrAPBEJoWDRpFEstnmzmEMDb7sjLzkY8kbkF10A7Be3wGKB1-vqaLf-pw==/calendar.ics?CacheKey=LrB9NG5Vfqp5p2sy90H13g%3D%3D&PassphraseKey=sURqFfACPM21d6AXSeaEXYCruimvSb8t0ce1vuxRAXk%3D"
         :file ,pcal-file)
        (:name "deepsat"
         :url "https://calendar.google.com/calendar/ical/craig.jennings%40deepsat.com/private-f0250a2c6752a5ca71d7b0636587a6d5/basic.ics"
         :file ,dcal-file)))

(defvar calendar-sync-interval-minutes 60
  "Sync interval in minutes.
Default: 60 minutes (1 hour).")

(defvar calendar-sync-auto-start t
  "Whether to automatically start calendar sync when module loads.
If non-nil, sync starts automatically when calendar-sync is loaded.
If nil, user must manually call `calendar-sync-start'.")

(defvar calendar-sync-user-emails
  '("craigmartinjennings@gmail.com" "craig.jennings@deepsat.com" "c@cjennings.net")
  "List of user email addresses for determining acceptance status.
Used by `calendar-sync--find-user-status' to look up the user's
PARTSTAT in event attendee lists.")

(defvar calendar-sync-past-months 3
  "Number of months in the past to include when expanding recurring events.
Default: 3 months. This keeps recent history visible in org-agenda.")

(defvar calendar-sync-future-months 12
  "Number of months in the future to include when expanding recurring events.
Default: 12 months. This provides a full year of future events.")

(defvar calendar-sync-fetch-timeout 120
  "Maximum time in seconds for a calendar fetch to complete.
This is the total time allowed for the entire transfer (connect + download).
Large calendars (thousands of events) may need more time on slow connections.
A separate 10-second connect timeout ensures fast failure when a host is
unreachable.")

;;; Internal state

(defvar calendar-sync--timer nil
  "Timer object for automatic syncing.")

(defvar calendar-sync--calendar-states (make-hash-table :test 'equal)
  "Per-calendar sync state.
Hash table mapping calendar name (string) to state plist with:
  :last-sync  - Time of last successful sync
  :status     - Symbol: ok, error, or syncing
  :last-error - Error message string, or nil")

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
            ;; Load per-calendar states
            (let ((cal-states (alist-get 'calendar-states state)))
              (clrhash calendar-sync--calendar-states)
              (dolist (entry cal-states)
                (puthash (car entry) (cdr entry) calendar-sync--calendar-states)))))
      (error
       (cj/log-silently "calendar-sync: Error loading state: %s" (error-message-string err))))))

(defun calendar-sync--get-calendar-state (calendar-name)
  "Get state plist for CALENDAR-NAME, or nil if not found."
  (gethash calendar-name calendar-sync--calendar-states))

(defun calendar-sync--set-calendar-state (calendar-name state)
  "Set STATE plist for CALENDAR-NAME."
  (puthash calendar-name state calendar-sync--calendar-states))

;;; Line Ending Normalization

(defun calendar-sync--normalize-line-endings (content)
  "Normalize line endings in CONTENT to Unix format (LF only).
Removes all carriage return characters (\\r) from CONTENT.
The iCalendar format (RFC 5545) uses CRLF line endings, but Emacs
and 'org-mode' expect LF only. This function ensures consistent line
endings throughout the parsing pipeline.

Returns CONTENT with all \\r characters removed."
  (if (not (stringp content))
      content
    (replace-regexp-in-string "\r" "" content)))

;;; Text Cleaning (ICS unescape + HTML strip)

(defun calendar-sync--unescape-ics-text (text)
  "Unescape RFC 5545 escape sequences in TEXT.
Converts: \\n→newline, \\,→comma, \\\\→backslash, \\;→semicolon.
Returns nil for nil input."
  (when text
    ;; Use placeholder for literal backslash to avoid double-unescaping.
    ;; replace-regexp-in-string with LITERAL=t avoids backslash interpretation.
    (let ((result (replace-regexp-in-string "\\\\\\\\" "\000" text)))
      (setq result (replace-regexp-in-string "\\\\n" "\n" result t t))
      (setq result (replace-regexp-in-string "\\\\," "," result t t))
      (setq result (replace-regexp-in-string "\\\\;" ";" result t t))
      (replace-regexp-in-string "\000" "\\" result t t))))

(defun calendar-sync--strip-html (text)
  "Strip HTML tags from TEXT and decode common HTML entities.
Converts <br>, <br/>, <br /> to newlines.  Strips all other tags.
Decodes &amp; &lt; &gt; &quot;.  Collapses excessive blank lines.
Returns nil for nil input."
  (when text
    (let ((result text))
      ;; Convert <br> variants to newline (must come before tag stripping)
      (setq result (replace-regexp-in-string "<br[ \t]*/?>[ \t]*" "\n" result))
      ;; Strip all remaining HTML tags
      (setq result (replace-regexp-in-string "<[^>]*>" "" result))
      ;; Decode HTML entities
      (setq result (replace-regexp-in-string "&amp;" "&" result))
      (setq result (replace-regexp-in-string "&lt;" "<" result))
      (setq result (replace-regexp-in-string "&gt;" ">" result))
      (setq result (replace-regexp-in-string "&quot;" "\"" result))
      ;; Collapse 3+ consecutive newlines to 2
      (setq result (replace-regexp-in-string "\n\\{3,\\}" "\n\n" result))
      result)))

(defun calendar-sync--clean-text (text)
  "Clean TEXT by unescaping ICS sequences, stripping HTML, and trimming.
Returns nil for nil input.  Returns empty string for whitespace-only input."
  (when text
    (string-trim (calendar-sync--strip-html (calendar-sync--unescape-ics-text text)))))

(defun calendar-sync--sanitize-org-body (text)
  "Sanitize TEXT for safe inclusion as org body content.
Replaces leading asterisks with dashes to prevent lines from being
parsed as org headings.  Handles multiple levels (e.g. ** becomes --)."
  (when text
    (replace-regexp-in-string
     "^\\(\\*+\\) "
     (lambda (match)
       (concat (make-string (length (match-string 1 match)) ?-) " "))
     text)))

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
Returns new (year month day).
Uses noon internally to avoid DST boundary issues where adding
86400 seconds to midnight can land on the same calendar date
during fall-back transitions."
  (let* ((year (nth 0 date))
         (month (nth 1 date))
         (day (nth 2 date))
         (time (encode-time 0 0 12 day month year))
         (new-time (time-add time (days-to-time days)))
         (decoded (decode-time new-time)))
    (list (nth 5 decoded) (nth 4 decoded) (nth 3 decoded))))

;;; RECURRENCE-ID Exception Handling

(defun calendar-sync--get-recurrence-id (event-str)
  "Extract RECURRENCE-ID value from EVENT-STR.
Returns the datetime value (without TZID parameter), or nil if not found.
Handles both simple values and values with parameters like TZID."
  (when (and event-str (stringp event-str))
    (calendar-sync--get-property event-str "RECURRENCE-ID")))

(defun calendar-sync--get-recurrence-id-line (event-str)
  "Extract full RECURRENCE-ID line from EVENT-STR, including parameters.
Returns the complete line like 'RECURRENCE-ID;TZID=Europe/Tallinn:20260203T170000'.
Returns nil if not found."
  (when (and event-str (stringp event-str))
    (calendar-sync--get-property-line event-str "RECURRENCE-ID")))

(defun calendar-sync--parse-ics-datetime (value)
  "Parse iCal datetime VALUE into (year month day hour minute) list.
Returns nil for invalid input. For date-only values, returns (year month day nil nil).
Handles formats: 20260203T090000Z, 20260203T090000, 20260203."
  (when (and value
             (stringp value)
             (not (string-empty-p value)))
    (cond
     ;; DateTime format: 20260203T090000Z or 20260203T090000
     ((string-match "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)Z?\\'" value)
      (list (string-to-number (match-string 1 value))
            (string-to-number (match-string 2 value))
            (string-to-number (match-string 3 value))
            (string-to-number (match-string 4 value))
            (string-to-number (match-string 5 value))))
     ;; Date-only format: 20260203
     ((string-match "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" value)
      (list (string-to-number (match-string 1 value))
            (string-to-number (match-string 2 value))
            (string-to-number (match-string 3 value))
            nil nil))
     (t nil))))

(defalias 'calendar-sync--parse-recurrence-id #'calendar-sync--parse-ics-datetime
  "Parse RECURRENCE-ID value. See `calendar-sync--parse-ics-datetime'.")

(defun calendar-sync--collect-recurrence-exceptions (ics-content)
  "Collect all RECURRENCE-ID events from ICS-CONTENT.
Returns hash table mapping UID to list of exception event plists.
Each exception plist contains :recurrence-id (parsed), :start, :end, :summary, etc."
  (let ((exceptions (make-hash-table :test 'equal)))
    (when (and ics-content (stringp ics-content))
      (let ((events (calendar-sync--split-events ics-content)))
        (dolist (event-str events)
          (let ((recurrence-id (calendar-sync--get-recurrence-id event-str))
                (uid (calendar-sync--get-property event-str "UID")))
            (when (and recurrence-id uid)
              ;; Parse the exception event
              (let* ((recurrence-id-line (calendar-sync--get-recurrence-id-line event-str))
                     (recurrence-id-tzid (calendar-sync--extract-tzid recurrence-id-line))
                     (recurrence-id-is-utc (and recurrence-id
                                                (string-suffix-p "Z" recurrence-id)))
                     (recurrence-id-parsed (calendar-sync--parse-recurrence-id recurrence-id))
                     ;; Parse the new times from the exception
                     (dtstart (calendar-sync--get-property event-str "DTSTART"))
                     (dtend (calendar-sync--get-property event-str "DTEND"))
                     (dtstart-line (calendar-sync--get-property-line event-str "DTSTART"))
                     (dtend-line (calendar-sync--get-property-line event-str "DTEND"))
                     (start-tzid (calendar-sync--extract-tzid dtstart-line))
                     (end-tzid (calendar-sync--extract-tzid dtend-line))
                     (start-parsed (calendar-sync--parse-timestamp dtstart start-tzid))
                     (end-parsed (and dtend (calendar-sync--parse-timestamp dtend end-tzid)))
                     (summary (calendar-sync--clean-text
                               (calendar-sync--get-property event-str "SUMMARY")))
                     (description (calendar-sync--clean-text
                                   (calendar-sync--get-property event-str "DESCRIPTION")))
                     (location (calendar-sync--clean-text
                                (calendar-sync--get-property event-str "LOCATION"))))
                (when (and recurrence-id-parsed start-parsed)
                  (let ((local-recurrence-id
                         (calendar-sync--localize-parsed-datetime
                          recurrence-id-parsed recurrence-id-is-utc recurrence-id-tzid)))
                    (let ((exception-plist
                           (list :recurrence-id local-recurrence-id
                                 :recurrence-id-raw recurrence-id
                                 :start start-parsed
                                 :end end-parsed
                                 :summary summary
                                 :description description
                                 :location location)))
                      ;; Add to hash table
                      (let ((existing (gethash uid exceptions)))
                        (puthash uid (cons exception-plist existing) exceptions)))))))))))
    exceptions))

(defun calendar-sync--occurrence-matches-exception-p (occurrence exception)
  "Check if OCCURRENCE matches EXCEPTION's recurrence-id.
Compares year, month, day, hour, minute."
  (let ((occ-start (plist-get occurrence :start))
        (exc-recid (plist-get exception :recurrence-id)))
    (and occ-start exc-recid
         (= (nth 0 occ-start) (nth 0 exc-recid))  ; year
         (= (nth 1 occ-start) (nth 1 exc-recid))  ; month
         (= (nth 2 occ-start) (nth 2 exc-recid))  ; day
         ;; Hour/minute check (handle nil for all-day events)
         (or (and (null (nth 3 occ-start)) (null (nth 3 exc-recid)))
             (and (nth 3 occ-start) (nth 3 exc-recid)
                  (= (nth 3 occ-start) (nth 3 exc-recid))
                  (= (or (nth 4 occ-start) 0) (or (nth 4 exc-recid) 0)))))))

(defun calendar-sync--apply-single-exception (occurrence exception)
  "Apply EXCEPTION to OCCURRENCE, returning modified occurrence."
  (let ((result (copy-sequence occurrence)))
    ;; Update time from exception
    (plist-put result :start (plist-get exception :start))
    (when (plist-get exception :end)
      (plist-put result :end (plist-get exception :end)))
    ;; Update summary if exception has one
    (when (plist-get exception :summary)
      (plist-put result :summary (plist-get exception :summary)))
    ;; Update other fields
    (when (plist-get exception :description)
      (plist-put result :description (plist-get exception :description)))
    (when (plist-get exception :location)
      (plist-put result :location (plist-get exception :location)))
    ;; Pass through new fields if exception overrides them
    (when (plist-get exception :attendees)
      (plist-put result :attendees (plist-get exception :attendees)))
    (when (plist-get exception :organizer)
      (plist-put result :organizer (plist-get exception :organizer)))
    (when (plist-get exception :url)
      (plist-put result :url (plist-get exception :url)))
    result))

(defun calendar-sync--apply-recurrence-exceptions (occurrences exceptions)
  "Apply EXCEPTIONS to OCCURRENCES list.
OCCURRENCES is list of event plists from RRULE expansion.
EXCEPTIONS is hash table from `calendar-sync--collect-recurrence-exceptions'.
Returns new list with matching occurrences replaced by exception times."
  (if (or (null occurrences) (null exceptions))
      occurrences
    (mapcar
     (lambda (occurrence)
       (let* ((uid (plist-get occurrence :uid))
              (uid-exceptions (and uid (gethash uid exceptions))))
         (if (null uid-exceptions)
             occurrence
           ;; Check if any exception matches this occurrence
           (let ((matching-exception
                  (cl-find-if (lambda (exc)
                                (calendar-sync--occurrence-matches-exception-p occurrence exc))
                              uid-exceptions)))
             (if matching-exception
                 (calendar-sync--apply-single-exception occurrence matching-exception)
               occurrence)))))
     occurrences)))

;;; EXDATE (Excluded Date) Handling

(defun calendar-sync--get-exdates (event-str)
  "Extract all EXDATE values from EVENT-STR.
Returns list of datetime strings (without TZID parameters), or nil if none found.
Handles both simple values and values with parameters like TZID."
  (when (and event-str (stringp event-str) (not (string-empty-p event-str)))
    (let ((exdates '())
          (pos 0))
      ;; Find all EXDATE lines
      (while (string-match "^EXDATE[^:\n]*:\\([^\n]+\\)" event-str pos)
        (push (match-string 1 event-str) exdates)
        (setq pos (match-end 0)))
      (nreverse exdates))))

(defun calendar-sync--get-exdate-line (event-str exdate-value)
  "Find the full EXDATE line containing EXDATE-VALUE from EVENT-STR.
Returns the complete line like 'EXDATE;TZID=America/New_York:20260210T130000'.
Returns nil if not found."
  (when (and event-str (stringp event-str) exdate-value)
    (let ((pattern (format "^\\(EXDATE[^:]*:%s\\)" (regexp-quote exdate-value))))
      (when (string-match pattern event-str)
        (match-string 1 event-str)))))

(defalias 'calendar-sync--parse-exdate #'calendar-sync--parse-ics-datetime
  "Parse EXDATE value. See `calendar-sync--parse-ics-datetime'.")

(defun calendar-sync--collect-exdates (event-str)
  "Collect all excluded dates from EVENT-STR, handling timezone conversion.
Returns list of parsed datetime lists (year month day hour minute).
Converts TZID-qualified and UTC times to local time."
  (if (or (null event-str)
          (not (stringp event-str))
          (string-empty-p event-str))
      '()
    (let ((exdate-values (calendar-sync--get-exdates event-str))
          (result '()))
      (dolist (exdate-value exdate-values)
        (let* ((exdate-line (calendar-sync--get-exdate-line event-str exdate-value))
               (exdate-tzid (and exdate-line (calendar-sync--extract-tzid exdate-line)))
               (exdate-is-utc (and exdate-value (string-suffix-p "Z" exdate-value)))
               (exdate-parsed (calendar-sync--parse-exdate exdate-value)))
          (when exdate-parsed
            (push (calendar-sync--localize-parsed-datetime
                   exdate-parsed exdate-is-utc exdate-tzid)
                  result))))
      (nreverse result))))

(defun calendar-sync--exdate-matches-p (occurrence-start exdate)
  "Check if OCCURRENCE-START matches EXDATE.
OCCURRENCE-START is (year month day hour minute).
EXDATE is (year month day hour minute) or (year month day nil nil) for date-only.
Date-only EXDATE matches any time on that day."
  (and occurrence-start exdate
       (= (nth 0 occurrence-start) (nth 0 exdate))  ; year
       (= (nth 1 occurrence-start) (nth 1 exdate))  ; month
       (= (nth 2 occurrence-start) (nth 2 exdate))  ; day
       ;; If EXDATE has nil hour/minute (date-only), match any time
       (or (null (nth 3 exdate))
           (and (nth 3 occurrence-start)
                (= (nth 3 occurrence-start) (nth 3 exdate))
                (= (or (nth 4 occurrence-start) 0) (or (nth 4 exdate) 0))))))

(defun calendar-sync--filter-exdates (occurrences exdates)
  "Filter OCCURRENCES list to remove entries matching EXDATES.
OCCURRENCES is list of event plists with :start key.
EXDATES is list of parsed datetime lists from `calendar-sync--collect-exdates'.
Returns filtered list with excluded dates removed."
  (if (or (null occurrences) (null exdates))
      (or occurrences '())
    (cl-remove-if
     (lambda (occurrence)
       (let ((occ-start (plist-get occurrence :start)))
         (cl-some (lambda (exdate)
                    (calendar-sync--exdate-matches-p occ-start exdate))
                  exdates)))
     occurrences)))

;;; .ics Parsing

(defun calendar-sync--split-events (ics-content)
  "Split ICS-CONTENT into individual VEVENT blocks.
Returns list of strings, each containing one VEVENT block."
  (let ((events '()))
    (with-temp-buffer
      (insert ics-content)
      (goto-char (point-min))
      (while (search-forward "BEGIN:VEVENT" nil t)
        (let ((start (match-beginning 0)))
          (when (search-forward "END:VEVENT" nil t)
            (push (buffer-substring-no-properties start (point)) events)))))
    (nreverse events)))

(defun calendar-sync--unfold-continuation (text value start)
  "Unfold RFC 5545 continuation lines from TEXT starting at START.
VALUE is the initial content to append to.  Continuation lines begin
with a space or tab after a newline.  Returns (unfolded-value . new-pos)."
  (while (and (< start (length text))
              (string-match "\n[ \t]\\([^\n]*\\)" text start)
              (= (match-beginning 0) start))
    (setq value (concat value (match-string 1 text)))
    (setq start (match-end 0)))
  (cons value start))

(defun calendar-sync--get-property (event property)
  "Extract PROPERTY value from EVENT string.
Handles property parameters (e.g., DTSTART;TZID=America/Chicago:value).
Handles multi-line values (lines starting with space).
Returns nil if property not found."
  (when (string-match (format "^%s[^:\n]*:\\(.*\\)$" (regexp-quote property)) event)
    (car (calendar-sync--unfold-continuation
          event (match-string 1 event) (match-end 0)))))

(defun calendar-sync--get-property-line (event property)
  "Extract full PROPERTY line from EVENT string, including parameters.
Returns the complete line like 'DTSTART;TZID=Europe/Lisbon:20260202T190000'.
Returns nil if property not found."
  (when (string-match (format "^\\(%s[^\n]*\\)$" (regexp-quote property)) event)
    (match-string 1 event)))

(defun calendar-sync--get-all-property-lines (event property)
  "Extract ALL lines matching PROPERTY from EVENT string.
Unlike `calendar-sync--get-property-line' which returns the first match,
this returns a list of all matching lines.  Handles continuation lines
\(lines starting with space or tab).
Returns nil if EVENT or PROPERTY is nil, or no matches found."
  (when (and event property (stringp event) (not (string-empty-p event)))
    (let ((lines '())
          (pattern (format "^%s[^\n]*" (regexp-quote property)))
          (pos 0))
      (while (string-match pattern event pos)
        (let* ((result (calendar-sync--unfold-continuation
                        event (match-string 0 event) (match-end 0)))
               (line (car result))
               (end (cdr result)))
          (push line lines)
          (setq pos (if (< end (length event)) (1+ end) end))))
      (nreverse lines))))

(defun calendar-sync--extract-cn (line)
  "Extract and dequote CN parameter from iCal LINE.
Returns the CN value string, or nil if not found."
  (when (string-match ";CN=\\([^;:]+\\)" line)
    (let ((cn (match-string 1 line)))
      (if (and (string-prefix-p "\"" cn) (string-suffix-p "\"" cn))
          (substring cn 1 -1)
        cn))))

(defun calendar-sync--extract-email (line)
  "Extract email address from mailto: value in iCal LINE.
Returns email string, or nil if not found."
  (when (string-match "mailto:\\([^>\n ]+\\)" line)
    (match-string 1 line)))

(defun calendar-sync--parse-attendee-line (line)
  "Parse single ATTENDEE LINE into plist.
Returns plist (:cn NAME :email EMAIL :partstat STATUS :role ROLE).
Returns nil for nil, empty, or malformed input."
  (when (and line (stringp line) (not (string-empty-p line))
             (string-match-p "^ATTENDEE" line))
    (let ((cn (calendar-sync--extract-cn line))
          (email (calendar-sync--extract-email line))
          (partstat nil)
          (role nil))
      (when (string-match ";PARTSTAT=\\([^;:]+\\)" line)
        (setq partstat (match-string 1 line)))
      (when (string-match ";ROLE=\\([^;:]+\\)" line)
        (setq role (match-string 1 line)))
      (when email
        (list :cn cn :email email :partstat partstat :role role)))))

(defun calendar-sync--find-user-status (attendees user-emails)
  "Find user's PARTSTAT from ATTENDEES list using USER-EMAILS.
ATTENDEES is list of plists from `calendar-sync--parse-attendee-line'.
USER-EMAILS is list of email strings to match against.
Returns lowercase status string (\"accepted\", \"declined\", etc.) or nil."
  (when (and attendees user-emails)
    (let ((user-emails-lower (mapcar #'downcase user-emails))
          (found nil))
      (cl-dolist (attendee attendees)
        (let ((attendee-email (downcase (or (plist-get attendee :email) ""))))
          (when (member attendee-email user-emails-lower)
            (let ((partstat (plist-get attendee :partstat)))
              (when partstat
                (setq found (downcase partstat))
                (cl-return found))))))
      found)))

(defun calendar-sync--parse-organizer (event-str)
  "Parse ORGANIZER property from EVENT-STR into plist.
Returns plist (:cn NAME :email EMAIL), or nil if no ORGANIZER found."
  (when (and event-str (stringp event-str))
    (let ((line (calendar-sync--get-property-line event-str "ORGANIZER")))
      (when line
        (let ((email (calendar-sync--extract-email line)))
          (when email
            (list :cn (calendar-sync--extract-cn line) :email email)))))))

(defun calendar-sync--extract-meeting-url (event-str)
  "Extract meeting URL from EVENT-STR.
Prefers X-GOOGLE-CONFERENCE over URL property.
Returns URL string or nil."
  (when (and event-str (stringp event-str))
    (or (calendar-sync--get-property event-str "X-GOOGLE-CONFERENCE")
        (calendar-sync--get-property event-str "URL"))))

(defun calendar-sync--extract-tzid (property-line)
  "Extract TZID parameter value from PROPERTY-LINE.
PROPERTY-LINE is like 'DTSTART;TZID=Europe/Lisbon:20260202T190000'.
Returns timezone string like 'Europe/Lisbon', or nil if no TZID.
Returns nil for malformed lines (missing colon separator)."
  (when (and property-line
             (stringp property-line)
             ;; Must have colon (property:value format)
             (string-match-p ":" property-line)
             (string-match ";TZID=\\([^;:]+\\)" property-line))
    (match-string 1 property-line)))

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

(defun calendar-sync--convert-tz-to-local (year month day hour minute source-tz)
  "Convert datetime from SOURCE-TZ timezone to local time.
SOURCE-TZ is a timezone name like 'Europe/Lisbon' or 'Asia/Yerevan'.
Returns list (year month day hour minute) in local timezone, or nil on error.

Uses Emacs built-in timezone support (encode-time/decode-time with ZONE
argument) for fast, subprocess-free conversion.  Uses the same system
TZ database as the `date' command."
  (when (and source-tz (not (string-empty-p source-tz)))
    (condition-case err
        (let* ((abs-time (encode-time 0 minute hour day month year source-tz))
               (local (decode-time abs-time)))
          (list (nth 5 local)    ; year
                (nth 4 local)    ; month
                (nth 3 local)    ; day
                (nth 2 local)    ; hour
                (nth 1 local)))  ; minute
      (error
       (cj/log-silently "calendar-sync: Error converting timezone %s: %s"
                        source-tz (error-message-string err))
       nil))))

(defun calendar-sync--localize-parsed-datetime (parsed is-utc tzid)
  "Convert PARSED datetime to local time using timezone info.
PARSED is (year month day hour minute) or (year month day nil nil).
IS-UTC non-nil means the value had a Z suffix.
TZID is a timezone string like \"Europe/Lisbon\", or nil.
Returns PARSED converted to local time, or PARSED unchanged if no conversion needed."
  (cond
   (is-utc
    (calendar-sync--convert-utc-to-local
     (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
     (or (nth 3 parsed) 0) (or (nth 4 parsed) 0) 0))
   (tzid
    (or (calendar-sync--convert-tz-to-local
         (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
         (or (nth 3 parsed) 0) (or (nth 4 parsed) 0)
         tzid)
        parsed))
   (t parsed)))

(defun calendar-sync--parse-timestamp (timestamp-str &optional tzid)
  "Parse iCal timestamp string TIMESTAMP-STR.
Returns (year month day hour minute) or (year month day) for all-day events.
Converts UTC times (ending in Z) to local time.
If TZID is provided (e.g., 'Europe/Lisbon'), converts from that timezone to local.
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
      (cond
       ;; UTC timestamp (Z suffix) - convert from UTC
       (is-utc
        (calendar-sync--convert-utc-to-local year month day hour minute second))
       ;; TZID provided - convert from that timezone
       (tzid
        (or (calendar-sync--convert-tz-to-local year month day hour minute tzid)
            ;; Fallback to raw time if conversion fails
            (list year month day hour minute)))
       ;; No timezone info - assume local time
       (t
        (list year month day hour minute)))))
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
  "Convert DATE to time value for comparison.
DATE should be a list starting with (year month day ...).
Only the first three elements are used; extra elements (hour, minute) are ignored."
  (let ((day (nth 2 date))
        (month (nth 1 date))
        (year (nth 0 date)))
    (encode-time 0 0 0 day month year)))

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

(defun calendar-sync--expand-simple-recurrence (base-event rrule range advance-fn)
  "Expand a simple (non-weekly) recurring event using ADVANCE-FN to step dates.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range.
ADVANCE-FN takes (current-date interval) and returns the next date."
  (let* ((start (plist-get base-event :start))
         (interval (plist-get rrule :interval))
         (until (plist-get rrule :until))
         (count (plist-get rrule :count))
         (occurrences '())
         (current-date (list (nth 0 start) (nth 1 start) (nth 2 start)))
         (num-generated 0)
         (range-end-time (cadr range)))
    (while (and (or count until (time-less-p (calendar-sync--date-to-time current-date) range-end-time))
                (or (not until) (calendar-sync--before-date-p current-date until))
                (or (not count) (< num-generated count)))
      (let ((occurrence-datetime (append current-date (nthcdr 3 start))))
        (setq num-generated (1+ num-generated))
        (when (calendar-sync--date-in-range-p occurrence-datetime range)
          (push (calendar-sync--create-occurrence base-event occurrence-datetime)
                occurrences)))
      (setq current-date (funcall advance-fn current-date interval)))
    (nreverse occurrences)))

(defun calendar-sync--expand-daily (base-event rrule range)
  "Expand daily recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range."
  (calendar-sync--expand-simple-recurrence
   base-event rrule range #'calendar-sync--add-days))

(defun calendar-sync--expand-weekly (base-event rrule range)
  "Expand weekly recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range."
  (let* ((start (plist-get base-event :start))
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
  (calendar-sync--expand-simple-recurrence
   base-event rrule range #'calendar-sync--add-months))

(defun calendar-sync--expand-yearly (base-event rrule range)
  "Expand yearly recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range."
  (calendar-sync--expand-simple-recurrence
   base-event rrule range
   (lambda (date interval) (calendar-sync--add-months date (* 12 interval)))))

(defun calendar-sync--expand-recurring-event (event-str range)
  "Expand recurring event EVENT-STR into individual occurrences within RANGE.
Returns list of event plists, or nil if not a recurring event.
Filters out dates excluded via EXDATE properties."
  (let ((rrule (calendar-sync--get-property event-str "RRULE")))
    (when rrule
      (let* ((base-event (calendar-sync--parse-event event-str))
             (parsed-rrule (calendar-sync--parse-rrule rrule))
             (freq (plist-get parsed-rrule :freq))
             (exdates (calendar-sync--collect-exdates event-str)))
        (when base-event
          (let ((occurrences
                 (pcase freq
                   ('daily (calendar-sync--expand-daily base-event parsed-rrule range))
                   ('weekly (calendar-sync--expand-weekly base-event parsed-rrule range))
                   ('monthly (calendar-sync--expand-monthly base-event parsed-rrule range))
                   ('yearly (calendar-sync--expand-yearly base-event parsed-rrule range))
                   (_ (cj/log-silently "calendar-sync: Unsupported RRULE frequency: %s" freq)
                      nil))))
            ;; Filter out EXDATE occurrences
            (if exdates
                (calendar-sync--filter-exdates occurrences exdates)
              occurrences)))))))

(defun calendar-sync--parse-event (event-str)
  "Parse single VEVENT string EVENT-STR into plist.
Returns plist with :uid :summary :description :location :start :end
:attendees :organizer :url :status.
Returns nil if event lacks required fields (DTSTART, SUMMARY).
Skips events with RECURRENCE-ID (individual instances of recurring events
are handled separately via exception collection).
Handles TZID-qualified timestamps by converting to local time.
Cleans text fields (description, location, summary) via `calendar-sync--clean-text'."
  ;; Skip individual instances of recurring events (they're collected as exceptions)
  (unless (calendar-sync--get-property event-str "RECURRENCE-ID")
    (let* ((uid (calendar-sync--get-property event-str "UID"))
           (summary (calendar-sync--clean-text
                     (calendar-sync--get-property event-str "SUMMARY")))
           (description (calendar-sync--clean-text
                         (calendar-sync--get-property event-str "DESCRIPTION")))
           (location (calendar-sync--clean-text
                      (calendar-sync--get-property event-str "LOCATION")))
           ;; Get raw property values
           (dtstart (calendar-sync--get-property event-str "DTSTART"))
           (dtend (calendar-sync--get-property event-str "DTEND"))
           ;; Extract TZID from property lines (if present)
           (dtstart-line (calendar-sync--get-property-line event-str "DTSTART"))
           (dtend-line (calendar-sync--get-property-line event-str "DTEND"))
           (start-tzid (calendar-sync--extract-tzid dtstart-line))
           (end-tzid (calendar-sync--extract-tzid dtend-line))
           ;; Extract attendees
           (attendee-lines (calendar-sync--get-all-property-lines event-str "ATTENDEE"))
           (attendees (delq nil (mapcar #'calendar-sync--parse-attendee-line attendee-lines)))
           ;; Extract organizer and URL
           (organizer (calendar-sync--parse-organizer event-str))
           (url (calendar-sync--extract-meeting-url event-str))
           ;; Determine user status from attendees
           (status (calendar-sync--find-user-status attendees calendar-sync-user-emails)))
      (when (and summary dtstart)
        (let ((start-parsed (calendar-sync--parse-timestamp dtstart start-tzid))
              (end-parsed (and dtend (calendar-sync--parse-timestamp dtend end-tzid))))
          (when start-parsed
            (list :uid uid
                  :summary summary
                  :description description
                  :location location
                  :start start-parsed
                  :end end-parsed
                  :attendees attendees
                  :organizer organizer
                  :url url
                  :status status)))))))

(defun calendar-sync--event-to-org (event)
  "Convert parsed EVENT plist to org entry string.
Produces property drawer with LOCATION, ORGANIZER, STATUS, URL when present.
Description appears as body text after the drawer."
  (let* ((summary (or (plist-get event :summary) "(No Title)"))
         (description (plist-get event :description))
         (location (plist-get event :location))
         (start (plist-get event :start))
         (end (plist-get event :end))
         (organizer (plist-get event :organizer))
         (status (plist-get event :status))
         (url (plist-get event :url))
         (timestamp (calendar-sync--format-timestamp start end))
         ;; Build property drawer entries
         (props '()))
    ;; Collect non-nil properties
    (when (and location (not (string-empty-p location)))
      (push (format ":LOCATION: %s" location) props))
    (when organizer
      (let ((org-name (or (plist-get organizer :cn)
                          (plist-get organizer :email))))
        (when org-name
          (push (format ":ORGANIZER: %s" org-name) props))))
    (when (and status (not (string-empty-p status)))
      (push (format ":STATUS: %s" status) props))
    (when (and url (not (string-empty-p url)))
      (push (format ":URL: %s" url) props))
    (setq props (nreverse props))
    ;; Build output
    (let ((parts (list timestamp (format "* %s" summary))))
      ;; Add property drawer if any properties exist
      (when props
        (push ":PROPERTIES:" parts)
        (dolist (prop props)
          (push prop parts))
        (push ":END:" parts))
      ;; Add description as body text (sanitized to prevent org heading conflicts)
      (when (and description (not (string-empty-p description)))
        (push (calendar-sync--sanitize-org-body description) parts))
      (string-join (nreverse parts) "\n"))))

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
Recurring events are expanded into individual occurrences.
RECURRENCE-ID exceptions are applied to override specific occurrences."
  (condition-case err
      (let* ((range (calendar-sync--get-date-range))
             (events (calendar-sync--split-events ics-content))
             ;; First pass: collect all RECURRENCE-ID exceptions
             (exceptions (calendar-sync--collect-recurrence-exceptions ics-content))
             (parsed-events '())
             (max-events 5000)  ; Safety limit to prevent Emacs from hanging
             (events-generated 0))
        ;; Process each event
        (dolist (event-str events)
          (when (< events-generated max-events)
            (let ((expanded (calendar-sync--expand-recurring-event event-str range)))
              (if expanded
                  ;; Recurring event - add all occurrences with exceptions applied
                  (let ((with-exceptions (calendar-sync--apply-recurrence-exceptions
                                          expanded exceptions)))
                    (setq parsed-events (append parsed-events with-exceptions))
                    (setq events-generated (+ events-generated (length with-exceptions))))
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
              (concat "# Calendar Events\n\n"
                      (string-join org-entries "\n\n")
                      "\n")
            nil)))
    (error
     (cj/log-silently "calendar-sync: Parse error: %s" (error-message-string err))
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
         :command (list "curl" "-s" "-L"
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
                            (cj/log-silently "calendar-sync: Fetch error: curl failed: %s" (string-trim event))
                            nil))))
                   (kill-buffer buf)
                   (funcall callback content))))))))
    (error
     (cj/log-silently "calendar-sync: Fetch error: %s" (error-message-string err))
     (funcall callback nil))))

(defun calendar-sync--write-file (content file)
  "Write CONTENT to FILE.
Creates parent directories if needed."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert content)))

;;; Debug Logging

(defun calendar-sync--debug-p ()
  "Return non-nil if calendar-sync debug logging is enabled.
Checks `cj/debug-modules' for symbol `calendar-sync' or t (all)."
  (and (boundp 'cj/debug-modules)
       (or (eq cj/debug-modules t)
           (memq 'calendar-sync cj/debug-modules))))

;;; Single Calendar Sync

(defun calendar-sync--sync-calendar (calendar)
  "Sync a single CALENDAR asynchronously.
CALENDAR is a plist with :name, :url, and :file keys.
Updates calendar state and saves to disk on completion.
Logs timing for each phase to *Messages* for performance diagnosis."
  (let ((name (plist-get calendar :name))
        (url (plist-get calendar :url))
        (file (plist-get calendar :file))
        (fetch-start (float-time)))
    ;; Mark as syncing
    (calendar-sync--set-calendar-state name '(:status syncing))
    (cj/log-silently "calendar-sync: [%s] Syncing..." name)
    (calendar-sync--fetch-ics
     url
     (lambda (ics-content)
       (let ((fetch-elapsed (- (float-time) fetch-start)))
         (if (null ics-content)
             (progn
               (cj/log-silently "calendar-sync: [%s] Fetch failed" name)
               (calendar-sync--set-calendar-state
                name
                (list :status 'error
                      :last-sync (plist-get (calendar-sync--get-calendar-state name) :last-sync)
                      :last-error "Fetch failed"))
               (calendar-sync--save-state)
               (message "calendar-sync: [%s] Sync failed (see *Messages*)" name))
           (when (calendar-sync--debug-p)
             (cj/log-silently "calendar-sync: [%s] Fetched %dKB in %.1fs"
                              name (/ (length ics-content) 1024) fetch-elapsed))
           (let* ((parse-start (float-time))
                  (org-content (calendar-sync--parse-ics ics-content))
                  (parse-elapsed (- (float-time) parse-start)))
             (if (null org-content)
                 (progn
                   (cj/log-silently "calendar-sync: [%s] Parse failed (%.1fs)" name parse-elapsed)
                   (calendar-sync--set-calendar-state
                    name
                    (list :status 'error
                          :last-sync (plist-get (calendar-sync--get-calendar-state name) :last-sync)
                          :last-error "Parse failed"))
                   (calendar-sync--save-state)
                   (message "calendar-sync: [%s] Sync failed (see *Messages*)" name))
               (when (calendar-sync--debug-p)
                 (cj/log-silently "calendar-sync: [%s] Parsed in %.1fs" name parse-elapsed))
               (let ((write-start (float-time)))
                 (calendar-sync--write-file org-content file)
                 (when (calendar-sync--debug-p)
                   (cj/log-silently "calendar-sync: [%s] Wrote %s in %.2fs"
                                    name (file-name-nondirectory file)
                                    (- (float-time) write-start))))
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
                          name total-elapsed file))))))))))

(defun calendar-sync--require-calendars ()
  "Return non-nil if calendars are configured, else warn and return nil."
  (or calendar-sync-calendars
      (progn (message "calendar-sync: No calendars configured (set calendar-sync-calendars)")
             nil)))

(defun calendar-sync--sync-all-calendars ()
  "Sync all configured calendars asynchronously.
Each calendar syncs in parallel."
  (when (calendar-sync--require-calendars)
    (message "calendar-sync: Syncing %d calendar(s)..." (length calendar-sync-calendars))
    (dolist (calendar calendar-sync-calendars)
      (calendar-sync--sync-calendar calendar))))

(defun calendar-sync--calendar-names ()
  "Return list of configured calendar names."
  (mapcar (lambda (cal) (plist-get cal :name)) calendar-sync-calendars))

(defun calendar-sync--get-calendar-by-name (name)
  "Find calendar plist by NAME, or nil if not found."
  (cl-find-if (lambda (cal) (string= (plist-get cal :name) name))
              calendar-sync-calendars))

;;;###autoload
(defun calendar-sync-now (&optional calendar-name)
  "Sync calendar(s) now asynchronously.
When called interactively, prompts to select a specific calendar or all.
When called non-interactively with CALENDAR-NAME, syncs that calendar.
When called non-interactively with nil, syncs all calendars."
  (interactive
   (list (when calendar-sync-calendars
           (let ((choices (cons "all" (calendar-sync--calendar-names))))
             (completing-read "Sync calendar: " choices nil t nil nil "all")))))
  (cond
   ((not (calendar-sync--require-calendars)) nil)
   ((or (null calendar-name) (string= calendar-name "all"))
    (calendar-sync--sync-all-calendars))
   (t
    (let ((calendar (calendar-sync--get-calendar-by-name calendar-name)))
      (if calendar
          (calendar-sync--sync-calendar calendar)
        (message "calendar-sync: Calendar '%s' not found" calendar-name))))))

;;;###autoload
(defun calendar-sync-status ()
  "Display sync status for all configured calendars."
  (interactive)
  (when (calendar-sync--require-calendars)
    (let ((status-lines '()))
      (dolist (calendar calendar-sync-calendars)
        (let* ((name (plist-get calendar :name))
               (file (plist-get calendar :file))
               (state (calendar-sync--get-calendar-state name))
               (status (or (plist-get state :status) 'never))
               (last-sync (plist-get state :last-sync))
               (last-error (plist-get state :last-error))
               (status-str
                (pcase status
                  ('ok (format "✓ %s" (if last-sync
                                          (format-time-string "%Y-%m-%d %H:%M" last-sync)
                                        "unknown")))
                  ('error (format "✗ %s" (or last-error "error")))
                  ('syncing "⟳ syncing...")
                  ('never "— never synced"))))
          (push (format "  %s: %s → %s" name status-str (abbreviate-file-name file))
                status-lines)))
      (message "calendar-sync status:\n%s"
               (string-join (nreverse status-lines) "\n")))))

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
  (calendar-sync--sync-all-calendars))

;;;###autoload
(defun calendar-sync-start ()
  "Start automatic calendar syncing.
Syncs all calendars immediately, then every `calendar-sync-interval-minutes'."
  (interactive)
  (when calendar-sync--timer
    (cancel-timer calendar-sync--timer))
  (when (calendar-sync--require-calendars)
    ;; Sync immediately
    (calendar-sync--sync-all-calendars)
    ;; Start timer for future syncs (convert minutes to seconds)
    (let ((interval-seconds (* calendar-sync-interval-minutes 60)))
      (setq calendar-sync--timer
            (run-at-time interval-seconds
                         interval-seconds
                         #'calendar-sync--sync-timer-function)))
    (message "calendar-sync: Auto-sync started (every %d minutes, %d calendars)"
             calendar-sync-interval-minutes
             (length calendar-sync-calendars))))

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
  "i" #'calendar-sync-status
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
      "C-; g i" "sync status"
      "C-; g t" "toggle auto-sync"
      "C-; g S" "start auto-sync"
      "C-; g x" "stop auto-sync")))

;;; Initialization

;; Load saved state from previous session
(calendar-sync--load-state)

;; Check for timezone change on startup
(when (and calendar-sync-calendars
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

;; Start auto-sync if enabled and calendars are configured
;; Syncs immediately then every calendar-sync-interval-minutes (default: 60 minutes)
(when (and calendar-sync-auto-start calendar-sync-calendars)
  (calendar-sync-start))

(provide 'calendar-sync)
;;; calendar-sync.el ends here
