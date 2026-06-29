;;; calendar-sync-ics.el --- iCalendar parsing primitives for calendar-sync -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-16

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: library.
;; Top-level side effects: none (defuns plus one internal state defvar).
;; Runtime requires: cl-lib, subr-x.
;; Direct test load: yes.
;;
;; Base layer of the calendar-sync parser: RFC 5545 text cleaning, VEVENT
;; property extraction, attendee/organizer/URL parsing, timezone and
;; timestamp conversion, date arithmetic, and single-event parsing.  It has
;; no dependency on the other calendar-sync modules, so the recurrence, org,
;; and source layers build on it.  The sync-window and user-identity
;; configuration it reads is owned by calendar-sync.el and forward-declared
;; here so this base layer never requires the top module back.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Configuration owned by calendar-sync.el; declared special here so this
;; base module reads it without a back-require onto the top module.
(defvar calendar-sync-past-months)
(defvar calendar-sync-future-months)
(defvar calendar-sync-user-emails)
(defvar calendar-sync-skip-declined)

;;; Logging

(defun calendar-sync--log-silently (format-string &rest args)
  "Log FORMAT-STRING with ARGS without requiring the full config."
  (if (fboundp 'cj/log-silently)
      (apply #'cj/log-silently format-string args)
    (apply #'message format-string args)))

;;; Internal state

(defvar calendar-sync--last-timezone-offset nil
  "Timezone offset in seconds from UTC at last sync.
Used to detect timezone changes (e.g., when traveling).")

;;; Timezone Detection

(defun calendar-sync--current-timezone-offset ()
  "Get current timezone offset in seconds from UTC.
Returns negative for west of UTC, positive for east.
Example: -21600 for CST (UTC-6), -28800 for PST (UTC-8)."
  (car (current-time-zone)))

(defun calendar-sync--format-timezone-offset (offset)
  "Format timezone OFFSET (in seconds) as human-readable string.
Example: -21600 → `UTC-6' or `UTC-6:00'."
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

;;; Line Ending Normalization

(defun calendar-sync--normalize-line-endings (content)
  "Normalize line endings in CONTENT to Unix format (LF only).
Removes all carriage return characters (\\r) from CONTENT.
The iCalendar format (RFC 5545) uses CRLF line endings, but Emacs
and `org-mode' expect LF only. This function ensures consistent line
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
    (if (= dow 0) 7 dow)))

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

(defun calendar-sync--date-to-time (date)
  "Convert DATE to time value for comparison.
DATE should be a list starting with (year month day ...).
Only the first three elements are used; extra elements (hour, minute) are
ignored."
  (let ((day (nth 2 date))
        (month (nth 1 date))
        (year (nth 0 date)))
    (encode-time 0 0 0 day month year)))

(defun calendar-sync--before-date-p (date1 date2)
  "Return t if DATE1 is before DATE2.
Both dates should be lists like (year month day)."
  (time-less-p (calendar-sync--date-to-time date1)
               (calendar-sync--date-to-time date2)))

;;; Datetime Parsing

(defun calendar-sync--parse-ics-datetime (value)
  "Parse iCal datetime VALUE into (year month day hour minute) list.
Returns nil for invalid input. For date-only values, returns
(year month day nil nil).
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

;;; .ics Property Extraction

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
Returns the complete line like
`DTSTART;TZID=Europe/Lisbon:20260202T190000'.
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

(defun calendar-sync--filter-declined (events)
  "Return EVENTS with declined entries removed when the toggle is on.
EVENTS is a list of plists produced by `calendar-sync--parse-event'.
Each plist's :status is the lowercase PARTSTAT for the user (set by
`calendar-sync--find-user-status'), or nil for events without an
attendee block. Drops only events whose :status is exactly the string
\"declined\" so that nil / accepted / tentative / needs-action all
survive. When `calendar-sync-skip-declined' is nil, returns EVENTS
unchanged."
  (if (and calendar-sync-skip-declined events)
      (cl-remove-if (lambda (event)
                      (equal (plist-get event :status) "declined"))
                    events)
    events))

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
PROPERTY-LINE is like `DTSTART;TZID=Europe/Lisbon:20260202T190000'.
Returns timezone string like `Europe/Lisbon', or nil if no TZID.
Returns nil for malformed lines (missing colon separator)."
  (when (and property-line
             (stringp property-line)
             ;; Must have colon (property:value format)
             (string-match-p ":" property-line)
             (string-match ";TZID=\\([^;:]+\\)" property-line))
    (match-string 1 property-line)))

;;; Timezone / Timestamp Conversion

(defun calendar-sync--convert-utc-to-local (year month day hour minute second)
  "Convert UTC datetime to local time.
Returns list (year month day hour minute) in local timezone."
  (let* ((utc-time (encode-time second minute hour day month year 0))
         (local-time (decode-time utc-time)))
    (list (nth 5 local-time)  ; year
          (nth 4 local-time)  ; month
          (nth 3 local-time)  ; day
          (nth 2 local-time)  ; hour
          (nth 1 local-time))))

(defun calendar-sync--convert-tz-to-local (year month day hour minute source-tz)
  "Convert datetime from SOURCE-TZ timezone to local time.
SOURCE-TZ is a timezone name like `Europe/Lisbon' or `Asia/Yerevan'.
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
       (calendar-sync--log-silently "calendar-sync: Error converting timezone %s: %s"
                        source-tz (error-message-string err))
       nil))))

(defun calendar-sync--localize-parsed-datetime (parsed is-utc tzid)
  "Convert PARSED datetime to local time using timezone info.
PARSED is (year month day hour minute) or (year month day nil nil).
IS-UTC non-nil means the value had a Z suffix.

TZID is a timezone string like \"Europe/Lisbon\", or nil.
Returns PARSED converted to local time, or PARSED unchanged if no
conversion needed."
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
If TZID is provided (e.g., `Europe/Lisbon'), converts from that timezone
to local.
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

;;; Single Event Parsing

(defun calendar-sync--parse-event (event-str)
  "Parse single VEVENT string EVENT-STR into plist.
Returns plist with :uid :summary :description :location :start :end
:attendees :organizer :url :status.
Returns nil if event lacks required fields (DTSTART, SUMMARY).
Skips events with RECURRENCE-ID (individual instances of recurring events
are handled separately via exception collection).
Handles TZID-qualified timestamps by converting to local time.
Cleans text fields (description, location, summary) via
`calendar-sync--clean-text'."
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

(provide 'calendar-sync-ics)
;;; calendar-sync-ics.el ends here
