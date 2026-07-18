;;; calendar-sync-recurrence.el --- RRULE / EXDATE / RECURRENCE-ID expansion -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-16

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: library.
;; Top-level side effects: none (defuns and defaliases only).
;; Runtime requires: cl-lib, subr-x, calendar-sync-ics.
;; Direct test load: yes (requires calendar-sync-ics explicitly).
;;
;; Recurrence layer of the calendar-sync parser: RECURRENCE-ID exception
;; collection and application, EXDATE exclusion, RRULE parsing, and
;; expansion of daily/weekly/monthly/yearly series into concrete
;; occurrences.  Builds on calendar-sync-ics for property extraction,
;; timestamp parsing, date arithmetic, and single-event parsing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar-sync-ics)

;; Configuration owned by calendar-sync.el; declared special here.
(defvar calendar-sync-user-emails)

;;; RECURRENCE-ID Exception Handling

(defun calendar-sync--get-recurrence-id (event-str)
  "Extract RECURRENCE-ID value from EVENT-STR.
Returns the datetime value (without TZID parameter), or nil if not found.
Handles both simple values and values with parameters like TZID."
  (when (and event-str (stringp event-str))
    (calendar-sync--get-property event-str "RECURRENCE-ID")))

(defun calendar-sync--get-recurrence-id-line (event-str)
  "Extract full RECURRENCE-ID line from EVENT-STR, including parameters.
Returns the complete line like
`RECURRENCE-ID;TZID=Europe/Tallinn:20260203T170000'.
Returns nil if not found."
  (when (and event-str (stringp event-str))
    (calendar-sync--get-property-line event-str "RECURRENCE-ID")))

(defalias 'calendar-sync--parse-recurrence-id #'calendar-sync--parse-ics-datetime
  "Parse RECURRENCE-ID value. See `calendar-sync--parse-ics-datetime'.")

(defun calendar-sync--parse-exception-event (event-str)
  "Parse a RECURRENCE-ID override EVENT-STR into an exception plist, or nil.
Returns nil when EVENT-STR carries no RECURRENCE-ID, or its recurrence-id /
start time fail to parse.  The plist holds :recurrence-id (localized),
:recurrence-id-raw, :start, :end, :summary, :description, :location,
:attendees.

:attendees is carried so `calendar-sync--apply-single-exception' can
re-derive the user's status when a single occurrence is declined: a
RECURRENCE-ID override is exactly how a calendar marks one occurrence of a
recurring series declined, and without the attendee block here the override
inherits the series' \"accepted\" status and the declined occurrence is never
dropped by `calendar-sync--filter-declined'."
  (let ((recurrence-id (calendar-sync--get-recurrence-id event-str)))
    (when recurrence-id
      (let* ((recurrence-id-line (calendar-sync--get-recurrence-id-line event-str))
             (recurrence-id-tzid (calendar-sync--extract-tzid recurrence-id-line))
             (recurrence-id-is-utc (string-suffix-p "Z" recurrence-id))
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
                        (calendar-sync--get-property event-str "LOCATION")))
             ;; Carry the override's attendee block so a singly-declined
             ;; occurrence can re-derive the user's status downstream.
             (attendee-lines (calendar-sync--get-all-property-lines event-str "ATTENDEE"))
             (attendees (delq nil (mapcar #'calendar-sync--parse-attendee-line
                                          attendee-lines))))
        (when (and recurrence-id-parsed start-parsed)
          (list :recurrence-id (calendar-sync--localize-parsed-datetime
                                recurrence-id-parsed recurrence-id-is-utc recurrence-id-tzid)
                :recurrence-id-raw recurrence-id
                ;; A cancelled override removes its occurrence downstream
                ;; rather than rescheduling it.
                :cancelled (calendar-sync--event-cancelled-p event-str)
                :start start-parsed
                :end end-parsed
                :summary summary
                :description description
                :location location
                :attendees attendees))))))

(defun calendar-sync--collect-recurrence-exceptions (ics-content)
  "Collect all RECURRENCE-ID events from ICS-CONTENT.
Returns hash table mapping UID to list of exception event plists.
Each exception plist contains :recurrence-id (parsed), :start, :end,
:summary, etc."
  (let ((exceptions (make-hash-table :test 'equal)))
    (when (and ics-content (stringp ics-content))
      (dolist (event-str (calendar-sync--split-events ics-content))
        (let ((uid (calendar-sync--get-property event-str "UID"))
              (exception-plist (calendar-sync--parse-exception-event event-str)))
          (when (and uid exception-plist)
            (puthash uid
                     (cons exception-plist (gethash uid exceptions))
                     exceptions)))))
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
      (plist-put result :attendees (plist-get exception :attendees))
      ;; Re-derive the user's status from the overridden attendees so a
      ;; singly-declined occurrence drops its inherited series "accepted"
      ;; (otherwise `calendar-sync--filter-declined' can't drop it). Leave the
      ;; inherited status when the override doesn't name the user.
      (let ((status (calendar-sync--find-user-status
                     (plist-get exception :attendees) calendar-sync-user-emails)))
        (when status
          (plist-put result :status status))))
    (when (plist-get exception :organizer)
      (plist-put result :organizer (plist-get exception :organizer)))
    (when (plist-get exception :url)
      (plist-put result :url (plist-get exception :url)))
    result))

(defun calendar-sync--apply-recurrence-exceptions (occurrences exceptions)
  "Apply EXCEPTIONS to OCCURRENCES list.
OCCURRENCES is list of event plists from RRULE expansion.
EXCEPTIONS is hash table from `calendar-sync--collect-recurrence-exceptions'.
Returns new list with matching occurrences replaced by exception times.
A cancelled exception (STATUS:CANCELLED override) removes its occurrence
from the list instead of overriding it."
  (if (or (null occurrences) (null exceptions))
      occurrences
    (delq nil
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
                   (cond
                    ((null matching-exception) occurrence)
                    ;; Cancelled instance: drop it entirely.
                    ((plist-get matching-exception :cancelled) nil)
                    (t (calendar-sync--apply-single-exception
                        occurrence matching-exception)))))))
           occurrences))))

;;; EXDATE (Excluded Date) Handling

(defun calendar-sync--get-exdates (event-str)
  "Extract all EXDATE values from EVENT-STR.
Returns list of datetime strings (without TZID parameters), or nil if
none found.
Handles both simple values and values with parameters like TZID."
  (when (and event-str (stringp event-str) (not (string-empty-p event-str)))
    (let ((exdates '())
          (pos 0))
      ;; Find all EXDATE lines.  One line may carry several comma-separated
      ;; datetimes (RFC 5545); split them so each is excluded individually.
      ;; Capture the match end BEFORE split-string: its internal matching
      ;; clobbers the match data, and reading (match-end 0) afterwards made
      ;; pos jump backwards to a comma offset inside the value -- re-matching
      ;; the same line forever and growing the list until the OOM killer
      ;; intervened (took two agent sessions down on 2026-07-13).
      (while (string-match "^EXDATE[^:\n]*:\\([^\n]+\\)" event-str pos)
        (let ((line-end (match-end 0)))
          (dolist (val (split-string (match-string 1 event-str) "," t))
            (push val exdates))
          (setq pos line-end)))
      (nreverse exdates))))

(defun calendar-sync--get-exdate-line (event-str exdate-value)
  "Find the full EXDATE line containing EXDATE-VALUE from EVENT-STR.
Returns the complete line like
`EXDATE;TZID=America/New_York:20260210T130000'.  Matches the value anywhere
in the value list, so a comma-separated line's shared TZID reaches every
value on it.  Returns nil if not found."
  (when (and event-str (stringp event-str) exdate-value)
    (let ((pattern (format "^EXDATE[^:\n]*:[^\n]*%s" (regexp-quote exdate-value))))
      (when (string-match pattern event-str)
        (match-string 0 event-str)))))

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
EXDATE is (year month day hour minute) or (year month day nil nil) for
date-only.
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

;;; RRULE Parsing and Expansion

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
Returns plist with :freq :interval :byday :bysetpos :bymonth :until :count.
BYMONTH keeps only the first value of a comma-separated list -- feeds in
practice emit a single month there."
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
            ("BYSETPOS" (setq result (plist-put result :bysetpos (string-to-number value))))
            ("BYMONTH" (setq result (plist-put result :bymonth (string-to-number value))))
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
                ;; UNTIL is inclusive (RFC 5545 3.3.10) -- on-or-before, not before.
                (or (not until) (calendar-sync--date-on-or-before-p current-date until))
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
                ;; UNTIL is inclusive (RFC 5545 3.3.10) -- on-or-before, not before.
                (or (not until) (calendar-sync--date-on-or-before-p current-date until)))
      (setq iterations (1+ iterations))
      ;; Generate occurrences for each weekday in this week
      (dolist (weekday weekdays)
        (let* ((current-weekday (calendar-sync--date-weekday current-date))
               (days-ahead (mod (- weekday current-weekday) 7))
               (occurrence-date (calendar-sync--add-days current-date days-ahead))
               (occurrence-datetime (append occurrence-date (nthcdr 3 start))))
          ;; Check UNTIL date first -- inclusive per RFC 5545 3.3.10.
          (when (or (not until) (calendar-sync--date-on-or-before-p occurrence-date until))
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
      (calendar-sync--log-silently "calendar-sync: WARNING: Hit max iterations (%d) expanding weekly event" max-iterations))
    (nreverse occurrences)))

(defun calendar-sync--parse-byday-entry (entry)
  "Parse a single RRULE BYDAY ENTRY into a cons (ORDINAL . WEEKDAY).
ENTRY is a string like \"2WE\" (2nd Wednesday), \"-1TU\" (last Tuesday),
or \"SU\" (bare weekday).  ORDINAL is nil for a bare weekday.  WEEKDAY is
1-7 (Monday = 1).  Returns nil for unparseable input."
  (when (and (stringp entry)
             (string-match "\\`\\(-?[0-9]+\\)?\\([A-Z][A-Z]\\)\\'" entry))
    (let ((ordinal (match-string 1 entry))
          (weekday (calendar-sync--weekday-to-number (match-string 2 entry))))
      (when weekday
        (cons (and ordinal (string-to-number ordinal)) weekday)))))

(defun calendar-sync--byday-days-in-month (year month byday-entries bysetpos)
  "Return the sorted day-of-month list BYDAY-ENTRIES select in YEAR/MONTH.
An entry with an ordinal (\"2WE\") resolves directly via
`calendar-sync--nth-weekday-of-month'.  A bare entry (\"SU\") expands to
every matching weekday in the month.  When BYSETPOS is non-nil it then
selects one day from the combined set (1-based; negative counts from the
end), per RFC 5545 3.8.5.3.  Months with no match return nil."
  (let ((days '()))
    (dolist (entry byday-entries)
      (let ((parsed (calendar-sync--parse-byday-entry entry)))
        (when parsed
          (let ((ordinal (car parsed))
                (weekday (cdr parsed)))
            (if ordinal
                (let ((day (calendar-sync--nth-weekday-of-month year month weekday ordinal)))
                  (when day (push day days)))
              (let ((n 1) day)
                (while (setq day (calendar-sync--nth-weekday-of-month year month weekday n))
                  (push day days)
                  (setq n (1+ n)))))))))
    (setq days (sort (delete-dups days) #'<))
    (if (and bysetpos days)
        (let* ((total (length days))
               (index (if (> bysetpos 0) bysetpos (+ total bysetpos 1))))
          (if (and (>= index 1) (<= index total))
              (list (nth (1- index) days))
            '()))
      days)))

(defun calendar-sync--expand-monthly-byday (base-event rrule range)
  "Expand a monthly nth-weekday (BYDAY) recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule (carrying :byday and
optionally :bysetpos), RANGE is date range.  Steps month by month from
DTSTART's month, landing each occurrence on the day its BYDAY rule selects
-- never on DTSTART's day-of-month."
  (let* ((start (plist-get base-event :start))
         (interval (plist-get rrule :interval))
         (byday (plist-get rrule :byday))
         (bysetpos (plist-get rrule :bysetpos))
         (until (plist-get rrule :until))
         (count (plist-get rrule :count))
         (occurrences '())
         (month-anchor (list (nth 0 start) (nth 1 start) 1))
         (start-day (nth 2 start))
         (first-month t)
         (num-generated 0)
         (range-end-time (cadr range))
         (max-iterations 1000)
         (iterations 0))
    (when (<= interval 0)
      (error "Invalid RRULE interval: %s (must be > 0)" interval))
    (while (and (< iterations max-iterations)
                (or count until
                    (time-less-p (calendar-sync--date-to-time month-anchor) range-end-time))
                (or (not count) (< num-generated count))
                ;; A month starting after UNTIL can't contain an occurrence
                ;; on-or-before it (UNTIL is inclusive, RFC 5545 3.3.10).
                (or (not until) (calendar-sync--date-on-or-before-p month-anchor until)))
      (setq iterations (1+ iterations))
      (dolist (day (calendar-sync--byday-days-in-month
                    (nth 0 month-anchor) (nth 1 month-anchor) byday bysetpos))
        (let* ((occurrence-date (list (nth 0 month-anchor) (nth 1 month-anchor) day))
               (occurrence-datetime (append occurrence-date (nthcdr 3 start))))
          ;; The series starts at DTSTART: skip earlier days in the first month.
          (unless (and first-month (< day start-day))
            (when (or (not until) (calendar-sync--date-on-or-before-p occurrence-date until))
              (when (or (not count) (< num-generated count))
                (setq num-generated (1+ num-generated))
                (when (calendar-sync--date-in-range-p occurrence-datetime range)
                  (push (calendar-sync--create-occurrence base-event occurrence-datetime)
                        occurrences)))))))
      (setq first-month nil)
      (setq month-anchor (calendar-sync--add-months month-anchor interval)))
    (when (>= iterations max-iterations)
      (calendar-sync--log-silently
       "calendar-sync: WARNING: Hit max iterations (%d) expanding monthly BYDAY event"
       max-iterations))
    (nreverse occurrences)))

(defun calendar-sync--expand-monthly (base-event rrule range)
  "Expand monthly recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range.
A rule with BYDAY (nth weekday, e.g. 2WE, -1TU, or SU with BYSETPOS)
expands via `calendar-sync--expand-monthly-byday'; a plain rule steps
DTSTART's day-of-month."
  (if (plist-get rrule :byday)
      (calendar-sync--expand-monthly-byday base-event rrule range)
    (calendar-sync--expand-simple-recurrence
     base-event rrule range #'calendar-sync--add-months)))

(defun calendar-sync--expand-yearly-byday (base-event rrule range)
  "Expand a yearly nth-weekday event (e.g. BYMONTH=3;BYDAY=2SU).
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range.
Reuses the monthly BYDAY expander with a 12-month step, anchored on
:bymonth (falling back to DTSTART's month)."
  (let* ((start (plist-get base-event :start))
         (month (or (plist-get rrule :bymonth) (nth 1 start)))
         (sched-event (plist-put (copy-sequence base-event) :start
                                 (append (list (nth 0 start) month (nth 2 start))
                                         (nthcdr 3 start))))
         (sched-rrule (plist-put (copy-sequence rrule) :interval
                                 (* 12 (or (plist-get rrule :interval) 1)))))
    (calendar-sync--expand-monthly-byday sched-event sched-rrule range)))

(defun calendar-sync--expand-yearly (base-event rrule range)
  "Expand yearly recurring event.
BASE-EVENT is the event plist, RRULE is parsed rrule, RANGE is date range.
A rule with BYDAY (the DST clock-change shape, BYMONTH=n;BYDAY=nWD)
expands via `calendar-sync--expand-yearly-byday'; a plain rule repeats
DTSTART's calendar date."
  (if (plist-get rrule :byday)
      (calendar-sync--expand-yearly-byday base-event rrule range)
    (calendar-sync--expand-simple-recurrence
     base-event rrule range
     (lambda (date interval) (calendar-sync--add-months date (* 12 interval))))))

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
                   (_ (calendar-sync--log-silently "calendar-sync: Unsupported RRULE frequency: %s" freq)
                      nil))))
            ;; Filter out EXDATE occurrences
            (if exdates
                (calendar-sync--filter-exdates occurrences exdates)
              occurrences)))))))

(provide 'calendar-sync-recurrence)
;;; calendar-sync-recurrence.el ends here
