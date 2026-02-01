;;; testutil-calendar-sync.el --- Test utilities for calendar-sync  -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for testing calendar-sync module, especially dynamic timestamp generation.
;; Following quality-engineer.org guidelines: no hardcoded dates!

;;; Code:

(require 'calendar)

;;; Test Environment Setup

;; Provide stub for cj/log-silently if not already defined
;; This function is defined in system-lib.el but tests should run standalone
(unless (fboundp 'cj/log-silently)
  (defun cj/log-silently (format-string &rest args)
    "Stub for testing: silently ignore log messages."
    nil))

;;; Dynamic Timestamp Generation

(defun test-calendar-sync-time-today-at (hour minute)
  "Generate time for today at HOUR:MINUTE.
Returns (year month day hour minute) list suitable for tests."
  (let* ((now (decode-time))
         (year (nth 5 now))
         (month (nth 4 now))
         (day (nth 3 now)))
    (list year month day hour minute)))

(defun test-calendar-sync-time-tomorrow-at (hour minute)
  "Generate time for tomorrow at HOUR:MINUTE."
  (let* ((tomorrow (time-add (current-time) (* 24 3600)))
         (decoded (decode-time tomorrow))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         (day (nth 3 decoded)))
    (list year month day hour minute)))

(defun test-calendar-sync-time-days-from-now (days hour minute)
  "Generate time for DAYS from now at HOUR:MINUTE."
  (let* ((future (time-add (current-time) (* days 24 3600)))
         (decoded (decode-time future))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         (day (nth 3 decoded)))
    (list year month day hour minute)))

(defun test-calendar-sync-time-days-ago (days hour minute)
  "Generate time for DAYS ago at HOUR:MINUTE."
  (let* ((past (time-subtract (current-time) (* days 24 3600)))
         (decoded (decode-time past))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         (day (nth 3 decoded)))
    (list year month day hour minute)))

(defun test-calendar-sync-time-date-only (offset-days)
  "Generate date-only timestamp for OFFSET-DAYS from now.
Returns (year month day) list for all-day events and UNTIL dates."
  (let* ((future (time-add (current-time) (* offset-days 24 3600)))
         (decoded (decode-time future))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         (day (nth 3 decoded)))
    (list year month day)))

(defun test-calendar-sync-time-date-only-ago (offset-days)
  "Generate date-only timestamp for OFFSET-DAYS ago.
Returns (year month day) list for UNTIL dates in the past."
  (let* ((past (time-subtract (current-time) (* offset-days 24 3600)))
         (decoded (decode-time past))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         (day (nth 3 decoded)))
    (list year month day)))

(defun test-calendar-sync-date-only-from-datetime (datetime)
  "Extract date-only (year month day) from DATETIME list.
DATETIME is (year month day hour minute).
Returns (year month day) suitable for UNTIL dates."
  (list (nth 0 datetime) (nth 1 datetime) (nth 2 datetime)))

;;; .ics Test Data Generation

(defun test-calendar-sync-ics-datetime (time-list)
  "Convert TIME-LIST to iCal DATETIME format.
TIME-LIST is (year month day hour minute).
Returns string like '20251116T140000Z'."
  (format "%04d%02d%02dT%02d%02d00Z"
          (nth 0 time-list)
          (nth 1 time-list)
          (nth 2 time-list)
          (nth 3 time-list)
          (nth 4 time-list)))

(defun test-calendar-sync-ics-date (time-list)
  "Convert TIME-LIST to iCal DATE format.
TIME-LIST is (year month day).
Returns string like '20251116'."
  (format "%04d%02d%02d"
          (nth 0 time-list)
          (nth 1 time-list)
          (nth 2 time-list)))

(defun test-calendar-sync-make-vevent (summary start end &optional description location)
  "Create a VEVENT block for testing.
START and END are time lists from test-calendar-sync-time-* functions.
Returns .ics formatted VEVENT string."
  (let* ((dtstart (if (= (length start) 5)
                      (test-calendar-sync-ics-datetime start)
                    (test-calendar-sync-ics-date start)))
         (dtend (when end
                  (if (= (length end) 5)
                      (test-calendar-sync-ics-datetime end)
                    (test-calendar-sync-ics-date end)))))
    (concat "BEGIN:VEVENT\n"
            "SUMMARY:" summary "\n"
            "DTSTART:" dtstart "\n"
            (when dtend (concat "DTEND:" dtend "\n"))
            (when description (concat "DESCRIPTION:" description "\n"))
            (when location (concat "LOCATION:" location "\n"))
            "END:VEVENT")))

(defun test-calendar-sync-make-ics (&rest events)
  "Create complete .ics file with EVENTS.
Each event should be a VEVENT string from `test-calendar-sync-make-vevent'."
  (concat "BEGIN:VCALENDAR\n"
          "VERSION:2.0\n"
          "PRODID:-//Test//Test//EN\n"
          (string-join events "\n")
          "\nEND:VCALENDAR"))

;;; Property Test Helpers

(defun test-calendar-sync-random-future-date ()
  "Generate random date 1-180 days in future with random time.
Returns (year month day hour minute) list."
  (test-calendar-sync-time-days-from-now
   (1+ (random 180))
   (random 24)
   (random 60)))

(defun test-calendar-sync-random-past-date ()
  "Generate random date 1-90 days in past with random time.
Returns (year month day hour minute) list."
  (test-calendar-sync-time-days-ago
   (1+ (random 90))
   (random 24)
   (random 60)))

(defun test-calendar-sync-random-weekday-subset ()
  "Generate random non-empty subset of weekdays.
Returns list of weekday strings like (\"MO\" \"WE\" \"FR\")."
  (let ((days '("MO" "TU" "WE" "TH" "FR" "SA" "SU"))
        (result '()))
    (dolist (day days)
      (when (zerop (random 2))
        (push day result)))
    ;; Ensure non-empty
    (or result (list (nth (random 7) days)))))

(defun test-calendar-sync-random-freq ()
  "Return random RRULE frequency symbol."
  (nth (random 4) '(daily weekly monthly yearly)))

(defun test-calendar-sync-days-between (date1 date2)
  "Calculate days between DATE1 and DATE2.
Both dates are (year month day ...) lists.
Returns float number of days (positive if date2 > date1)."
  (let ((t1 (calendar-sync--date-to-time (list (nth 0 date1) (nth 1 date1) (nth 2 date1))))
        (t2 (calendar-sync--date-to-time (list (nth 0 date2) (nth 1 date2) (nth 2 date2)))))
    (/ (float-time (time-subtract t2 t1)) 86400.0)))

(defun test-calendar-sync-wide-range ()
  "Generate wide date range: 90 days past to 365 days future.
Returns (start-time end-time) suitable for expansion functions."
  (list (time-subtract (current-time) (* 90 86400))
        (time-add (current-time) (* 365 86400))))

(defun test-calendar-sync-narrow-range ()
  "Generate narrow date range: today to 30 days future.
Returns (start-time end-time) suitable for expansion functions."
  (list (current-time)
        (time-add (current-time) (* 30 86400))))

(defun test-calendar-sync-date-to-time-value (date)
  "Convert DATE list to Emacs time value.
DATE is (year month day) or (year month day hour minute)."
  (let ((year (nth 0 date))
        (month (nth 1 date))
        (day (nth 2 date))
        (hour (or (nth 3 date) 0))
        (minute (or (nth 4 date) 0)))
    (encode-time 0 minute hour day month year)))

;;; Timezone Test Helpers

(defun test-calendar-sync-ics-datetime-local (time-list)
  "Convert TIME-LIST to iCal DATETIME format WITHOUT Z suffix.
TIME-LIST is (year month day hour minute).
Returns string like '20251116T140000' (no timezone, treated as local)."
  (format "%04d%02d%02dT%02d%02d00"
          (nth 0 time-list)
          (nth 1 time-list)
          (nth 2 time-list)
          (nth 3 time-list)
          (nth 4 time-list)))

(defun test-calendar-sync-ics-datetime-with-tzid (time-list tzid)
  "Convert TIME-LIST to iCal DTSTART with TZID parameter.
TIME-LIST is (year month day hour minute).
TZID is timezone string like \"Europe/Lisbon\".
Returns string like 'DTSTART;TZID=Europe/Lisbon:20260202T190000'."
  (format "DTSTART;TZID=%s:%04d%02d%02dT%02d%02d00"
          tzid
          (nth 0 time-list)
          (nth 1 time-list)
          (nth 2 time-list)
          (nth 3 time-list)
          (nth 4 time-list)))

(defun test-calendar-sync-make-vevent-with-tzid (summary start end tzid &optional description location)
  "Create a VEVENT block with TZID-qualified timestamps.
START and END are time lists (year month day hour minute).
TZID is timezone string like \"Europe/Lisbon\".
Returns .ics formatted VEVENT string."
  (let* ((dtstart-val (format "%04d%02d%02dT%02d%02d00"
                              (nth 0 start) (nth 1 start) (nth 2 start)
                              (nth 3 start) (nth 4 start)))
         (dtend-val (when end
                      (format "%04d%02d%02dT%02d%02d00"
                              (nth 0 end) (nth 1 end) (nth 2 end)
                              (nth 3 end) (nth 4 end)))))
    (concat "BEGIN:VEVENT\n"
            "SUMMARY:" summary "\n"
            "DTSTART;TZID=" tzid ":" dtstart-val "\n"
            (when dtend-val (concat "DTEND;TZID=" tzid ":" dtend-val "\n"))
            (when description (concat "DESCRIPTION:" description "\n"))
            (when location (concat "LOCATION:" location "\n"))
            "END:VEVENT")))

(defun test-calendar-sync-convert-tz-via-date (year month day hour minute source-tz)
  "Convert datetime from SOURCE-TZ to local time using date command.
Returns (year month day hour minute) in local timezone.
This is the reference implementation for verifying our conversion function."
  (let* ((date-input (format "%04d-%02d-%02d %02d:%02d" year month day hour minute))
         ;; Don't set TZ= prefix - let date use system local timezone as output
         ;; The TZ="source-tz" inside -d specifies the INPUT timezone
         (cmd (format "date -d 'TZ=\"%s\" %s' '+%%Y %%m %%d %%H %%M' 2>/dev/null"
                      source-tz
                      date-input))
         (result (string-trim (shell-command-to-string cmd)))
         (parts (split-string result " ")))
    (when (= 5 (length parts))
      (list (string-to-number (nth 0 parts))
            (string-to-number (nth 1 parts))
            (string-to-number (nth 2 parts))
            (string-to-number (nth 3 parts))
            (string-to-number (nth 4 parts))))))

(defun test-calendar-sync-local-tz-name ()
  "Get the local timezone name (e.g., 'America/Chicago').
Returns nil if unable to determine."
  (let ((tz (getenv "TZ")))
    (if (and tz (not (string-empty-p tz)))
        tz
      ;; Try to read from /etc/timezone or /etc/localtime
      (cond
       ((file-exists-p "/etc/timezone")
        (string-trim (with-temp-buffer
                       (insert-file-contents "/etc/timezone")
                       (buffer-string))))
       ((file-symlink-p "/etc/localtime")
        (let ((target (file-truename "/etc/localtime")))
          (when (string-match "/zoneinfo/\\(.+\\)$" target)
            (match-string 1 target))))
       (t nil)))))

(provide 'testutil-calendar-sync)
;;; testutil-calendar-sync.el ends here
