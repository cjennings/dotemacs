;;; testutil-calendar-sync.el --- Test utilities for calendar-sync  -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for testing calendar-sync module, especially dynamic timestamp generation.
;; Following quality-engineer.org guidelines: no hardcoded dates!

;;; Code:

(require 'calendar)

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
Returns (year month day) list for all-day events."
  (let* ((future (time-add (current-time) (* offset-days 24 3600)))
         (decoded (decode-time future))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         (day (nth 3 decoded)))
    (list year month day)))

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

(provide 'testutil-calendar-sync)
;;; testutil-calendar-sync.el ends here
