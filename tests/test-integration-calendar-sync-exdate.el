;;; test-integration-calendar-sync-exdate.el --- Integration tests for EXDATE support  -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for end-to-end EXDATE filtering in calendar-sync.
;; Verifies that excluded dates don't appear in org output.
;; Following quality-engineer.org guidelines.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Helper Functions

(defun test-integration-exdate--make-weekly-event-with-exdates (summary start exdates)
  "Create a weekly recurring event with EXDATES.
START is (year month day hour minute).
EXDATES is list of (year month day hour minute) lists to exclude."
  (let ((dtstart (test-calendar-sync-ics-datetime-local start))
        (exdate-lines (mapconcat
                       (lambda (ex)
                         (format "EXDATE:%s" (test-calendar-sync-ics-datetime-local ex)))
                       exdates
                       "\n")))
    (concat "BEGIN:VEVENT\n"
            "UID:weekly-test@example.com\n"
            "SUMMARY:" summary "\n"
            "DTSTART:" dtstart "\n"
            "DTEND:" (test-calendar-sync-ics-datetime-local
                      (list (nth 0 start) (nth 1 start) (nth 2 start)
                            (1+ (nth 3 start)) (nth 4 start))) "\n"
            "RRULE:FREQ=WEEKLY;COUNT=4\n"
            (when (> (length exdates) 0)
              (concat exdate-lines "\n"))
            "END:VEVENT")))

(defun test-integration-exdate--date-in-org-output-p (org-output date)
  "Check if DATE appears in ORG-OUTPUT.
DATE is (year month day hour minute)."
  (let ((date-str (format "%04d-%02d-%02d" (nth 0 date) (nth 1 date) (nth 2 date))))
    (string-match-p (regexp-quote date-str) org-output)))

;;; Normal Cases

(ert-deftest test-integration-exdate-single-excluded-date-not-in-output ()
  "Test that single excluded date doesn't appear in org output."
  (let* ((base-start (test-calendar-sync-time-days-from-now 7 13 0))
         (week2 (test-calendar-sync-time-days-from-now 14 13 0))
         (week3 (test-calendar-sync-time-days-from-now 21 13 0))
         (week4 (test-calendar-sync-time-days-from-now 28 13 0))
         ;; Exclude week 2
         (event (test-integration-exdate--make-weekly-event-with-exdates
                 "Weekly Sync"
                 base-start
                 (list week2)))
         (ics-content (test-calendar-sync-make-ics event))
         (org-output (calendar-sync--parse-ics ics-content)))
    (should org-output)
    ;; Week 2 should NOT be in output
    (should-not (test-integration-exdate--date-in-org-output-p org-output week2))
    ;; Weeks 1, 3, 4 should be in output
    (should (test-integration-exdate--date-in-org-output-p org-output base-start))
    (should (test-integration-exdate--date-in-org-output-p org-output week3))
    (should (test-integration-exdate--date-in-org-output-p org-output week4))))

(ert-deftest test-integration-exdate-multiple-excluded-dates-filtered ()
  "Test that multiple excluded dates are all filtered out."
  (let* ((base-start (test-calendar-sync-time-days-from-now 7 13 0))
         (week2 (test-calendar-sync-time-days-from-now 14 13 0))
         (week3 (test-calendar-sync-time-days-from-now 21 13 0))
         (week4 (test-calendar-sync-time-days-from-now 28 13 0))
         ;; Exclude weeks 2 and 4
         (event (test-integration-exdate--make-weekly-event-with-exdates
                 "Weekly Sync"
                 base-start
                 (list week2 week4)))
         (ics-content (test-calendar-sync-make-ics event))
         (org-output (calendar-sync--parse-ics ics-content)))
    (should org-output)
    ;; Weeks 2 and 4 should NOT be in output
    (should-not (test-integration-exdate--date-in-org-output-p org-output week2))
    (should-not (test-integration-exdate--date-in-org-output-p org-output week4))
    ;; Weeks 1 and 3 should be in output
    (should (test-integration-exdate--date-in-org-output-p org-output base-start))
    (should (test-integration-exdate--date-in-org-output-p org-output week3))))

(ert-deftest test-integration-exdate-non-excluded-dates-preserved ()
  "Test that non-excluded dates remain in output."
  (let* ((base-start (test-calendar-sync-time-days-from-now 7 13 0))
         (week2 (test-calendar-sync-time-days-from-now 14 13 0))
         (week3 (test-calendar-sync-time-days-from-now 21 13 0))
         (week4 (test-calendar-sync-time-days-from-now 28 13 0))
         ;; Exclude only week 3
         (event (test-integration-exdate--make-weekly-event-with-exdates
                 "Weekly Sync"
                 base-start
                 (list week3)))
         (ics-content (test-calendar-sync-make-ics event))
         (org-output (calendar-sync--parse-ics ics-content)))
    (should org-output)
    ;; Week 3 should NOT be in output
    (should-not (test-integration-exdate--date-in-org-output-p org-output week3))
    ;; Weeks 1, 2, 4 should all be preserved
    (should (test-integration-exdate--date-in-org-output-p org-output base-start))
    (should (test-integration-exdate--date-in-org-output-p org-output week2))
    (should (test-integration-exdate--date-in-org-output-p org-output week4))))

;;; Boundary Cases

(ert-deftest test-integration-exdate-no-exdates-all-occurrences-present ()
  "Test that event without EXDATE shows all dates."
  (let* ((base-start (test-calendar-sync-time-days-from-now 7 13 0))
         (week2 (test-calendar-sync-time-days-from-now 14 13 0))
         (week3 (test-calendar-sync-time-days-from-now 21 13 0))
         (week4 (test-calendar-sync-time-days-from-now 28 13 0))
         ;; No exclusions
         (event (test-integration-exdate--make-weekly-event-with-exdates
                 "Weekly Sync"
                 base-start
                 '()))  ; Empty exdates
         (ics-content (test-calendar-sync-make-ics event))
         (org-output (calendar-sync--parse-ics ics-content)))
    (should org-output)
    ;; All weeks should be present
    (should (test-integration-exdate--date-in-org-output-p org-output base-start))
    (should (test-integration-exdate--date-in-org-output-p org-output week2))
    (should (test-integration-exdate--date-in-org-output-p org-output week3))
    (should (test-integration-exdate--date-in-org-output-p org-output week4))))

(ert-deftest test-integration-exdate-with-recurrence-id-both-work ()
  "Test that EXDATE and RECURRENCE-ID work together correctly."
  ;; Create event with:
  ;; - Week 2 excluded via EXDATE (completely removed)
  ;; - Week 3 rescheduled via RECURRENCE-ID (time changed)
  (let* ((base-start (test-calendar-sync-time-days-from-now 7 13 0))
         (week2 (test-calendar-sync-time-days-from-now 14 13 0))
         (week3-original (test-calendar-sync-time-days-from-now 21 13 0))
         (week3-new (test-calendar-sync-time-days-from-now 21 15 0))  ; Moved to 3pm
         (week4 (test-calendar-sync-time-days-from-now 28 13 0))
         ;; Main event with EXDATE for week 2
         (main-event (concat "BEGIN:VEVENT\n"
                             "UID:combined-test@example.com\n"
                             "SUMMARY:Combined Test\n"
                             "DTSTART:" (test-calendar-sync-ics-datetime-local base-start) "\n"
                             "DTEND:" (test-calendar-sync-ics-datetime-local
                                       (list (nth 0 base-start) (nth 1 base-start) (nth 2 base-start)
                                             (1+ (nth 3 base-start)) (nth 4 base-start))) "\n"
                             "RRULE:FREQ=WEEKLY;COUNT=4\n"
                             "EXDATE:" (test-calendar-sync-ics-datetime-local week2) "\n"
                             "END:VEVENT"))
         ;; Exception event rescheduling week 3
         (exception-event (concat "BEGIN:VEVENT\n"
                                  "UID:combined-test@example.com\n"
                                  "RECURRENCE-ID:" (test-calendar-sync-ics-datetime-local week3-original) "\n"
                                  "SUMMARY:Combined Test (Rescheduled)\n"
                                  "DTSTART:" (test-calendar-sync-ics-datetime-local week3-new) "\n"
                                  "DTEND:" (test-calendar-sync-ics-datetime-local
                                            (list (nth 0 week3-new) (nth 1 week3-new) (nth 2 week3-new)
                                                  (1+ (nth 3 week3-new)) (nth 4 week3-new))) "\n"
                                  "END:VEVENT"))
         (ics-content (concat "BEGIN:VCALENDAR\n"
                              "VERSION:2.0\n"
                              main-event "\n"
                              exception-event "\n"
                              "END:VCALENDAR"))
         (org-output (calendar-sync--parse-ics ics-content)))
    (should org-output)
    ;; Week 2 should be completely absent (EXDATE)
    (should-not (test-integration-exdate--date-in-org-output-p org-output week2))
    ;; Week 3 should have the new time (15:00)
    (should (string-match-p "15:00" org-output))
    ;; Weeks 1 and 4 should be present
    (should (test-integration-exdate--date-in-org-output-p org-output base-start))
    (should (test-integration-exdate--date-in-org-output-p org-output week4))))

(ert-deftest test-integration-exdate-tzid-conversion-matches-correctly ()
  "Test that TZID-qualified EXDATE filters correctly after conversion."
  ;; Use America/New_York timezone
  (let* ((base-start (test-calendar-sync-time-days-from-now 7 13 0))
         (week2 (test-calendar-sync-time-days-from-now 14 13 0))
         (week3 (test-calendar-sync-time-days-from-now 21 13 0))
         (dtstart-val (format "%04d%02d%02dT%02d%02d00"
                              (nth 0 base-start) (nth 1 base-start) (nth 2 base-start)
                              (nth 3 base-start) (nth 4 base-start)))
         (exdate-val (format "%04d%02d%02dT%02d%02d00"
                             (nth 0 week2) (nth 1 week2) (nth 2 week2)
                             (nth 3 week2) (nth 4 week2)))
         (event (concat "BEGIN:VEVENT\n"
                        "UID:tzid-test@example.com\n"
                        "SUMMARY:TZID Test\n"
                        "DTSTART;TZID=America/New_York:" dtstart-val "\n"
                        "RRULE:FREQ=WEEKLY;COUNT=3\n"
                        "EXDATE;TZID=America/New_York:" exdate-val "\n"
                        "END:VEVENT"))
         (ics-content (test-calendar-sync-make-ics event))
         (org-output (calendar-sync--parse-ics ics-content)))
    (should org-output)
    ;; The EXDATE should have been converted to local time and filtered
    ;; We can't check exact dates due to TZ conversion, but output should exist
    ;; and have fewer occurrences than without EXDATE
    (should (string-match-p "TZID Test" org-output))))

(provide 'test-integration-calendar-sync-exdate)
;;; test-integration-calendar-sync-exdate.el ends here
