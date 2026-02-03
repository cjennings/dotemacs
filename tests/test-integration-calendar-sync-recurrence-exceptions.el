;;; test-integration-calendar-sync-recurrence-exceptions.el --- Integration tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for RECURRENCE-ID exception handling workflow.
;; Tests the complete pipeline: ICS parsing → RRULE expansion → exception application.
;; Tests verify the final org output contains correct relative times.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Test Data - Using local times (no Z suffix) to avoid timezone conversion

(defun test-integration-make-recurring-event-with-exception-local ()
  "Create ICS content with local times (no Z suffix).
Weekly meeting with one instance rescheduled from 09:00 to 10:00."
  (let* ((base-start (test-calendar-sync-time-days-from-now 0 9 0))
         (base-end (test-calendar-sync-time-days-from-now 0 10 0))
         ;; Exception: week 2 moved from 9:00 to 10:00
         (exception-start (test-calendar-sync-time-days-from-now 7 10 0))
         (exception-end (test-calendar-sync-time-days-from-now 7 11 0))
         (recurrence-id-date (test-calendar-sync-time-days-from-now 7 9 0)))
    (concat "BEGIN:VCALENDAR\n"
            "VERSION:2.0\n"
            "PRODID:-//Test//Test//EN\n"
            ;; Base recurring event - use local time (no Z)
            "BEGIN:VEVENT\n"
            "UID:weekly-meeting@google.com\n"
            "SUMMARY:Weekly Team Sync\n"
            "DTSTART:" (test-calendar-sync-ics-datetime-local base-start) "\n"
            "DTEND:" (test-calendar-sync-ics-datetime-local base-end) "\n"
            "RRULE:FREQ=WEEKLY;COUNT=4\n"
            "END:VEVENT\n"
            ;; Exception event - local time
            "BEGIN:VEVENT\n"
            "UID:weekly-meeting@google.com\n"
            "RECURRENCE-ID:" (test-calendar-sync-ics-datetime-local recurrence-id-date) "\n"
            "SUMMARY:Weekly Team Sync (Rescheduled)\n"
            "DTSTART:" (test-calendar-sync-ics-datetime-local exception-start) "\n"
            "DTEND:" (test-calendar-sync-ics-datetime-local exception-end) "\n"
            "END:VEVENT\n"
            "END:VCALENDAR")))

;;; Integration Tests

(ert-deftest test-integration-recurrence-exception-org-output ()
  "Test that parse-ics returns valid org content with exception summary."
  (let* ((ics-content (test-integration-make-recurring-event-with-exception-local))
         (org-output (calendar-sync--parse-ics ics-content)))
    ;; Should return string with org content
    (should (stringp org-output))
    (should (string-match-p "Weekly Team Sync" org-output))
    ;; Should contain the rescheduled event with correct summary
    (should (string-match-p "Rescheduled" org-output))))

(ert-deftest test-integration-recurrence-exception-times-in-output ()
  "Test that rescheduled event shows different time than regular occurrences."
  (let* ((ics-content (test-integration-make-recurring-event-with-exception-local))
         (org-output (calendar-sync--parse-ics ics-content)))
    ;; The org output should contain both 09:00 (regular) and 10:00 (rescheduled)
    (should (string-match-p "09:00" org-output))
    (should (string-match-p "10:00" org-output))))

(ert-deftest test-integration-recurrence-no-exceptions-org-output ()
  "Test recurring event without exceptions produces correct org output."
  (let* ((base-start (test-calendar-sync-time-days-from-now 0 9 0))
         (base-end (test-calendar-sync-time-days-from-now 0 10 0))
         (ics-content (concat "BEGIN:VCALENDAR\n"
                              "VERSION:2.0\n"
                              "BEGIN:VEVENT\n"
                              "UID:simple-weekly@google.com\n"
                              "SUMMARY:Simple Weekly\n"
                              "DTSTART:" (test-calendar-sync-ics-datetime-local base-start) "\n"
                              "DTEND:" (test-calendar-sync-ics-datetime-local base-end) "\n"
                              "RRULE:FREQ=WEEKLY;COUNT=3\n"
                              "END:VEVENT\n"
                              "END:VCALENDAR"))
         (org-output (calendar-sync--parse-ics ics-content)))
    ;; Should have org content
    (should (stringp org-output))
    ;; Should have the event title
    (should (string-match-p "Simple Weekly" org-output))
    ;; All occurrences should be at 09:00 (no exceptions)
    (should (string-match-p "09:00-10:00" org-output))))

(ert-deftest test-integration-collect-exceptions-from-ics ()
  "Test that collect-recurrence-exceptions correctly extracts exceptions."
  (let* ((ics-content (test-integration-make-recurring-event-with-exception-local))
         (exceptions (calendar-sync--collect-recurrence-exceptions ics-content)))
    ;; Should have collected one exception
    (should (hash-table-p exceptions))
    (should (gethash "weekly-meeting@google.com" exceptions))
    ;; The exception should exist
    (let ((exc-list (gethash "weekly-meeting@google.com" exceptions)))
      (should (= 1 (length exc-list)))
      ;; The exception should have the rescheduled time (10:00)
      (let* ((exc (car exc-list))
             (exc-start (plist-get exc :start)))
        (should (= 10 (nth 3 exc-start)))))))

(ert-deftest test-integration-multiple-events-with-exceptions ()
  "Test multiple recurring events each with their own exceptions."
  (let* ((start-a (test-calendar-sync-time-days-from-now 0 9 0))
         (end-a (test-calendar-sync-time-days-from-now 0 10 0))
         (start-b (test-calendar-sync-time-days-from-now 0 14 0))
         (end-b (test-calendar-sync-time-days-from-now 0 15 0))
         ;; Exceptions for week 2: A moves from 9:00→10:00, B moves from 14:00→15:00
         (exc-a-start (test-calendar-sync-time-days-from-now 7 10 0))
         (exc-a-end (test-calendar-sync-time-days-from-now 7 11 0))
         (exc-b-start (test-calendar-sync-time-days-from-now 7 15 0))
         (exc-b-end (test-calendar-sync-time-days-from-now 7 16 0))
         (rec-id-a (test-calendar-sync-time-days-from-now 7 9 0))
         (rec-id-b (test-calendar-sync-time-days-from-now 7 14 0))
         (ics-content
          (concat "BEGIN:VCALENDAR\n"
                  "VERSION:2.0\n"
                  ;; Event A base
                  "BEGIN:VEVENT\n"
                  "UID:event-a@google.com\n"
                  "SUMMARY:Morning Standup\n"
                  "DTSTART:" (test-calendar-sync-ics-datetime-local start-a) "\n"
                  "DTEND:" (test-calendar-sync-ics-datetime-local end-a) "\n"
                  "RRULE:FREQ=WEEKLY;COUNT=2\n"
                  "END:VEVENT\n"
                  ;; Event A exception
                  "BEGIN:VEVENT\n"
                  "UID:event-a@google.com\n"
                  "RECURRENCE-ID:" (test-calendar-sync-ics-datetime-local rec-id-a) "\n"
                  "SUMMARY:Morning Standup (Moved)\n"
                  "DTSTART:" (test-calendar-sync-ics-datetime-local exc-a-start) "\n"
                  "DTEND:" (test-calendar-sync-ics-datetime-local exc-a-end) "\n"
                  "END:VEVENT\n"
                  ;; Event B base
                  "BEGIN:VEVENT\n"
                  "UID:event-b@google.com\n"
                  "SUMMARY:Afternoon Review\n"
                  "DTSTART:" (test-calendar-sync-ics-datetime-local start-b) "\n"
                  "DTEND:" (test-calendar-sync-ics-datetime-local end-b) "\n"
                  "RRULE:FREQ=WEEKLY;COUNT=2\n"
                  "END:VEVENT\n"
                  ;; Event B exception
                  "BEGIN:VEVENT\n"
                  "UID:event-b@google.com\n"
                  "RECURRENCE-ID:" (test-calendar-sync-ics-datetime-local rec-id-b) "\n"
                  "SUMMARY:Afternoon Review (Moved)\n"
                  "DTSTART:" (test-calendar-sync-ics-datetime-local exc-b-start) "\n"
                  "DTEND:" (test-calendar-sync-ics-datetime-local exc-b-end) "\n"
                  "END:VEVENT\n"
                  "END:VCALENDAR"))
         (org-output (calendar-sync--parse-ics ics-content)))
    (should (stringp org-output))
    ;; Should have both events
    (should (string-match-p "Morning Standup" org-output))
    (should (string-match-p "Afternoon Review" org-output))
    ;; Should have the "(Moved)" versions
    (should (string-match-p "Moved" org-output))
    ;; Should have rescheduled times (10:00 and 15:00)
    (should (string-match-p "10:00" org-output))
    (should (string-match-p "15:00" org-output))))

(provide 'test-integration-calendar-sync-recurrence-exceptions)
;;; test-integration-calendar-sync-recurrence-exceptions.el ends here
