;;; test-calendar-sync--collect-recurrence-exceptions.el --- Tests for exception collection  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--collect-recurrence-exceptions function.
;; Tests collecting all RECURRENCE-ID events from ICS content.
;; Following quality-engineer.org guidelines: one function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Test Data Helpers

(defun test-make-exception-event (uid recurrence-id summary start end)
  "Create a VEVENT with RECURRENCE-ID for testing.
UID is the base event UID. RECURRENCE-ID is the original occurrence date.
SUMMARY, START, END define the exception event."
  (concat "BEGIN:VEVENT\n"
          "UID:" uid "\n"
          "RECURRENCE-ID:" recurrence-id "\n"
          "SUMMARY:" summary "\n"
          "DTSTART:" (test-calendar-sync-ics-datetime start) "\n"
          "DTEND:" (test-calendar-sync-ics-datetime end) "\n"
          "END:VEVENT"))

(defun test-make-base-event-with-rrule (uid summary start end rrule)
  "Create a recurring VEVENT with RRULE for testing."
  (concat "BEGIN:VEVENT\n"
          "UID:" uid "\n"
          "SUMMARY:" summary "\n"
          "DTSTART:" (test-calendar-sync-ics-datetime start) "\n"
          "DTEND:" (test-calendar-sync-ics-datetime end) "\n"
          "RRULE:" rrule "\n"
          "END:VEVENT"))

;;; Normal Cases

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-normal-single-exception ()
  "Test collecting single exception event."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (ics (test-calendar-sync-make-ics
               (test-make-exception-event "event123@google.com"
                                          "20260203T090000Z"
                                          "Rescheduled Meeting"
                                          start end)))
         (exceptions (calendar-sync--collect-recurrence-exceptions ics)))
    ;; Should have one exception keyed by UID
    (should (hash-table-p exceptions))
    (should (gethash "event123@google.com" exceptions))))

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-normal-multiple-same-uid ()
  "Test collecting multiple exceptions for same recurring event."
  (let* ((start1 (test-calendar-sync-time-days-from-now 7 10 0))
         (end1 (test-calendar-sync-time-days-from-now 7 11 0))
         (start2 (test-calendar-sync-time-days-from-now 14 11 0))
         (end2 (test-calendar-sync-time-days-from-now 14 12 0))
         (ics (test-calendar-sync-make-ics
               (test-make-exception-event "weekly123@google.com"
                                          "20260203T090000Z"
                                          "Week 1 Rescheduled"
                                          start1 end1)
               (test-make-exception-event "weekly123@google.com"
                                          "20260210T090000Z"
                                          "Week 2 Rescheduled"
                                          start2 end2)))
         (exceptions (calendar-sync--collect-recurrence-exceptions ics)))
    ;; Should have list of exceptions under single UID
    (should (hash-table-p exceptions))
    (let ((uid-exceptions (gethash "weekly123@google.com" exceptions)))
      (should uid-exceptions)
      (should (= 2 (length uid-exceptions))))))

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-normal-multiple-uids ()
  "Test collecting exceptions for different recurring events."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (ics (test-calendar-sync-make-ics
               (test-make-exception-event "event-a@google.com"
                                          "20260203T090000Z"
                                          "Event A Rescheduled"
                                          start end)
               (test-make-exception-event "event-b@google.com"
                                          "20260203T140000Z"
                                          "Event B Rescheduled"
                                          start end)))
         (exceptions (calendar-sync--collect-recurrence-exceptions ics)))
    ;; Should have two UIDs
    (should (hash-table-p exceptions))
    (should (gethash "event-a@google.com" exceptions))
    (should (gethash "event-b@google.com" exceptions))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-boundary-no-exceptions ()
  "Test ICS with no RECURRENCE-ID events returns empty hash."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (ics (test-calendar-sync-make-ics
               (test-calendar-sync-make-vevent "Regular Event" start end)))
         (exceptions (calendar-sync--collect-recurrence-exceptions ics)))
    (should (hash-table-p exceptions))
    (should (= 0 (hash-table-count exceptions)))))

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-boundary-mixed-events ()
  "Test ICS with both regular and exception events."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (base-start (test-calendar-sync-time-days-from-now 1 9 0))
         (base-end (test-calendar-sync-time-days-from-now 1 10 0))
         (ics (test-calendar-sync-make-ics
               ;; Regular event (no RECURRENCE-ID)
               (test-calendar-sync-make-vevent "Normal Meeting" base-start base-end)
               ;; Base recurring event with RRULE
               (test-make-base-event-with-rrule "recurring@google.com"
                                                 "Weekly Sync"
                                                 base-start base-end
                                                 "FREQ=WEEKLY;COUNT=10")
               ;; Exception to the recurring event
               (test-make-exception-event "recurring@google.com"
                                          "20260210T090000Z"
                                          "Weekly Sync (Rescheduled)"
                                          start end)))
         (exceptions (calendar-sync--collect-recurrence-exceptions ics)))
    ;; Should only collect the exception, not regular or base events
    (should (hash-table-p exceptions))
    (should (= 1 (hash-table-count exceptions)))
    (should (gethash "recurring@google.com" exceptions))))

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-boundary-tzid-exception ()
  "Test exception event with TZID-qualified RECURRENCE-ID."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (event (concat "BEGIN:VEVENT\n"
                        "UID:tzid-event@google.com\n"
                        "RECURRENCE-ID;TZID=Europe/Tallinn:20260203T170000\n"
                        "SUMMARY:Tallinn Meeting Rescheduled\n"
                        "DTSTART:" (test-calendar-sync-ics-datetime start) "\n"
                        "DTEND:" (test-calendar-sync-ics-datetime end) "\n"
                        "END:VEVENT"))
         (ics (test-calendar-sync-make-ics event))
         (exceptions (calendar-sync--collect-recurrence-exceptions ics)))
    (should (hash-table-p exceptions))
    (should (gethash "tzid-event@google.com" exceptions))))

;;; Error Cases

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-error-empty-string ()
  "Test empty ICS content returns empty hash."
  (let ((exceptions (calendar-sync--collect-recurrence-exceptions "")))
    (should (hash-table-p exceptions))
    (should (= 0 (hash-table-count exceptions)))))

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-error-nil-input ()
  "Test nil input returns empty hash or nil."
  (let ((exceptions (calendar-sync--collect-recurrence-exceptions nil)))
    (should (or (null exceptions)
                (and (hash-table-p exceptions)
                     (= 0 (hash-table-count exceptions)))))))

(ert-deftest test-calendar-sync--collect-recurrence-exceptions-error-malformed-ics ()
  "Test malformed ICS handles gracefully."
  (let ((exceptions (calendar-sync--collect-recurrence-exceptions "not valid ics content")))
    ;; Should not crash, return empty hash
    (should (or (null exceptions)
                (and (hash-table-p exceptions)
                     (= 0 (hash-table-count exceptions)))))))

(provide 'test-calendar-sync--collect-recurrence-exceptions)
;;; test-calendar-sync--collect-recurrence-exceptions.el ends here
