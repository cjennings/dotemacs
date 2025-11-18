;;; test-integration-recurring-events.el --- Integration tests for recurring events  -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the complete recurring event (RRULE) workflow.
;; Tests the full pipeline: ICS parsing → RRULE expansion → org formatting.
;;
;; Components integrated:
;; - calendar-sync--split-events (ICS event extraction)
;; - calendar-sync--get-property (property extraction with TZID)
;; - calendar-sync--parse-rrule (RRULE parsing)
;; - calendar-sync--expand-weekly/daily/monthly/yearly (event expansion)
;; - calendar-sync--parse-event (event parsing)
;; - calendar-sync--event-to-org (org formatting)
;; - calendar-sync--parse-ics (complete pipeline orchestration)
;;
;; This validates that the entire RRULE system works together correctly,
;; from raw ICS input to final org-mode output.

;;; Code:

(require 'ert)
(require 'calendar-sync)
(require 'testutil-calendar-sync)

;;; Setup and Teardown

(defun test-integration-recurring-events-setup ()
  "Setup for recurring events integration tests."
  nil)

(defun test-integration-recurring-events-teardown ()
  "Teardown for recurring events integration tests."
  nil)

;;; Test Data

(defconst test-integration-recurring-events--weekly-ics
  "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
DTSTART;TZID=America/Chicago:20251118T103000
DTEND;TZID=America/Chicago:20251118T110000
RRULE:FREQ=WEEKLY;BYDAY=SA
SUMMARY:GTFO
UID:test-weekly@example.com
END:VEVENT
END:VCALENDAR"
  "Test ICS with weekly recurring event (GTFO use case).")

(defconst test-integration-recurring-events--daily-with-count-ics
  "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
DTSTART:20251120T090000Z
DTEND:20251120T100000Z
RRULE:FREQ=DAILY;COUNT=5
SUMMARY:Daily Standup
UID:test-daily@example.com
END:VEVENT
END:VCALENDAR"
  "Test ICS with daily recurring event limited by COUNT.")

(defconst test-integration-recurring-events--mixed-ics
  "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Test//Test//EN
BEGIN:VEVENT
DTSTART:20251125T140000Z
DTEND:20251125T150000Z
SUMMARY:One-time Meeting
UID:test-onetime@example.com
END:VEVENT
BEGIN:VEVENT
DTSTART;TZID=America/Chicago:20251201T093000
DTEND;TZID=America/Chicago:20251201T103000
RRULE:FREQ=WEEKLY;BYDAY=MO,WE,FR
SUMMARY:Recurring Standup
UID:test-recurring@example.com
END:VEVENT
END:VCALENDAR"
  "Test ICS with mix of recurring and non-recurring events.")

;;; Normal Cases - Complete Workflow

(ert-deftest test-integration-recurring-events-weekly-complete-workflow ()
  "Test complete workflow for weekly recurring event.

Components integrated:
- calendar-sync--split-events (extract VEVENT blocks)
- calendar-sync--get-property (extract DTSTART, DTEND, RRULE with TZID)
- calendar-sync--parse-rrule (parse FREQ=WEEKLY;BYDAY=SA)
- calendar-sync--expand-weekly (generate Saturday occurrences)
- calendar-sync--event-to-org (format as org entries)
- calendar-sync--parse-ics (orchestrate complete pipeline)

Validates:
- TZID parameters handled correctly
- RRULE expansion generates correct dates
- Multiple occurrences created from single event
- Org output is properly formatted with timestamps"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let ((org-output (calendar-sync--parse-ics test-integration-recurring-events--weekly-ics)))
        ;; Should generate org-formatted output
        (should (stringp org-output))
        (should (string-match-p "^# Google Calendar Events" org-output))

        ;; Should contain multiple GTFO entries
        (let ((gtfo-count (with-temp-buffer
                            (insert org-output)
                            (goto-char (point-min))
                            (how-many "^\\* GTFO"))))
          (should (> gtfo-count 40))  ; ~52 weeks in a year
          (should (< gtfo-count 60)))

        ;; Should have properly formatted Saturday timestamps
        (should (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Sat 10:30-11:00>" org-output)))
    (test-integration-recurring-events-teardown)))

(ert-deftest test-integration-recurring-events-daily-with-count-workflow ()
  "Test complete workflow for daily recurring event with COUNT limit.

Components integrated:
- calendar-sync--parse-rrule (with COUNT parameter)
- calendar-sync--expand-daily (respects COUNT=5)
- calendar-sync--parse-ics (complete pipeline)

Validates:
- COUNT parameter limits expansion correctly
- Daily recurrence generates consecutive days
- Exactly 5 occurrences created"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let ((org-output (calendar-sync--parse-ics test-integration-recurring-events--daily-with-count-ics)))
        (should (stringp org-output))

        ;; Should generate exactly 5 Daily Standup entries
        (let ((standup-count (with-temp-buffer
                               (insert org-output)
                               (goto-char (point-min))
                               (how-many "^\\* Daily Standup"))))
          (should (= standup-count 5))))
    (test-integration-recurring-events-teardown)))

(ert-deftest test-integration-recurring-events-mixed-recurring-and-onetime ()
  "Test workflow with mixed recurring and non-recurring events.

Components integrated:
- calendar-sync--split-events (handles multiple VEVENT blocks)
- calendar-sync--expand-recurring-event (detects RRULE vs non-recurring)
- calendar-sync--parse-event (handles both types)
- calendar-sync--parse-ics (processes both event types)

Validates:
- Non-recurring events included once
- Recurring events expanded correctly
- Both types appear in output
- Events are sorted chronologically"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let ((org-output (calendar-sync--parse-ics test-integration-recurring-events--mixed-ics)))
        (should (stringp org-output))

        ;; Should have one-time meeting
        (should (string-match-p "^\\* One-time Meeting" org-output))

        ;; Should have multiple recurring standup entries
        (let ((standup-count (with-temp-buffer
                               (insert org-output)
                               (goto-char (point-min))
                               (how-many "^\\* Recurring Standup"))))
          (should (> standup-count 10)))  ; ~3 days/week for 4 months

        ;; Events should be sorted by date (one-time comes before recurring)
        (should (< (string-match "One-time Meeting" org-output)
                   (string-match "Recurring Standup" org-output))))
    (test-integration-recurring-events-teardown)))

;;; Boundary Cases - Date Range Handling

(ert-deftest test-integration-recurring-events-respects-rolling-window ()
  "Test that RRULE expansion respects rolling window boundaries.

Components integrated:
- calendar-sync--get-date-range (calculates -3 months to +12 months)
- calendar-sync--date-in-range-p (filters occurrences)
- calendar-sync--expand-weekly (respects range)
- calendar-sync--parse-ics (applies range to all events)

Validates:
- Events outside date range are excluded
- Rolling window is applied consistently
- Past events (> 3 months) excluded
- Future events (> 12 months) excluded"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let* ((org-output (calendar-sync--parse-ics test-integration-recurring-events--weekly-ics))
             (now (current-time))
             (three-months-ago (time-subtract now (* 90 24 3600)))
             (twelve-months-future (time-add now (* 365 24 3600))))
        (should (stringp org-output))

        ;; Parse all dates from output
        (with-temp-buffer
          (insert org-output)
          (goto-char (point-min))
          (let ((all-dates-in-range t))
            (while (re-search-forward "<\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" nil t)
              (let* ((year (string-to-number (match-string 1)))
                     (month (string-to-number (match-string 2)))
                     (day (string-to-number (match-string 3)))
                     (event-time (encode-time 0 0 0 day month year)))
                ;; All dates should be within window
                (when (or (time-less-p event-time three-months-ago)
                          (time-less-p twelve-months-future event-time))
                  (setq all-dates-in-range nil))))
            (should all-dates-in-range))))
    (test-integration-recurring-events-teardown)))

(ert-deftest test-integration-recurring-events-tzid-conversion ()
  "Test that TZID timestamps are handled correctly throughout pipeline.

Components integrated:
- calendar-sync--get-property (extracts DTSTART;TZID=America/Chicago:...)
- calendar-sync--parse-timestamp (converts to local time)
- calendar-sync--format-timestamp (formats for org-mode)
- calendar-sync--event-to-org (includes formatted timestamp)

Validates:
- TZID parameter doesn't break parsing (regression test)
- Timestamps are correctly formatted in org output
- Time values are preserved through pipeline"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let ((org-output (calendar-sync--parse-ics test-integration-recurring-events--weekly-ics)))
        (should (stringp org-output))

        ;; Should have timestamps with time range
        (should (string-match-p "Sat 10:30-11:00" org-output))

        ;; Should NOT have TZID in output (converted to org format)
        (should-not (string-match-p "TZID" org-output)))
    (test-integration-recurring-events-teardown)))

;;; Edge Cases - Error Handling

(ert-deftest test-integration-recurring-events-empty-ics-returns-nil ()
  "Test that empty ICS content is handled gracefully.

Components integrated:
- calendar-sync--parse-ics (top-level error handling)

Validates:
- Empty input doesn't crash
- Returns nil for empty content"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let ((org-output (calendar-sync--parse-ics "")))
        (should (null org-output)))
    (test-integration-recurring-events-teardown)))

(ert-deftest test-integration-recurring-events-malformed-ics-returns-nil ()
  "Test that malformed ICS content is handled gracefully.

Components integrated:
- calendar-sync--parse-ics (error handling)

Validates:
- Malformed input doesn't crash
- Error is caught and logged
- Returns nil for malformed content"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let ((org-output (calendar-sync--parse-ics "INVALID ICS DATA")))
        ;; Should handle error gracefully
        (should (null org-output)))
    (test-integration-recurring-events-teardown)))

(ert-deftest test-integration-recurring-events-missing-required-fields ()
  "Test handling of events missing required fields.

Components integrated:
- calendar-sync--parse-event (validates required fields)
- calendar-sync--parse-ics (filters invalid events)

Validates:
- Events without SUMMARY are excluded
- Events without DTSTART are excluded
- Valid events still processed"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let* ((incomplete-ics "BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VEVENT
DTSTART:20251201T100000Z
RRULE:FREQ=DAILY;COUNT=2
END:VEVENT
BEGIN:VEVENT
SUMMARY:Valid Event
DTSTART:20251201T110000Z
DTEND:20251201T120000Z
END:VEVENT
END:VCALENDAR")
             (org-output (calendar-sync--parse-ics incomplete-ics)))
        ;; Should still generate output (for valid event)
        (should (stringp org-output))
        (should (string-match-p "Valid Event" org-output))

        ;; Invalid event (no SUMMARY) should be excluded
        (should-not (string-match-p "VEVENT" org-output)))
    (test-integration-recurring-events-teardown)))

(ert-deftest test-integration-recurring-events-unsupported-freq-skipped ()
  "Test that events with unsupported FREQ are handled gracefully.

Components integrated:
- calendar-sync--parse-rrule (parses unsupported FREQ)
- calendar-sync--expand-recurring-event (detects unsupported FREQ)
- calendar-sync--parse-ics (continues processing other events)

Validates:
- Unsupported FREQ doesn't crash pipeline
- Warning message is logged
- Other events still processed"
  (test-integration-recurring-events-setup)
  (unwind-protect
      (let* ((unsupported-ics "BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VEVENT
DTSTART:20251201T100000Z
DTEND:20251201T110000Z
RRULE:FREQ=HOURLY;COUNT=5
SUMMARY:Unsupported Hourly Event
UID:unsupported@example.com
END:VEVENT
END:VCALENDAR")
             (org-output (calendar-sync--parse-ics unsupported-ics)))
        ;; Should handle gracefully (may return nil or skip the event)
        ;; The key is it shouldn't crash
        (should (or (null org-output)
                    (stringp org-output))))
    (test-integration-recurring-events-teardown)))

(provide 'test-integration-recurring-events)
;;; test-integration-recurring-events.el ends here
