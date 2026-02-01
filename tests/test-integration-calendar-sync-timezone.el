;;; test-integration-calendar-sync-timezone.el --- Integration tests for timezone handling  -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for calendar-sync timezone conversion workflow.
;; Tests the complete flow from ICS with TZID to correct local time in org output.
;;
;; Components integrated:
;; - calendar-sync--extract-tzid (TZID extraction from property lines)
;; - calendar-sync--convert-tz-to-local (timezone conversion)
;; - calendar-sync--get-property (property extraction, now with TZID awareness)
;; - calendar-sync--parse-timestamp (timestamp parsing with timezone)
;; - calendar-sync--parse-event (full event parsing)
;; - calendar-sync--event-to-org (org format output)
;; - calendar-sync--parse-ics (full ICS parsing)
;;
;; Validates:
;; - TZID is extracted from property lines and passed through workflow
;; - Timezone conversion produces correct local times
;; - Final org timestamps reflect local time, not source timezone
;; - Multiple timezones in same ICS are handled independently

;;; Code:

(require 'ert)
(require 'calendar-sync)
(require 'testutil-calendar-sync)

;;; Test Data

(defun test-integration-tz--make-ics-with-tzid-event (summary start-time tzid)
  "Create minimal ICS with single TZID-qualified event.
START-TIME is (year month day hour minute).
Returns complete ICS string."
  (let* ((dtstart (format "%04d%02d%02dT%02d%02d00"
                          (nth 0 start-time) (nth 1 start-time) (nth 2 start-time)
                          (nth 3 start-time) (nth 4 start-time)))
         (dtend (format "%04d%02d%02dT%02d%02d00"
                        (nth 0 start-time) (nth 1 start-time) (nth 2 start-time)
                        (1+ (nth 3 start-time)) (nth 4 start-time))))
    (concat "BEGIN:VCALENDAR\n"
            "VERSION:2.0\n"
            "PRODID:-//Test//Test//EN\n"
            "BEGIN:VEVENT\n"
            "SUMMARY:" summary "\n"
            "DTSTART;TZID=" tzid ":" dtstart "\n"
            "DTEND;TZID=" tzid ":" dtend "\n"
            "END:VEVENT\n"
            "END:VCALENDAR")))

(defun test-integration-tz--make-mixed-ics ()
  "Create ICS with events in different timezone formats.
Returns ICS with: UTC event, TZID event, and local event."
  (let* ((time1 (test-calendar-sync-time-days-from-now 7 14 0))
         (time2 (test-calendar-sync-time-days-from-now 7 19 0))
         (time3 (test-calendar-sync-time-days-from-now 7 10 0)))
    (concat "BEGIN:VCALENDAR\n"
            "VERSION:2.0\n"
            "PRODID:-//Test//Test//EN\n"
            ;; Event 1: UTC (Z suffix)
            "BEGIN:VEVENT\n"
            "SUMMARY:UTC Event\n"
            "DTSTART:" (test-calendar-sync-ics-datetime time1) "\n"
            "DTEND:" (test-calendar-sync-ics-datetime
                      (list (nth 0 time1) (nth 1 time1) (nth 2 time1)
                            (1+ (nth 3 time1)) (nth 4 time1))) "\n"
            "END:VEVENT\n"
            ;; Event 2: TZID-qualified (Europe/Lisbon)
            "BEGIN:VEVENT\n"
            "SUMMARY:Lisbon Event\n"
            "DTSTART;TZID=Europe/Lisbon:" (test-calendar-sync-ics-datetime-local time2) "\n"
            "DTEND;TZID=Europe/Lisbon:" (test-calendar-sync-ics-datetime-local
                                          (list (nth 0 time2) (nth 1 time2) (nth 2 time2)
                                                (1+ (nth 3 time2)) (nth 4 time2))) "\n"
            "END:VEVENT\n"
            ;; Event 3: Local (no Z, no TZID)
            "BEGIN:VEVENT\n"
            "SUMMARY:Local Event\n"
            "DTSTART:" (test-calendar-sync-ics-datetime-local time3) "\n"
            "DTEND:" (test-calendar-sync-ics-datetime-local
                      (list (nth 0 time3) (nth 1 time3) (nth 2 time3)
                            (1+ (nth 3 time3)) (nth 4 time3))) "\n"
            "END:VEVENT\n"
            "END:VCALENDAR")))

;;; Integration Tests - Full Workflow

(ert-deftest test-integration-timezone-lisbon-event-converts-to-local ()
  "Test that Europe/Lisbon event is converted to local time.

When an event has DTSTART;TZID=Europe/Lisbon:20260202T190000, the parsed
event should have local time (e.g., 13:00 CST), not the original 19:00.

Components integrated:
- calendar-sync--split-events (event extraction)
- calendar-sync--get-property (property with TZID)
- calendar-sync--extract-tzid (TZID parameter extraction)
- calendar-sync--parse-timestamp (parsing with timezone conversion)
- calendar-sync--convert-tz-to-local (actual timezone conversion)
- calendar-sync--parse-event (full event plist)

Validates:
- TZID is detected and passed to conversion function
- Conversion uses correct offset (Lisbon winter = UTC+0)
- Result contains local hour, not source timezone hour"
  (let* ((source-hour 19)
         (source-time (list 2026 2 2 source-hour 0))
         (ics (test-integration-tz--make-ics-with-tzid-event
               "Lisbon Meeting" source-time "Europe/Lisbon"))
         ;; Calculate expected local time
         (expected-local (test-calendar-sync-convert-tz-via-date
                          2026 2 2 source-hour 0 "Europe/Lisbon"))
         (expected-local-hour (nth 3 expected-local)))
    ;; Sanity check: local hour should differ from source
    ;; (unless we happen to be in Lisbon, which is unlikely)
    (should expected-local)
    ;; Parse the ICS and check the event
    (let* ((events (calendar-sync--split-events ics))
           (event-str (car events))
           (parsed (calendar-sync--parse-event event-str)))
      (should parsed)
      (should (string= "Lisbon Meeting" (plist-get parsed :summary)))
      (let* ((start (plist-get parsed :start))
             (result-hour (nth 3 start)))
        ;; The hour should be the LOCAL hour, not the source hour
        (should (= expected-local-hour result-hour))))))

(ert-deftest test-integration-timezone-yerevan-event-converts-to-local ()
  "Test that Asia/Yerevan event is converted to local time.

Asia/Yerevan is UTC+4 year-round, so 20:00 Yerevan = 16:00 UTC.
For US Central (UTC-6), that's 10:00 local.

Components integrated:
- calendar-sync--split-events
- calendar-sync--get-property
- calendar-sync--extract-tzid
- calendar-sync--parse-timestamp
- calendar-sync--convert-tz-to-local
- calendar-sync--parse-event

Validates:
- Large timezone offset (10 hours from Yerevan to US Central) handled
- Date may change during conversion (handled correctly)"
  (let* ((source-hour 20)
         (source-time (list 2026 2 2 source-hour 0))
         (ics (test-integration-tz--make-ics-with-tzid-event
               "Yerevan Call" source-time "Asia/Yerevan"))
         (expected-local (test-calendar-sync-convert-tz-via-date
                          2026 2 2 source-hour 0 "Asia/Yerevan"))
         (expected-local-hour (nth 3 expected-local)))
    (should expected-local)
    (let* ((events (calendar-sync--split-events ics))
           (parsed (calendar-sync--parse-event (car events))))
      (should parsed)
      (let* ((start (plist-get parsed :start))
             (result-hour (nth 3 start)))
        (should (= expected-local-hour result-hour))))))

(ert-deftest test-integration-timezone-mixed-formats-all-convert ()
  "Test ICS with UTC, TZID, and local timestamps all parse correctly.

Components integrated:
- calendar-sync--parse-ics (full ICS parsing)
- All timestamp parsing and conversion functions

Validates:
- UTC events (Z suffix) convert to local
- TZID events convert from source timezone to local
- Local events (no Z, no TZID) remain unchanged
- All three formats can coexist in same ICS"
  (let* ((ics (test-integration-tz--make-mixed-ics))
         (org-output (calendar-sync--parse-ics ics)))
    (should org-output)
    ;; Should contain all three events
    (should (string-match-p "UTC Event" org-output))
    (should (string-match-p "Lisbon Event" org-output))
    (should (string-match-p "Local Event" org-output))
    ;; Each should have valid org timestamps
    (should (string-match-p "<[0-9]+-[0-9]+-[0-9]+ [A-Za-z]+" org-output))))

(ert-deftest test-integration-timezone-org-timestamp-format-correct ()
  "Test that final org output has correctly formatted local timestamp.

Components integrated:
- Full parsing pipeline through calendar-sync--event-to-org
- calendar-sync--format-timestamp

Validates:
- Org timestamp format is correct (<YYYY-MM-DD Day HH:MM-HH:MM>)
- Hour in timestamp is the converted local hour"
  (let* ((source-time (list 2026 2 2 19 0))
         (ics (test-integration-tz--make-ics-with-tzid-event
               "Test Event" source-time "Europe/Lisbon"))
         (expected-local (test-calendar-sync-convert-tz-via-date
                          2026 2 2 19 0 "Europe/Lisbon"))
         (expected-hour (nth 3 expected-local))
         (org-output (calendar-sync--parse-ics ics)))
    (should org-output)
    (should (string-match-p "Test Event" org-output))
    ;; Check that the timestamp contains the expected local hour
    (let ((hour-pattern (format "%02d:" expected-hour)))
      (should (string-match-p hour-pattern org-output)))))

(ert-deftest test-integration-timezone-date-change-handled ()
  "Test that timezone conversion crossing date boundary is handled.

When converting late evening in Europe to US time, the date may change.
e.g., 23:00 London on Feb 2 = 17:00 CST on Feb 2 (same day)
but 02:00 Tokyo on Feb 3 = previous day in US

Components integrated:
- Full parsing pipeline
- Date arithmetic in timezone conversion

Validates:
- Date changes during timezone conversion are reflected in output
- Year/month boundaries are handled correctly"
  (let* ((source-time (list 2026 2 3 2 0))  ; 2 AM Tokyo on Feb 3
         (ics (test-integration-tz--make-ics-with-tzid-event
               "Early Tokyo Meeting" source-time "Asia/Tokyo"))
         (expected-local (test-calendar-sync-convert-tz-via-date
                          2026 2 3 2 0 "Asia/Tokyo"))
         (expected-day (nth 2 expected-local)))
    (should expected-local)
    (let* ((events (calendar-sync--split-events ics))
           (parsed (calendar-sync--parse-event (car events))))
      (should parsed)
      (let* ((start (plist-get parsed :start))
             (result-day (nth 2 start)))
        ;; Day should match expected (may be Feb 2 instead of Feb 3)
        (should (= expected-day result-day))))))

(ert-deftest test-integration-timezone-utc-still-works ()
  "Test that UTC timestamps (Z suffix) still convert correctly.

Regression test to ensure TZID handling doesn't break existing UTC conversion.

Components integrated:
- calendar-sync--parse-timestamp (UTC path)
- calendar-sync--convert-utc-to-local

Validates:
- Z suffix timestamps still trigger UTC-to-local conversion
- Behavior unchanged from before TZID feature"
  (let* ((utc-time (list 2026 2 2 19 0))
         (event (test-calendar-sync-make-vevent
                 "UTC Meeting"
                 utc-time
                 (list 2026 2 2 20 0)))
         (ics (test-calendar-sync-make-ics event))
         ;; UTC conversion: 19:00 UTC to local
         (utc-as-time (encode-time 0 0 19 2 2 2026 0))
         (local-decoded (decode-time utc-as-time))
         (expected-hour (nth 2 local-decoded)))
    (let* ((events (calendar-sync--split-events ics))
           (parsed (calendar-sync--parse-event (car events))))
      (should parsed)
      (let* ((start (plist-get parsed :start))
             (result-hour (nth 3 start)))
        (should (= expected-hour result-hour))))))

(provide 'test-integration-calendar-sync-timezone)
;;; test-integration-calendar-sync-timezone.el ends here
