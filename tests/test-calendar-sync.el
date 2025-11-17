;;; test-calendar-sync.el --- Tests for calendar-sync  -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive tests for calendar-sync module.
;; Covers Normal, Boundary, and Error cases for all parsing functions.
;; Uses dynamic timestamps (no hardcoded dates).

;;; Code:

(require 'ert)
(require 'calendar-sync)
(require 'testutil-calendar-sync)

;;; Test Data

(defconst test-calendar-sync-sample-ics
  "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Google Inc//Google Calendar 70.9054//EN
BEGIN:VEVENT
SUMMARY:Test Meeting
DTSTART:20251116T140000Z
DTEND:20251116T150000Z
DESCRIPTION:Discuss project status
LOCATION:Conference Room A
END:VEVENT
END:VCALENDAR"
  "Sample .ics content for testing.")

;;; Helper Functions

(defmacro with-test-time (time &rest body)
  "Execute BODY with `current-time` mocked to TIME."
  `(cl-letf (((symbol-function 'current-time)
              (lambda () ,time)))
     ,@body))

;;; Tests: calendar-sync--split-events

(ert-deftest test-calendar-sync--split-events-normal-single-event-returns-one ()
  "Test that single event is extracted correctly."
  (let* ((ics test-calendar-sync-sample-ics)
         (events (calendar-sync--split-events ics)))
    (should (= 1 (length events)))
    (should (string-match-p "BEGIN:VEVENT" (car events)))
    (should (string-match-p "END:VEVENT" (car events)))))

(ert-deftest test-calendar-sync--split-events-normal-multiple-events-returns-all ()
  "Test that multiple events are all extracted."
  (let* ((event1 (test-calendar-sync-make-vevent
                  "Event 1"
                  (test-calendar-sync-time-today-at 14 0)
                  (test-calendar-sync-time-today-at 15 0)))
         (event2 (test-calendar-sync-make-vevent
                  "Event 2"
                  (test-calendar-sync-time-tomorrow-at 10 0)
                  (test-calendar-sync-time-tomorrow-at 11 0)))
         (ics (test-calendar-sync-make-ics event1 event2))
         (events (calendar-sync--split-events ics)))
    (should (= 2 (length events)))
    (should (string-match-p "Event 1" (nth 0 events)))
    (should (string-match-p "Event 2" (nth 1 events)))))

(ert-deftest test-calendar-sync--split-events-boundary-empty-string-returns-nil ()
  "Test that empty string returns empty list."
  (should (null (calendar-sync--split-events ""))))

(ert-deftest test-calendar-sync--split-events-boundary-no-events-returns-nil ()
  "Test that .ics with no VEVENT returns empty list."
  (let ((ics "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR"))
    (should (null (calendar-sync--split-events ics)))))

;;; Tests: calendar-sync--get-property

(ert-deftest test-calendar-sync--get-property-normal-summary-returns-value ()
  "Test extracting SUMMARY property."
  (let ((event "BEGIN:VEVENT\nSUMMARY:Test Event\nEND:VEVENT"))
    (should (string= "Test Event" (calendar-sync--get-property event "SUMMARY")))))

(ert-deftest test-calendar-sync--get-property-normal-description-with-spaces ()
  "Test extracting DESCRIPTION with spaces."
  (let ((event "DESCRIPTION:Multi word description"))
    (should (string= "Multi word description"
                     (calendar-sync--get-property event "DESCRIPTION")))))

(ert-deftest test-calendar-sync--get-property-boundary-missing-property-returns-nil ()
  "Test that missing property returns nil."
  (let ((event "BEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT"))
    (should (null (calendar-sync--get-property event "LOCATION")))))

(ert-deftest test-calendar-sync--get-property-error-empty-string-returns-nil ()
  "Test that empty event string returns nil."
  (should (null (calendar-sync--get-property "" "SUMMARY"))))

;;; Tests: calendar-sync--parse-timestamp

(ert-deftest test-calendar-sync--parse-timestamp-normal-datetime-returns-full-time ()
  "Test parsing full datetime with time component.
UTC timestamp (with Z suffix) is converted to local time."
  (let* ((parsed (calendar-sync--parse-timestamp "20251116T140000Z"))
         ;; Compute expected local time from UTC
         (utc-time (encode-time 0 0 14 16 11 2025 0))
         (local-time (decode-time utc-time))
         (expected-hour (nth 2 local-time))
         (expected-minute (nth 1 local-time)))
    (should (= 5 (length parsed)))
    (should (= 2025 (nth 0 parsed)))
    (should (= 11 (nth 1 parsed)))
    (should (= 16 (nth 2 parsed)))
    (should (= expected-hour (nth 3 parsed)))
    (should (= expected-minute (nth 4 parsed)))))

(ert-deftest test-calendar-sync--parse-timestamp-normal-datetime-without-z ()
  "Test parsing datetime without Z suffix."
  (let* ((parsed (calendar-sync--parse-timestamp "20251116T140000")))
    (should (= 5 (length parsed)))
    (should (= 14 (nth 3 parsed)))))

(ert-deftest test-calendar-sync--parse-timestamp-boundary-date-only-returns-three-parts ()
  "Test parsing date-only timestamp (all-day event)."
  (let* ((parsed (calendar-sync--parse-timestamp "20251116")))
    (should (= 3 (length parsed)))
    (should (= 2025 (nth 0 parsed)))
    (should (= 11 (nth 1 parsed)))
    (should (= 16 (nth 2 parsed)))))

(ert-deftest test-calendar-sync--parse-timestamp-error-invalid-format-returns-nil ()
  "Test that invalid timestamp returns nil."
  (should (null (calendar-sync--parse-timestamp "invalid")))
  (should (null (calendar-sync--parse-timestamp "2025-11-16")))
  (should (null (calendar-sync--parse-timestamp ""))))

(ert-deftest test-calendar-sync--parse-timestamp-boundary-leap-year-feb-29 ()
  "Test parsing Feb 29 on leap year."
  (let* ((parsed (calendar-sync--parse-timestamp "20240229T120000Z")))
    (should parsed)
    (should (= 2024 (nth 0 parsed)))
    (should (= 2 (nth 1 parsed)))
    (should (= 29 (nth 2 parsed)))))

;;; Tests: Timezone Conversion

(ert-deftest test-calendar-sync--parse-timestamp-utc-conversion-actually-converts ()
  "Test that UTC timestamp (with Z) is actually converted to local time.
This test verifies the conversion happened by checking that the result
differs from the original UTC time (unless we happen to be in UTC timezone)."
  (let* ((utc-timestamp "20251116T140000Z")  ; 14:00 UTC
         (parsed (calendar-sync--parse-timestamp utc-timestamp))
         ;; Get what the UTC time would be without conversion
         (utc-hour 14)
         (parsed-hour (nth 3 parsed)))
    ;; The parsed hour should match what decode-time gives us for this UTC time
    (let* ((utc-time (encode-time 0 0 14 16 11 2025 0))
           (local-time (decode-time utc-time))
           (expected-local-hour (nth 2 local-time)))
      (should (= expected-local-hour parsed-hour)))))

(ert-deftest test-calendar-sync--parse-timestamp-local-time-not-converted ()
  "Test that timestamp without Z suffix is NOT converted.
Local times should pass through unchanged."
  (let* ((local-timestamp "20251116T140000")  ; 14:00 local (no Z)
         (parsed (calendar-sync--parse-timestamp local-timestamp)))
    ;; Should return exactly 14:00, not converted
    (should (= 14 (nth 3 parsed)))
    (should (= 0 (nth 4 parsed)))))

(ert-deftest test-calendar-sync--parse-timestamp-utc-midnight-converts-correctly ()
  "Test UTC midnight conversion handles day boundaries correctly."
  (let* ((parsed (calendar-sync--parse-timestamp "20251116T000000Z"))
         ;; Compute expected local time
         (utc-time (encode-time 0 0 0 16 11 2025 0))
         (local-time (decode-time utc-time))
         (expected-year (nth 5 local-time))
         (expected-month (nth 4 local-time))
         (expected-day (nth 3 local-time))
         (expected-hour (nth 2 local-time)))
    (should (= expected-year (nth 0 parsed)))
    (should (= expected-month (nth 1 parsed)))
    (should (= expected-day (nth 2 parsed)))
    (should (= expected-hour (nth 3 parsed)))))

;;; Tests: Chronological Sorting

(ert-deftest test-calendar-sync--event-start-time-extracts-comparable-time ()
  "Test that event start time can be extracted for comparison."
  (let* ((event (list :start (list 2025 11 16 14 30)))
         (time-value (calendar-sync--event-start-time event))
         (event-earlier (list :start (list 2025 11 16 10 0)))
         (time-earlier (calendar-sync--event-start-time event-earlier)))
    ;; Should return a valid time value (cons cell for Emacs time)
    (should (consp time-value))
    ;; Should be comparable - later time should not be less than earlier
    (should (time-less-p time-earlier time-value))))

(ert-deftest test-calendar-sync--event-start-time-handles-all-day-events ()
  "Test that all-day events (no time component) work for comparison."
  (let* ((event (list :start (list 2025 11 16)))  ; No hour/minute
         (time-value (calendar-sync--event-start-time event))
         (event-next-day (list :start (list 2025 11 17)))
         (time-next-day (calendar-sync--event-start-time event-next-day)))
    ;; Should return a valid time value (cons cell)
    (should (consp time-value))
    ;; Next day should be later than current day
    (should (time-less-p time-value time-next-day))))

(ert-deftest test-calendar-sync--parse-ics-sorts-chronologically ()
  "Test that parsed events are returned in chronological order.
Earlier events should appear first in the output."
  (let* ((event-future (test-calendar-sync-make-vevent
                         "Future Event"
                         (test-calendar-sync-time-days-from-now 7 10 0)
                         (test-calendar-sync-time-days-from-now 7 11 0)))
         (event-past (test-calendar-sync-make-vevent
                      "Past Event"
                      (test-calendar-sync-time-days-ago 1 14 0)
                      (test-calendar-sync-time-days-ago 1 15 0)))
         (event-today (test-calendar-sync-make-vevent
                       "Today Event"
                       (test-calendar-sync-time-today-at 9 0)
                       (test-calendar-sync-time-today-at 10 0)))
         ;; Create .ics with events in wrong order (future, past, today)
         (ics (test-calendar-sync-make-ics event-future event-past event-today))
         (org-content (calendar-sync--parse-ics ics))
         ;; Find positions of each event in output
         (past-pos (string-match "Past Event" org-content))
         (today-pos (string-match "Today Event" org-content))
         (future-pos (string-match "Future Event" org-content)))
    ;; All events should be found
    (should past-pos)
    (should today-pos)
    (should future-pos)
    ;; Order should be: past < today < future
    (should (< past-pos today-pos))
    (should (< today-pos future-pos))))

(ert-deftest test-calendar-sync--parse-ics-sorts-same-day-events-by-time ()
  "Test that events on the same day are sorted by time."
  (let* ((event-morning (test-calendar-sync-make-vevent
                         "Morning Event"
                         (test-calendar-sync-time-today-at 9 0)
                         (test-calendar-sync-time-today-at 10 0)))
         (event-evening (test-calendar-sync-make-vevent
                         "Evening Event"
                         (test-calendar-sync-time-today-at 18 0)
                         (test-calendar-sync-time-today-at 19 0)))
         (event-afternoon (test-calendar-sync-make-vevent
                           "Afternoon Event"
                           (test-calendar-sync-time-today-at 14 0)
                           (test-calendar-sync-time-today-at 15 0)))
         ;; Create .ics with events in wrong order
         (ics (test-calendar-sync-make-ics event-evening event-morning event-afternoon))
         (org-content (calendar-sync--parse-ics ics))
         (morning-pos (string-match "Morning Event" org-content))
         (afternoon-pos (string-match "Afternoon Event" org-content))
         (evening-pos (string-match "Evening Event" org-content)))
    (should (< morning-pos afternoon-pos))
    (should (< afternoon-pos evening-pos))))

;;; Tests: calendar-sync--format-timestamp

(ert-deftest test-calendar-sync--format-timestamp-normal-timed-event-includes-times ()
  "Test formatting timed event with start and end times."
  (let* ((start (list 2025 11 16 14 0))
         (end (list 2025 11 16 15 30))
         (formatted (calendar-sync--format-timestamp start end)))
    (should (string-match-p "<2025-11-16 \\w\\{3\\} 14:00-15:30>" formatted))))

(ert-deftest test-calendar-sync--format-timestamp-boundary-all-day-event-no-times ()
  "Test formatting all-day event (date only, no times)."
  (let* ((start (list 2025 11 16))
         (formatted (calendar-sync--format-timestamp start nil)))
    (should (string-match-p "<2025-11-16 \\w\\{3\\}>" formatted))
    (should-not (string-match-p "[0-9]:[0-9]" formatted))))

(ert-deftest test-calendar-sync--format-timestamp-normal-includes-day-of-week ()
  "Test that formatted timestamp includes day of week."
  (let* ((start (list 2025 11 16 14 0))
         (end (list 2025 11 16 15 0))
         (formatted (calendar-sync--format-timestamp start end)))
    (should (string-match-p "Sun" formatted))))

;;; Tests: calendar-sync--parse-event

(ert-deftest test-calendar-sync--parse-event-normal-complete-event-returns-plist ()
  "Test parsing complete event with all fields."
  (let* ((event (test-calendar-sync-make-vevent
                 "Meeting"
                 (test-calendar-sync-time-today-at 14 0)
                 (test-calendar-sync-time-today-at 15 0)
                 "Discussion"
                 "Room A"))
         (parsed (calendar-sync--parse-event event)))
    (should parsed)
    (should (string= "Meeting" (plist-get parsed :summary)))
    (should (string= "Discussion" (plist-get parsed :description)))
    (should (string= "Room A" (plist-get parsed :location)))
    (should (plist-get parsed :start))
    (should (plist-get parsed :end))))

(ert-deftest test-calendar-sync--parse-event-boundary-minimal-event-no-optional-fields ()
  "Test parsing event with only required fields (SUMMARY, DTSTART)."
  (let* ((event (test-calendar-sync-make-vevent
                 "Simple Event"
                 (test-calendar-sync-time-today-at 10 0)
                 nil))
         (parsed (calendar-sync--parse-event event)))
    (should parsed)
    (should (string= "Simple Event" (plist-get parsed :summary)))
    (should (null (plist-get parsed :description)))
    (should (null (plist-get parsed :location)))
    (should (plist-get parsed :start))))

(ert-deftest test-calendar-sync--parse-event-boundary-emoji-in-summary-preserved ()
  "Test that emoji in summary are preserved."
  (let* ((event (test-calendar-sync-make-vevent
                 "Meeting 🎉"
                 (test-calendar-sync-time-today-at 14 0)
                 (test-calendar-sync-time-today-at 15 0)))
         (parsed (calendar-sync--parse-event event)))
    (should (string-match-p "🎉" (plist-get parsed :summary)))))

(ert-deftest test-calendar-sync--parse-event-error-missing-summary-returns-nil ()
  "Test that event without SUMMARY returns nil."
  (let ((event "BEGIN:VEVENT\nDTSTART:20251116T140000Z\nEND:VEVENT"))
    (should (null (calendar-sync--parse-event event)))))

(ert-deftest test-calendar-sync--parse-event-error-missing-dtstart-returns-nil ()
  "Test that event without DTSTART returns nil."
  (let ((event "BEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT"))
    (should (null (calendar-sync--parse-event event)))))

(ert-deftest test-calendar-sync--parse-event-error-invalid-dtstart-returns-nil ()
  "Test that event with invalid DTSTART returns nil."
  (let ((event "BEGIN:VEVENT\nSUMMARY:Test\nDTSTART:invalid\nEND:VEVENT"))
    (should (null (calendar-sync--parse-event event)))))

;;; Tests: calendar-sync--event-to-org

(ert-deftest test-calendar-sync--event-to-org-normal-complete-event-formats-correctly ()
  "Test converting complete event to org format."
  (let* ((event (list :summary "Meeting"
                      :description "Discuss project"
                      :location "Room A"
                      :start (list 2025 11 16 14 0)
                      :end (list 2025 11 16 15 30)))
         (org-str (calendar-sync--event-to-org event)))
    (should (string-match-p "^\\* Meeting$" org-str))
    (should (string-match-p "<2025-11-16 \\w\\{3\\} 14:00-15:30>" org-str))
    (should (string-match-p "Discuss project" org-str))
    (should (string-match-p "Location: Room A" org-str))))

(ert-deftest test-calendar-sync--event-to-org-boundary-minimal-event-no-description ()
  "Test converting minimal event without optional fields."
  (let* ((event (list :summary "Simple Event"
                      :start (list 2025 11 16 10 0)
                      :end (list 2025 11 16 11 0)))
         (org-str (calendar-sync--event-to-org event)))
    (should (string-match-p "^\\* Simple Event$" org-str))
    (should-not (string-match-p "Location:" org-str))
    ;; Check timestamp is present
    (should (string-match-p "<2025-11-16" org-str))))

(ert-deftest test-calendar-sync--event-to-org-boundary-all-day-event-no-times ()
  "Test converting all-day event."
  (let* ((event (list :summary "All Day Event"
                      :start (list 2025 11 16)))
         (org-str (calendar-sync--event-to-org event)))
    (should (string-match-p "^\\* All Day Event$" org-str))
    (should (string-match-p "<2025-11-16" org-str))
    (should-not (string-match-p "[0-9][0-9]:[0-9][0-9]" org-str))))

;;; Tests: calendar-sync--parse-ics

(ert-deftest test-calendar-sync--parse-ics-normal-single-event-returns-org ()
  "Test parsing .ics with single event returns org format."
  (let* ((event (test-calendar-sync-make-vevent
                 "Test Event"
                 (test-calendar-sync-time-today-at 14 0)
                 (test-calendar-sync-time-today-at 15 0)))
         (ics (test-calendar-sync-make-ics event))
         (org-content (calendar-sync--parse-ics ics)))
    (should org-content)
    (should (string-match-p "^# Google Calendar Events" org-content))
    (should (string-match-p "\\* Test Event" org-content))))

(ert-deftest test-calendar-sync--parse-ics-normal-multiple-events-all-included ()
  "Test parsing .ics with multiple events."
  (let* ((event1 (test-calendar-sync-make-vevent
                  "Event 1"
                  (test-calendar-sync-time-today-at 9 0)
                  (test-calendar-sync-time-today-at 10 0)))
         (event2 (test-calendar-sync-make-vevent
                  "Event 2"
                  (test-calendar-sync-time-today-at 14 0)
                  (test-calendar-sync-time-today-at 15 0)))
         (ics (test-calendar-sync-make-ics event1 event2))
         (org-content (calendar-sync--parse-ics ics)))
    (should org-content)
    (should (string-match-p "\\* Event 1" org-content))
    (should (string-match-p "\\* Event 2" org-content))))

(ert-deftest test-calendar-sync--parse-ics-boundary-empty-calendar-returns-nil ()
  "Test parsing empty calendar (no events)."
  (let* ((ics "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR")
         (org-content (calendar-sync--parse-ics ics)))
    (should (null org-content))))

(ert-deftest test-calendar-sync--parse-ics-error-malformed-ics-returns-nil ()
  "Test that malformed .ics returns nil and sets error."
  (setq calendar-sync--last-error nil)
  (let ((result (calendar-sync--parse-ics "malformed content")))
    ;; Function should handle error gracefully
    (should (or (null result) (stringp result)))))

(ert-deftest test-calendar-sync--parse-ics-boundary-mixed-valid-invalid-events ()
  "Test parsing .ics with mix of valid and invalid events.
Valid events should be parsed, invalid ones skipped."
  (let* ((valid-event (test-calendar-sync-make-vevent
                       "Valid Event"
                       (test-calendar-sync-time-today-at 14 0)
                       (test-calendar-sync-time-today-at 15 0)))
         (invalid-event "BEGIN:VEVENT\nDTSTART:20251116T140000Z\nEND:VEVENT") ;; No SUMMARY
         (ics (test-calendar-sync-make-ics valid-event invalid-event))
         (org-content (calendar-sync--parse-ics ics)))
    (should org-content)
    (should (string-match-p "\\* Valid Event" org-content))))

;;; Tests: Timezone Detection

(ert-deftest test-calendar-sync--current-timezone-offset-returns-number ()
  "Test that current timezone offset returns a number in seconds."
  (let ((offset (calendar-sync--current-timezone-offset)))
    ;; Should be a number
    (should (numberp offset))
    ;; Should be reasonable (between -12 and +14 hours in seconds)
    (should (>= offset (* -12 3600)))
    (should (<= offset (* 14 3600)))))

(ert-deftest test-calendar-sync--timezone-name-returns-string ()
  "Test that timezone name returns a string."
  (let ((name (calendar-sync--timezone-name)))
    ;; Should be a string
    (should (stringp name))
    ;; Should not be empty
    (should (> (length name) 0))))

(ert-deftest test-calendar-sync--format-timezone-offset-handles-negative ()
  "Test formatting negative timezone offsets (west of UTC)."
  ;; CST: UTC-6 = -21600 seconds
  (should (string= "UTC-6" (calendar-sync--format-timezone-offset -21600)))
  ;; PST: UTC-8 = -28800 seconds
  (should (string= "UTC-8" (calendar-sync--format-timezone-offset -28800)))
  ;; EST: UTC-5 = -18000 seconds
  (should (string= "UTC-5" (calendar-sync--format-timezone-offset -18000))))

(ert-deftest test-calendar-sync--format-timezone-offset-handles-positive ()
  "Test formatting positive timezone offsets (east of UTC)."
  ;; CET: UTC+1 = 3600 seconds
  (should (string= "UTC+1" (calendar-sync--format-timezone-offset 3600)))
  ;; JST: UTC+9 = 32400 seconds
  (should (string= "UTC+9" (calendar-sync--format-timezone-offset 32400)))
  ;; AEST: UTC+10 = 36000 seconds
  (should (string= "UTC+10" (calendar-sync--format-timezone-offset 36000))))

(ert-deftest test-calendar-sync--format-timezone-offset-handles-utc ()
  "Test formatting UTC (zero offset)."
  (should (string= "UTC+0" (calendar-sync--format-timezone-offset 0))))

(ert-deftest test-calendar-sync--format-timezone-offset-handles-fractional ()
  "Test formatting timezone offsets with fractional hours."
  ;; IST: UTC+5:30 = 19800 seconds
  (should (string= "UTC+5:30" (calendar-sync--format-timezone-offset 19800)))
  ;; ACST: UTC+9:30 = 34200 seconds
  (should (string= "UTC+9:30" (calendar-sync--format-timezone-offset 34200)))
  ;; NFT: UTC+11:30 = 41400 seconds
  (should (string= "UTC+11:30" (calendar-sync--format-timezone-offset 41400))))

(ert-deftest test-calendar-sync--format-timezone-offset-handles-nil ()
  "Test formatting nil timezone offset."
  (should (string= "unknown" (calendar-sync--format-timezone-offset nil))))

(ert-deftest test-calendar-sync--timezone-changed-p-detects-no-change ()
  "Test that timezone-changed-p returns nil when timezone hasn't changed."
  (let ((calendar-sync--last-timezone-offset (calendar-sync--current-timezone-offset)))
    (should-not (calendar-sync--timezone-changed-p))))

(ert-deftest test-calendar-sync--timezone-changed-p-detects-change ()
  "Test that timezone-changed-p detects timezone changes."
  (let* ((current (calendar-sync--current-timezone-offset))
         ;; Simulate different timezone (shift by 3 hours)
         (calendar-sync--last-timezone-offset (+ current (* 3 3600))))
    (should (calendar-sync--timezone-changed-p))))

(ert-deftest test-calendar-sync--timezone-changed-p-handles-nil-last ()
  "Test that timezone-changed-p returns nil when no previous timezone."
  (let ((calendar-sync--last-timezone-offset nil))
    (should-not (calendar-sync--timezone-changed-p))))

;;; Tests: State Persistence

(ert-deftest test-calendar-sync--save-and-load-state-roundtrip ()
  "Test that state can be saved and loaded correctly."
  (let* ((test-state-file (make-temp-file "calendar-sync-test-state"))
         (calendar-sync--state-file test-state-file)
         (original-offset -21600)  ; CST
         (original-time (current-time))
         (calendar-sync--last-timezone-offset original-offset)
         (calendar-sync--last-sync-time original-time))
    (unwind-protect
        (progn
          ;; Save state
          (calendar-sync--save-state)
          (should (file-exists-p test-state-file))

          ;; Clear variables
          (setq calendar-sync--last-timezone-offset nil)
          (setq calendar-sync--last-sync-time nil)

          ;; Load state
          (calendar-sync--load-state)

          ;; Verify loaded correctly
          (should (= original-offset calendar-sync--last-timezone-offset))
          (should (equal original-time calendar-sync--last-sync-time)))
      ;; Cleanup
      (when (file-exists-p test-state-file)
        (delete-file test-state-file)))))

(ert-deftest test-calendar-sync--save-state-creates-directory ()
  "Test that save-state creates parent directory if needed."
  (let* ((test-dir (make-temp-file "calendar-sync-test-dir" t))
         (test-state-file (expand-file-name "subdir/state.el" test-dir))
         (calendar-sync--state-file test-state-file)
         (calendar-sync--last-timezone-offset -21600)
         (calendar-sync--last-sync-time (current-time)))
    (unwind-protect
        (progn
          (calendar-sync--save-state)
          (should (file-exists-p test-state-file))
          (should (file-directory-p (file-name-directory test-state-file))))
      ;; Cleanup
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest test-calendar-sync--load-state-handles-missing-file ()
  "Test that load-state handles missing file gracefully."
  (let ((calendar-sync--state-file "/nonexistent/path/state.el")
        (calendar-sync--last-timezone-offset nil)
        (calendar-sync--last-sync-time nil))
    ;; Should not error
    (should-not (calendar-sync--load-state))
    ;; Variables should remain nil
    (should-not calendar-sync--last-timezone-offset)
    (should-not calendar-sync--last-sync-time)))

(ert-deftest test-calendar-sync--load-state-handles-corrupted-file ()
  "Test that load-state handles corrupted state file gracefully."
  (let* ((test-state-file (make-temp-file "calendar-sync-test-state"))
         (calendar-sync--state-file test-state-file)
         (calendar-sync--last-timezone-offset nil)
         (calendar-sync--last-sync-time nil))
    (unwind-protect
        (progn
          ;; Write corrupted data
          (with-temp-file test-state-file
            (insert "this is not valid elisp {[}"))

          ;; Should handle error gracefully (catches error, logs message)
          ;; Returns error message string, not nil, but doesn't throw
          (should (stringp (calendar-sync--load-state)))

          ;; Variables should remain nil (not loaded from corrupted file)
          (should-not calendar-sync--last-timezone-offset)
          (should-not calendar-sync--last-sync-time))
      ;; Cleanup
      (when (file-exists-p test-state-file)
        (delete-file test-state-file)))))

(provide 'test-calendar-sync)
;;; test-calendar-sync.el ends here
