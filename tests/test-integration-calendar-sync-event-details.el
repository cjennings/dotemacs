;;; test-integration-calendar-sync-event-details.el --- Integration tests for event details  -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the enhanced event details workflow.
;; Tests the complete flow from raw ICS with HTML description, attendees,
;; organizer through parse-ics to verify org output.
;;
;; Components integrated:
;; - calendar-sync--unescape-ics-text (ICS text unescaping)
;; - calendar-sync--strip-html (HTML tag removal)
;; - calendar-sync--clean-text (combined cleaning)
;; - calendar-sync--get-all-property-lines (multi-line property extraction)
;; - calendar-sync--parse-attendee-line (attendee parsing)
;; - calendar-sync--find-user-status (user status lookup)
;; - calendar-sync--parse-organizer (organizer extraction)
;; - calendar-sync--extract-meeting-url (meeting URL extraction)
;; - calendar-sync--parse-event (full event parsing)
;; - calendar-sync--event-to-org (org format output with property drawer)
;; - calendar-sync--parse-ics (full ICS pipeline)

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Test Helpers

(defun test-integration-details--make-full-ics (start-time)
  "Create ICS with HTML description, attendees, organizer, and meeting URL.
START-TIME is (year month day hour minute)."
  (let ((dtstart (test-calendar-sync-ics-datetime start-time))
        (dtend (test-calendar-sync-ics-datetime
                (list (nth 0 start-time) (nth 1 start-time) (nth 2 start-time)
                      (1+ (nth 3 start-time)) (nth 4 start-time)))))
    (concat "BEGIN:VCALENDAR\n"
            "VERSION:2.0\n"
            "PRODID:-//Google Inc//Google Calendar//EN\n"
            "BEGIN:VEVENT\n"
            "UID:full-test@example.com\n"
            "SUMMARY:Team Planning\n"
            "DTSTART:" dtstart "\n"
            "DTEND:" dtend "\n"
            "DESCRIPTION:Discuss Q1 goals<br>Review metrics\\, update roadmap\n"
            "LOCATION:Conference Room B\n"
            "ORGANIZER;CN=John Smith:mailto:john@example.com\n"
            "ATTENDEE;CN=John Smith;PARTSTAT=ACCEPTED:mailto:john@example.com\n"
            "ATTENDEE;CN=Craig Jennings;PARTSTAT=ACCEPTED:mailto:craigmartinjennings@gmail.com\n"
            "ATTENDEE;CN=Alice;PARTSTAT=DECLINED:mailto:alice@example.com\n"
            "X-GOOGLE-CONFERENCE:https://meet.google.com/abc-defg-hij\n"
            "END:VEVENT\n"
            "END:VCALENDAR")))

(defun test-integration-details--make-plain-ics (start-time)
  "Create simple ICS without attendees or special properties.
START-TIME is (year month day hour minute)."
  (let ((dtstart (test-calendar-sync-ics-datetime start-time))
        (dtend (test-calendar-sync-ics-datetime
                (list (nth 0 start-time) (nth 1 start-time) (nth 2 start-time)
                      (1+ (nth 3 start-time)) (nth 4 start-time)))))
    (concat "BEGIN:VCALENDAR\n"
            "VERSION:2.0\n"
            "PRODID:-//Test//Test//EN\n"
            "BEGIN:VEVENT\n"
            "UID:plain-test@example.com\n"
            "SUMMARY:Simple Reminder\n"
            "DTSTART:" dtstart "\n"
            "DTEND:" dtend "\n"
            "END:VEVENT\n"
            "END:VCALENDAR")))

(defun test-integration-details--make-recurring-with-attendees (start-time)
  "Create ICS with recurring event that has attendees.
START-TIME is (year month day hour minute)."
  (let ((dtstart (test-calendar-sync-ics-datetime start-time))
        (dtend (test-calendar-sync-ics-datetime
                (list (nth 0 start-time) (nth 1 start-time) (nth 2 start-time)
                      (1+ (nth 3 start-time)) (nth 4 start-time)))))
    (concat "BEGIN:VCALENDAR\n"
            "VERSION:2.0\n"
            "PRODID:-//Test//Test//EN\n"
            "BEGIN:VEVENT\n"
            "UID:recurring-attendees@example.com\n"
            "SUMMARY:Weekly Standup\n"
            "DTSTART:" dtstart "\n"
            "DTEND:" dtend "\n"
            "RRULE:FREQ=WEEKLY;COUNT=3\n"
            "ORGANIZER;CN=Manager:mailto:manager@example.com\n"
            "ATTENDEE;CN=Craig;PARTSTAT=ACCEPTED:mailto:craigmartinjennings@gmail.com\n"
            "LOCATION:Virtual\n"
            "END:VEVENT\n"
            "END:VCALENDAR")))

(defun test-integration-details--make-ics-escaped-location (start-time)
  "Create ICS with ICS-escaped location field.
START-TIME is (year month day hour minute)."
  (let ((dtstart (test-calendar-sync-ics-datetime start-time))
        (dtend (test-calendar-sync-ics-datetime
                (list (nth 0 start-time) (nth 1 start-time) (nth 2 start-time)
                      (1+ (nth 3 start-time)) (nth 4 start-time)))))
    (concat "BEGIN:VCALENDAR\n"
            "VERSION:2.0\n"
            "PRODID:-//Test//Test//EN\n"
            "BEGIN:VEVENT\n"
            "UID:escaped-loc@example.com\n"
            "SUMMARY:Offsite Meeting\n"
            "DTSTART:" dtstart "\n"
            "DTEND:" dtend "\n"
            "LOCATION:123 Main St\\, Suite 400\\, New Orleans\\, LA\n"
            "END:VEVENT\n"
            "END:VCALENDAR")))

;;; Integration Tests

(ert-deftest test-integration-details-full-pipeline ()
  "Test full pipeline: ICS with HTML + attendees + organizer → clean org output.
Verifies:
- HTML in description is cleaned
- ICS escapes in description are unescaped
- Property drawer contains LOCATION, ORGANIZER, STATUS, URL
- Description appears as body text after drawer"
  (let* ((start-time (test-calendar-sync-time-days-from-now 7 14 0))
         (ics (test-integration-details--make-full-ics start-time))
         (calendar-sync-user-emails '("craigmartinjennings@gmail.com"))
         (org-output (calendar-sync--parse-ics ics)))
    (should org-output)
    ;; Summary present
    (should (string-match-p "Team Planning" org-output))
    ;; Clean description (HTML <br> → newline, ICS \, → comma)
    (should (string-match-p "Discuss Q1 goals" org-output))
    (should (string-match-p "Review metrics, update roadmap" org-output))
    (should-not (string-match-p "<br>" org-output))
    ;; Property drawer
    (should (string-match-p ":PROPERTIES:" org-output))
    (should (string-match-p ":LOCATION: Conference Room B" org-output))
    (should (string-match-p ":ORGANIZER: John Smith" org-output))
    (should (string-match-p ":STATUS: accepted" org-output))
    (should (string-match-p ":URL: https://meet.google.com/abc-defg-hij" org-output))
    (should (string-match-p ":END:" org-output))))

(ert-deftest test-integration-details-plain-event-no-drawer ()
  "Test plain event without attendees/location produces no property drawer."
  (let* ((start-time (test-calendar-sync-time-days-from-now 7 14 0))
         (ics (test-integration-details--make-plain-ics start-time))
         (org-output (calendar-sync--parse-ics ics)))
    (should org-output)
    (should (string-match-p "Simple Reminder" org-output))
    (should-not (string-match-p ":PROPERTIES:" org-output))))

(ert-deftest test-integration-details-recurring-with-attendees ()
  "Test recurring event with attendees flows details to expanded instances."
  (let* ((start-time (test-calendar-sync-time-days-from-now 7 14 0))
         (ics (test-integration-details--make-recurring-with-attendees start-time))
         (calendar-sync-user-emails '("craigmartinjennings@gmail.com"))
         (org-output (calendar-sync--parse-ics ics)))
    (should org-output)
    ;; Should have multiple occurrences of Weekly Standup
    (let ((count 0)
          (pos 0))
      (while (string-match "Weekly Standup" org-output pos)
        (setq count (1+ count))
        (setq pos (match-end 0)))
      (should (>= count 2)))))

(ert-deftest test-integration-details-escaped-location ()
  "Test ICS escapes in location are cleaned in org output."
  (let* ((start-time (test-calendar-sync-time-days-from-now 7 14 0))
         (ics (test-integration-details--make-ics-escaped-location start-time))
         (org-output (calendar-sync--parse-ics ics)))
    (should org-output)
    (should (string-match-p "Offsite Meeting" org-output))
    ;; Location should have real commas, not escaped
    (should (string-match-p "123 Main St, Suite 400" org-output))
    (should-not (string-match-p "\\\\," org-output))))

(provide 'test-integration-calendar-sync-event-details)
;;; test-integration-calendar-sync-event-details.el ends here
