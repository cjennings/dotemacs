;;; test-calendar-sync--extract-meeting-url.el --- Tests for meeting URL extraction  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--extract-meeting-url function.
;; Extracts URL from X-GOOGLE-CONFERENCE (preferred) or URL property.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--extract-meeting-url-normal-google-conference ()
  "Test extracting X-GOOGLE-CONFERENCE URL."
  (let ((event "BEGIN:VEVENT\nX-GOOGLE-CONFERENCE:https://meet.google.com/abc-defg-hij\nSUMMARY:Test\nEND:VEVENT"))
    (should (string= "https://meet.google.com/abc-defg-hij"
                      (calendar-sync--extract-meeting-url event)))))

(ert-deftest test-calendar-sync--extract-meeting-url-normal-url-property ()
  "Test extracting URL property."
  (let ((event "BEGIN:VEVENT\nURL:https://zoom.us/j/123456\nSUMMARY:Test\nEND:VEVENT"))
    (should (string= "https://zoom.us/j/123456"
                      (calendar-sync--extract-meeting-url event)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--extract-meeting-url-boundary-both-present ()
  "Test X-GOOGLE-CONFERENCE is preferred when both present."
  (let ((event "BEGIN:VEVENT\nURL:https://zoom.us/j/123456\nX-GOOGLE-CONFERENCE:https://meet.google.com/abc\nSUMMARY:Test\nEND:VEVENT"))
    (should (string= "https://meet.google.com/abc"
                      (calendar-sync--extract-meeting-url event)))))

(ert-deftest test-calendar-sync--extract-meeting-url-boundary-neither-present ()
  "Test returns nil when neither URL property exists."
  (let ((event "BEGIN:VEVENT\nSUMMARY:Test\nDTSTART:20260210T140000Z\nEND:VEVENT"))
    (should (null (calendar-sync--extract-meeting-url event)))))

(ert-deftest test-calendar-sync--extract-meeting-url-boundary-url-with-params ()
  "Test URL property with parameters."
  (let ((event "BEGIN:VEVENT\nURL;VALUE=URI:https://teams.microsoft.com/l/meetup-join/abc\nSUMMARY:Test\nEND:VEVENT"))
    (should (string-match-p "teams.microsoft.com"
                             (calendar-sync--extract-meeting-url event)))))

;;; Error Cases

(ert-deftest test-calendar-sync--extract-meeting-url-error-nil-event ()
  "Test nil event returns nil."
  (should (null (calendar-sync--extract-meeting-url nil))))

(provide 'test-calendar-sync--extract-meeting-url)
;;; test-calendar-sync--extract-meeting-url.el ends here
