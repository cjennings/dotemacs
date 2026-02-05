;;; test-calendar-sync--parse-organizer.el --- Tests for organizer parsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--parse-organizer function.
;; Parses ORGANIZER property line into plist (:cn NAME :email EMAIL).
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-organizer-normal-cn-and-email ()
  "Test parsing organizer with both CN and email."
  (let ((event "BEGIN:VEVENT\nORGANIZER;CN=John Smith:mailto:john@example.com\nSUMMARY:Test\nEND:VEVENT"))
    (let ((result (calendar-sync--parse-organizer event)))
      (should (string= "John Smith" (plist-get result :cn)))
      (should (string= "john@example.com" (plist-get result :email))))))

(ert-deftest test-calendar-sync--parse-organizer-normal-no-cn ()
  "Test parsing organizer without CN."
  (let ((event "BEGIN:VEVENT\nORGANIZER:mailto:organizer@example.com\nSUMMARY:Test\nEND:VEVENT"))
    (let ((result (calendar-sync--parse-organizer event)))
      (should (null (plist-get result :cn)))
      (should (string= "organizer@example.com" (plist-get result :email))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-organizer-boundary-no-organizer ()
  "Test event without ORGANIZER returns nil."
  (let ((event "BEGIN:VEVENT\nSUMMARY:Test\nDTSTART:20260210T140000Z\nEND:VEVENT"))
    (should (null (calendar-sync--parse-organizer event)))))

(ert-deftest test-calendar-sync--parse-organizer-boundary-complex-cn ()
  "Test organizer with complex CN (quotes, special chars)."
  (let ((event "BEGIN:VEVENT\nORGANIZER;CN=\"Dr. Jane O'Brien\":mailto:jane@example.com\nSUMMARY:Test\nEND:VEVENT"))
    (let ((result (calendar-sync--parse-organizer event)))
      (should (plist-get result :email)))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-organizer-error-nil-event ()
  "Test nil event returns nil."
  (should (null (calendar-sync--parse-organizer nil))))

(provide 'test-calendar-sync--parse-organizer)
;;; test-calendar-sync--parse-organizer.el ends here
