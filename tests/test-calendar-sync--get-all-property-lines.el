;;; test-calendar-sync--get-all-property-lines.el --- Tests for get-all-property-lines  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--get-all-property-lines function.
;; Like get-property-line but returns ALL matching lines.
;; Needed because ATTENDEE appears multiple times in an event.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--get-all-property-lines-normal-multiple ()
  "Test extracting multiple ATTENDEE lines."
  (let ((event "BEGIN:VEVENT\nATTENDEE;CN=Alice:mailto:alice@example.com\nATTENDEE;CN=Bob:mailto:bob@example.com\nEND:VEVENT"))
    (let ((result (calendar-sync--get-all-property-lines event "ATTENDEE")))
      (should (= 2 (length result)))
      (should (string-match-p "Alice" (nth 0 result)))
      (should (string-match-p "Bob" (nth 1 result))))))

(ert-deftest test-calendar-sync--get-all-property-lines-normal-single ()
  "Test extracting single matching line."
  (let ((event "BEGIN:VEVENT\nATTENDEE;CN=Alice:mailto:alice@example.com\nSUMMARY:Test\nEND:VEVENT"))
    (let ((result (calendar-sync--get-all-property-lines event "ATTENDEE")))
      (should (= 1 (length result)))
      (should (string-match-p "Alice" (car result))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--get-all-property-lines-boundary-no-match ()
  "Test no matching property returns empty list."
  (let ((event "BEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT"))
    (should (null (calendar-sync--get-all-property-lines event "ATTENDEE")))))

(ert-deftest test-calendar-sync--get-all-property-lines-boundary-continuation ()
  "Test handling of continuation lines (lines starting with space)."
  (let ((event "BEGIN:VEVENT\nATTENDEE;CN=Very Long Name;PARTSTAT=ACCEPTED:\n mailto:longname@example.com\nSUMMARY:Test\nEND:VEVENT"))
    (let ((result (calendar-sync--get-all-property-lines event "ATTENDEE")))
      (should (= 1 (length result)))
      (should (string-match-p "longname@example.com" (car result))))))

;;; Error Cases

(ert-deftest test-calendar-sync--get-all-property-lines-error-nil-event ()
  "Test nil event returns nil."
  (should (null (calendar-sync--get-all-property-lines nil "ATTENDEE"))))

(ert-deftest test-calendar-sync--get-all-property-lines-error-nil-property ()
  "Test nil property returns nil."
  (should (null (calendar-sync--get-all-property-lines "BEGIN:VEVENT\nEND:VEVENT" nil))))

(ert-deftest test-calendar-sync--get-all-property-lines-error-empty-event ()
  "Test empty event string returns nil."
  (should (null (calendar-sync--get-all-property-lines "" "ATTENDEE"))))

(provide 'test-calendar-sync--get-all-property-lines)
;;; test-calendar-sync--get-all-property-lines.el ends here
