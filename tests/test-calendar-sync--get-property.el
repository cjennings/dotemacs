;;; test-calendar-sync--get-property.el --- Tests for calendar-sync--get-property  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--get-property function.
;; Tests property extraction from iCalendar event strings,
;; especially with property parameters like TZID.
;;
;; Critical: This function had a bug where it couldn't parse
;; properties with parameters (e.g., DTSTART;TZID=America/Chicago:...)
;; These tests prevent regression of that bug.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Setup and Teardown

(defun test-calendar-sync--get-property-setup ()
  "Setup for calendar-sync--get-property tests."
  nil)

(defun test-calendar-sync--get-property-teardown ()
  "Teardown for calendar-sync--get-property tests."
  nil)

;;; Normal Cases

(ert-deftest test-calendar-sync--get-property-normal-simple-property-returns-value ()
  "Test extracting simple property without parameters."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nSUMMARY:Test Event\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "SUMMARY") "Test Event")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-normal-dtstart-without-tzid-returns-value ()
  "Test extracting DTSTART without timezone parameter."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDTSTART:20251118T140000Z\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "DTSTART") "20251118T140000Z")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-normal-dtstart-with-tzid-returns-value ()
  "Test extracting DTSTART with TZID parameter (the bug we fixed)."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDTSTART;TZID=America/Chicago:20251118T140000\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "DTSTART") "20251118T140000")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-normal-location-returns-value ()
  "Test extracting LOCATION property."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nLOCATION:Conference Room A\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "LOCATION") "Conference Room A")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-normal-description-returns-value ()
  "Test extracting DESCRIPTION property."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDESCRIPTION:This is a test event\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "DESCRIPTION") "This is a test event")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-normal-rrule-returns-value ()
  "Test extracting RRULE property."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nRRULE:FREQ=WEEKLY;BYDAY=SA\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "RRULE") "FREQ=WEEKLY;BYDAY=SA")))
    (test-calendar-sync--get-property-teardown)))

;;; Boundary Cases

(ert-deftest test-calendar-sync--get-property-boundary-value-param-with-multiple-params-returns-value ()
  "Test extracting property with multiple parameters."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDTSTART;TZID=America/Chicago;VALUE=DATE-TIME:20251118T140000\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "DTSTART") "20251118T140000")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-boundary-complex-tzid-returns-value ()
  "Test extracting property with complex timezone ID."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDTSTART;TZID=America/Argentina/Buenos_Aires:20251118T140000\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "DTSTART") "20251118T140000")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-boundary-empty-value-returns-empty-string ()
  "Test extracting property with empty value."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDESCRIPTION:\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "DESCRIPTION") "")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-boundary-property-at-start-returns-value ()
  "Test extracting property when it's the first line."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "SUMMARY:First Property\nDTSTART:20251118T140000Z"))
        (should (equal (calendar-sync--get-property event "SUMMARY") "First Property")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-boundary-property-at-end-returns-value ()
  "Test extracting property when it's the last line."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "DTSTART:20251118T140000Z\nSUMMARY:Last Property"))
        (should (equal (calendar-sync--get-property event "SUMMARY") "Last Property")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-boundary-value-with-special-chars-returns-value ()
  "Test extracting property value with special characters."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nLOCATION:Room 123, Building A (Main Campus)\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "LOCATION") "Room 123, Building A (Main Campus)")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-boundary-value-with-semicolons-returns-value ()
  "Test extracting property value containing semicolons."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDESCRIPTION:Tasks: setup; review; deploy\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "DESCRIPTION") "Tasks: setup; review; deploy")))
    (test-calendar-sync--get-property-teardown)))

;;; Error Cases

(ert-deftest test-calendar-sync--get-property-error-missing-property-returns-nil ()
  "Test extracting non-existent property returns nil."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nSUMMARY:Test Event\nEND:VEVENT"))
        (should (null (calendar-sync--get-property event "LOCATION"))))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-error-empty-event-returns-nil ()
  "Test extracting property from empty event string."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (should (null (calendar-sync--get-property "" "SUMMARY")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-error-malformed-property-returns-nil ()
  "Test extracting property with missing colon.
Malformed properties without colons should not match."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nSUMMARY Test Event\nEND:VEVENT"))
        ;; Space instead of colon - should not match
        (should (null (calendar-sync--get-property event "SUMMARY"))))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-boundary-case-insensitive-returns-value ()
  "Test that property matching is case-insensitive per RFC 5545.
iCalendar spec requires property names to be case-insensitive."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nsummary:Test Event\nEND:VEVENT"))
        (should (equal (calendar-sync--get-property event "SUMMARY") "Test Event")))
    (test-calendar-sync--get-property-teardown)))

(ert-deftest test-calendar-sync--get-property-error-property-in-value-not-matched ()
  "Test that property name in another property's value is not matched."
  (test-calendar-sync--get-property-setup)
  (unwind-protect
      (let ((event "BEGIN:VEVENT\nDESCRIPTION:SUMMARY: Not a real summary\nEND:VEVENT"))
        (should (null (calendar-sync--get-property event "SUMMARY"))))
    (test-calendar-sync--get-property-teardown)))

(provide 'test-calendar-sync--get-property)
;;; test-calendar-sync--get-property.el ends here
