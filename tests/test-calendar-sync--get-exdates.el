;;; test-calendar-sync--get-exdates.el --- Tests for EXDATE extraction  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--get-exdates function.
;; Tests extraction of EXDATE properties from VEVENT strings.
;; Following quality-engineer.org guidelines: one function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--get-exdates-normal-single-returns-list ()
  "Test extracting single EXDATE returns list with one value."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE:20260210T130000
SUMMARY:Weekly Meeting
END:VEVENT"))
    (let ((result (calendar-sync--get-exdates event)))
      (should (listp result))
      (should (= 1 (length result)))
      (should (string= "20260210T130000" (car result))))))

(ert-deftest test-calendar-sync--get-exdates-normal-multiple-returns-all ()
  "Test extracting multiple EXDATEs returns all values."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE:20260210T130000
EXDATE:20260217T130000
EXDATE:20260224T130000
SUMMARY:Weekly Meeting
END:VEVENT"))
    (let ((result (calendar-sync--get-exdates event)))
      (should (= 3 (length result)))
      (should (member "20260210T130000" result))
      (should (member "20260217T130000" result))
      (should (member "20260224T130000" result)))))

(ert-deftest test-calendar-sync--get-exdates-normal-with-tzid-returns-value ()
  "Test extracting EXDATE with TZID parameter extracts value correctly."
  (let ((event "BEGIN:VEVENT
DTSTART;TZID=America/New_York:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE;TZID=America/New_York:20260210T130000
SUMMARY:Weekly Meeting
END:VEVENT"))
    (let ((result (calendar-sync--get-exdates event)))
      (should (= 1 (length result)))
      (should (string= "20260210T130000" (car result))))))

(ert-deftest test-calendar-sync--get-exdates-normal-with-z-suffix-returns-value ()
  "Test extracting UTC EXDATE with Z suffix."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T180000Z
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE:20260210T180000Z
SUMMARY:Weekly UTC Meeting
END:VEVENT"))
    (let ((result (calendar-sync--get-exdates event)))
      (should (= 1 (length result)))
      (should (string= "20260210T180000Z" (car result))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--get-exdates-boundary-no-exdate-returns-nil ()
  "Test that event without EXDATE returns nil."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
SUMMARY:Weekly Meeting
END:VEVENT"))
    (should (null (calendar-sync--get-exdates event)))))

(ert-deftest test-calendar-sync--get-exdates-boundary-date-only-returns-value ()
  "Test extracting all-day EXDATE (date only, no time)."
  (let ((event "BEGIN:VEVENT
DTSTART;VALUE=DATE:20260203
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE;VALUE=DATE:20260210
SUMMARY:All Day Event
END:VEVENT"))
    (let ((result (calendar-sync--get-exdates event)))
      (should (= 1 (length result)))
      (should (string= "20260210" (car result))))))

(ert-deftest test-calendar-sync--get-exdates-boundary-multiple-params-returns-value ()
  "Test extracting EXDATE with VALUE=DATE-TIME and TZID parameters."
  (let ((event "BEGIN:VEVENT
DTSTART;TZID=America/Chicago:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE;VALUE=DATE-TIME;TZID=America/Chicago:20260210T130000
SUMMARY:Multi-param Meeting
END:VEVENT"))
    (let ((result (calendar-sync--get-exdates event)))
      (should (= 1 (length result)))
      (should (string= "20260210T130000" (car result))))))

;;; Error Cases

(ert-deftest test-calendar-sync--get-exdates-error-empty-string-returns-nil ()
  "Test that empty string returns nil."
  (should (null (calendar-sync--get-exdates ""))))

(ert-deftest test-calendar-sync--get-exdates-error-nil-input-returns-nil ()
  "Test that nil input returns nil."
  (should (null (calendar-sync--get-exdates nil))))

(ert-deftest test-calendar-sync--get-exdates-error-malformed-returns-nil ()
  "Test that malformed event string returns nil."
  (should (null (calendar-sync--get-exdates "not a vevent"))))

(provide 'test-calendar-sync--get-exdates)
;;; test-calendar-sync--get-exdates.el ends here
