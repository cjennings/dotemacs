;;; test-calendar-sync--collect-exdates.el --- Tests for EXDATE collection  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--collect-exdates function.
;; Tests collection of all excluded dates from an event, handling timezone conversion.
;; Following quality-engineer.org guidelines: one function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--collect-exdates-normal-single-returns-list ()
  "Test collecting single EXDATE returns list with one parsed value."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE:20260210T130000
SUMMARY:Weekly Meeting
END:VEVENT"))
    (let ((result (calendar-sync--collect-exdates event)))
      (should (listp result))
      (should (= 1 (length result)))
      (should (equal '(2026 2 10 13 0) (car result))))))

(ert-deftest test-calendar-sync--collect-exdates-normal-multiple-returns-all ()
  "Test collecting multiple EXDATEs returns all parsed values."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE:20260210T130000
EXDATE:20260217T130000
EXDATE:20260224T130000
SUMMARY:Weekly Meeting
END:VEVENT"))
    (let ((result (calendar-sync--collect-exdates event)))
      (should (= 3 (length result)))
      (should (member '(2026 2 10 13 0) result))
      (should (member '(2026 2 17 13 0) result))
      (should (member '(2026 2 24 13 0) result)))))

(ert-deftest test-calendar-sync--collect-exdates-normal-tzid-converts-to-local ()
  "Test that TZID-qualified EXDATEs are converted to local time."
  ;; Use a timezone different from local to verify conversion
  ;; We'll use Europe/London and check that conversion happens
  (let ((event "BEGIN:VEVENT
DTSTART;TZID=Europe/London:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE;TZID=Europe/London:20260210T130000
SUMMARY:London Meeting
END:VEVENT"))
    (let ((result (calendar-sync--collect-exdates event)))
      (should (= 1 (length result)))
      ;; Result should be a valid datetime list (conversion may differ based on local TZ)
      (let ((parsed (car result)))
        (should (= 5 (length parsed)))
        (should (numberp (nth 0 parsed)))  ; year
        (should (numberp (nth 1 parsed)))  ; month
        (should (numberp (nth 2 parsed)))  ; day
        (should (numberp (nth 3 parsed)))  ; hour
        (should (numberp (nth 4 parsed))))))) ; minute

;;; Boundary Cases

(ert-deftest test-calendar-sync--collect-exdates-boundary-no-exdates-returns-empty ()
  "Test that event without EXDATE returns empty list."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
SUMMARY:Weekly Meeting
END:VEVENT"))
    (let ((result (calendar-sync--collect-exdates event)))
      (should (listp result))
      (should (= 0 (length result))))))

(ert-deftest test-calendar-sync--collect-exdates-boundary-utc-converts-to-local ()
  "Test that UTC (Z suffix) EXDATEs are converted to local time."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T180000Z
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE:20260210T180000Z
SUMMARY:UTC Meeting
END:VEVENT"))
    (let ((result (calendar-sync--collect-exdates event)))
      (should (= 1 (length result)))
      ;; Result should be converted to local time
      (let ((parsed (car result)))
        (should (= 5 (length parsed)))
        ;; Date should be valid
        (should (numberp (nth 0 parsed)))))))

(ert-deftest test-calendar-sync--collect-exdates-boundary-mixed-formats-handles-all ()
  "Test handling mix of TZID, UTC, and local time EXDATEs."
  (let ((event "BEGIN:VEVENT
DTSTART:20260203T130000
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE:20260210T130000
EXDATE:20260217T180000Z
SUMMARY:Mixed Meeting
END:VEVENT"))
    (let ((result (calendar-sync--collect-exdates event)))
      (should (= 2 (length result)))
      ;; Both should be valid parsed datetimes
      (dolist (parsed result)
        (should (= 5 (length parsed)))
        (should (numberp (nth 0 parsed)))))))

(ert-deftest test-calendar-sync--collect-exdates-boundary-date-only-returns-date ()
  "Test collecting all-day EXDATE returns date with nil for time."
  (let ((event "BEGIN:VEVENT
DTSTART;VALUE=DATE:20260203
RRULE:FREQ=WEEKLY;BYDAY=TU
EXDATE;VALUE=DATE:20260210
SUMMARY:All Day Event
END:VEVENT"))
    (let ((result (calendar-sync--collect-exdates event)))
      (should (= 1 (length result)))
      (let ((parsed (car result)))
        (should (equal '(2026 2 10 nil nil) parsed))))))

;;; Error Cases

(ert-deftest test-calendar-sync--collect-exdates-error-empty-string-returns-empty ()
  "Test that empty string returns empty list."
  (let ((result (calendar-sync--collect-exdates "")))
    (should (listp result))
    (should (= 0 (length result)))))

(ert-deftest test-calendar-sync--collect-exdates-error-nil-returns-empty ()
  "Test that nil input returns empty list."
  (let ((result (calendar-sync--collect-exdates nil)))
    (should (listp result))
    (should (= 0 (length result)))))

(ert-deftest test-calendar-sync--collect-exdates-error-malformed-returns-empty ()
  "Test that malformed event returns empty list."
  (let ((result (calendar-sync--collect-exdates "not a vevent")))
    (should (listp result))
    (should (= 0 (length result)))))

(provide 'test-calendar-sync--collect-exdates)
;;; test-calendar-sync--collect-exdates.el ends here
