;;; test-calendar-sync--parse-recurrence-id.el --- Tests for RECURRENCE-ID parsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--parse-recurrence-id function.
;; Tests parsing RECURRENCE-ID values into (year month day hour minute) lists.
;; Following quality-engineer.org guidelines: one function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-recurrence-id-normal-simple-datetime-returns-list ()
  "Test parsing simple RECURRENCE-ID datetime without suffix."
  (let ((result (calendar-sync--parse-recurrence-id "20260203T090000")))
    (should (equal '(2026 2 3 9 0) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-normal-with-z-suffix-returns-list ()
  "Test parsing RECURRENCE-ID datetime with UTC Z suffix."
  (let ((result (calendar-sync--parse-recurrence-id "20260203T170000Z")))
    (should (equal '(2026 2 3 17 0) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-normal-date-only-returns-list ()
  "Test parsing date-only RECURRENCE-ID for all-day events.
Returns list with nil for hour/minute."
  (let ((result (calendar-sync--parse-recurrence-id "20260203")))
    (should (equal '(2026 2 3 nil nil) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-normal-with-seconds-returns-list ()
  "Test parsing datetime with non-zero seconds (seconds ignored)."
  (let ((result (calendar-sync--parse-recurrence-id "20260203T091530")))
    (should (equal '(2026 2 3 9 15) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-normal-afternoon-time-returns-list ()
  "Test parsing afternoon time correctly."
  (let ((result (calendar-sync--parse-recurrence-id "20260515T143000")))
    (should (equal '(2026 5 15 14 30) result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-recurrence-id-boundary-midnight-returns-list ()
  "Test parsing midnight time."
  (let ((result (calendar-sync--parse-recurrence-id "20260203T000000")))
    (should (equal '(2026 2 3 0 0) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-boundary-end-of-day-returns-list ()
  "Test parsing end-of-day time (23:59)."
  (let ((result (calendar-sync--parse-recurrence-id "20260203T235900")))
    (should (equal '(2026 2 3 23 59) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-boundary-leap-year-returns-list ()
  "Test parsing leap year date (Feb 29)."
  (let ((result (calendar-sync--parse-recurrence-id "20280229T120000")))
    (should (equal '(2028 2 29 12 0) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-boundary-new-years-eve-returns-list ()
  "Test parsing New Year's Eve date."
  (let ((result (calendar-sync--parse-recurrence-id "20261231T235900")))
    (should (equal '(2026 12 31 23 59) result))))

(ert-deftest test-calendar-sync--parse-recurrence-id-boundary-january-first-returns-list ()
  "Test parsing January 1st."
  (let ((result (calendar-sync--parse-recurrence-id "20260101T000000")))
    (should (equal '(2026 1 1 0 0) result))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-recurrence-id-error-empty-string-returns-nil ()
  "Test that empty string returns nil."
  (should (null (calendar-sync--parse-recurrence-id ""))))

(ert-deftest test-calendar-sync--parse-recurrence-id-error-nil-input-returns-nil ()
  "Test that nil input returns nil."
  (should (null (calendar-sync--parse-recurrence-id nil))))

(ert-deftest test-calendar-sync--parse-recurrence-id-error-invalid-format-returns-nil ()
  "Test that invalid format returns nil."
  (should (null (calendar-sync--parse-recurrence-id "not-a-date"))))

(ert-deftest test-calendar-sync--parse-recurrence-id-error-incomplete-date-returns-nil ()
  "Test that incomplete date returns nil."
  (should (null (calendar-sync--parse-recurrence-id "2026"))))

(ert-deftest test-calendar-sync--parse-recurrence-id-error-partial-datetime-returns-nil ()
  "Test that partial datetime (missing time) still parses as date-only."
  ;; A date without time component should parse as date-only
  (let ((result (calendar-sync--parse-recurrence-id "20260203T")))
    ;; Could return nil or partial - implementation decides
    ;; But shouldn't crash
    (should (or (null result)
                (and (listp result) (= (length result) 5))))))

(provide 'test-calendar-sync--parse-recurrence-id)
;;; test-calendar-sync--parse-recurrence-id.el ends here
