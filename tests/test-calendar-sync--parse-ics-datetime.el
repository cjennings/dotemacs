;;; test-calendar-sync--parse-ics-datetime.el --- Tests for iCal datetime parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--parse-ics-datetime.
;; Covers three formats: UTC datetime (Z suffix), local datetime, date-only.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-ics-datetime-normal-utc ()
  "UTC datetime (Z suffix) returns (year month day hour minute)."
  (let ((result (calendar-sync--parse-ics-datetime "20260203T090000Z")))
    (should (equal '(2026 2 3 9 0) result))))

(ert-deftest test-calendar-sync--parse-ics-datetime-normal-local ()
  "Local datetime (no Z) returns (year month day hour minute)."
  (let ((result (calendar-sync--parse-ics-datetime "20260315T143000")))
    (should (equal '(2026 3 15 14 30) result))))

(ert-deftest test-calendar-sync--parse-ics-datetime-normal-date-only ()
  "Date-only returns (year month day nil nil)."
  (let ((result (calendar-sync--parse-ics-datetime "20260203")))
    (should (equal '(2026 2 3 nil nil) result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-ics-datetime-boundary-midnight ()
  "Midnight is parsed as hour 0, minute 0."
  (let ((result (calendar-sync--parse-ics-datetime "20260101T000000Z")))
    (should (equal '(2026 1 1 0 0) result))))

(ert-deftest test-calendar-sync--parse-ics-datetime-boundary-end-of-day ()
  "23:59 is parsed correctly."
  (let ((result (calendar-sync--parse-ics-datetime "20261231T235900")))
    (should (equal '(2026 12 31 23 59) result))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-ics-datetime-error-nil ()
  "Nil input returns nil."
  (should (null (calendar-sync--parse-ics-datetime nil))))

(ert-deftest test-calendar-sync--parse-ics-datetime-error-empty-string ()
  "Empty string returns nil."
  (should (null (calendar-sync--parse-ics-datetime ""))))

(provide 'test-calendar-sync--parse-ics-datetime)
;;; test-calendar-sync--parse-ics-datetime.el ends here
