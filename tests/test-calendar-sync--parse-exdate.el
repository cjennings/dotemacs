;;; test-calendar-sync--parse-exdate.el --- Tests for EXDATE parsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--parse-exdate function.
;; Tests parsing EXDATE values into (year month day hour minute) lists.
;; Following quality-engineer.org guidelines: one function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-exdate-normal-datetime-returns-list ()
  "Test parsing standard datetime format returns correct list."
  (let ((result (calendar-sync--parse-exdate "20260203T130000")))
    (should (equal '(2026 2 3 13 0) result))))

(ert-deftest test-calendar-sync--parse-exdate-normal-with-z-returns-list ()
  "Test parsing UTC datetime with Z suffix returns correct list."
  (let ((result (calendar-sync--parse-exdate "20260203T180000Z")))
    (should (equal '(2026 2 3 18 0) result))))

(ert-deftest test-calendar-sync--parse-exdate-normal-date-only-returns-list ()
  "Test parsing date-only format returns list with nil for time."
  (let ((result (calendar-sync--parse-exdate "20260203")))
    (should (equal '(2026 2 3 nil nil) result))))

(ert-deftest test-calendar-sync--parse-exdate-normal-with-seconds-returns-list ()
  "Test parsing datetime ignores seconds."
  (let ((result (calendar-sync--parse-exdate "20260203T130045")))
    (should (equal '(2026 2 3 13 0) result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-exdate-boundary-midnight-returns-zero-hour ()
  "Test parsing midnight time returns hour=0."
  (let ((result (calendar-sync--parse-exdate "20260203T000000")))
    (should (equal '(2026 2 3 0 0) result))))

(ert-deftest test-calendar-sync--parse-exdate-boundary-end-of-day-returns-23 ()
  "Test parsing end-of-day time returns hour=23."
  (let ((result (calendar-sync--parse-exdate "20260203T235900")))
    (should (equal '(2026 2 3 23 59) result))))

(ert-deftest test-calendar-sync--parse-exdate-boundary-leap-year-feb29-returns-correct ()
  "Test parsing Feb 29 on leap year."
  (let ((result (calendar-sync--parse-exdate "20280229T120000")))
    (should (equal '(2028 2 29 12 0) result))))

(ert-deftest test-calendar-sync--parse-exdate-boundary-new-years-eve-returns-correct ()
  "Test parsing Dec 31."
  (let ((result (calendar-sync--parse-exdate "20261231T235900")))
    (should (equal '(2026 12 31 23 59) result))))

(ert-deftest test-calendar-sync--parse-exdate-boundary-jan-1-returns-correct ()
  "Test parsing Jan 1."
  (let ((result (calendar-sync--parse-exdate "20260101T000000")))
    (should (equal '(2026 1 1 0 0) result))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-exdate-error-empty-returns-nil ()
  "Test that empty string returns nil."
  (should (null (calendar-sync--parse-exdate ""))))

(ert-deftest test-calendar-sync--parse-exdate-error-nil-returns-nil ()
  "Test that nil input returns nil."
  (should (null (calendar-sync--parse-exdate nil))))

(ert-deftest test-calendar-sync--parse-exdate-error-invalid-format-returns-nil ()
  "Test that invalid format returns nil."
  (should (null (calendar-sync--parse-exdate "not-a-date"))))

(provide 'test-calendar-sync--parse-exdate)
;;; test-calendar-sync--parse-exdate.el ends here
