;;; test-calendar-sync--extract-tzid.el --- Tests for calendar-sync--extract-tzid  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--extract-tzid function.
;; Tests extraction of TZID parameter from iCal property lines.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--extract-tzid-normal-europe-lisbon ()
  "Test extracting TZID=Europe/Lisbon from standard DTSTART."
  (let ((prop-line "DTSTART;TZID=Europe/Lisbon:20260202T190000"))
    (should (string= "Europe/Lisbon"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-normal-asia-yerevan ()
  "Test extracting TZID=Asia/Yerevan from DTSTART."
  (let ((prop-line "DTSTART;TZID=Asia/Yerevan:20230801T200000"))
    (should (string= "Asia/Yerevan"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-normal-america-chicago ()
  "Test extracting TZID=America/Chicago from DTSTART."
  (let ((prop-line "DTSTART;TZID=America/Chicago:20230721T160000"))
    (should (string= "America/Chicago"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-normal-dtend ()
  "Test extracting TZID from DTEND property."
  (let ((prop-line "DTEND;TZID=America/New_York:20260202T200000"))
    (should (string= "America/New_York"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-normal-multiple-params ()
  "Test extracting TZID when other parameters present."
  (let ((prop-line "DTSTART;VALUE=DATE-TIME;TZID=UTC:20260202T190000"))
    (should (string= "UTC"
                     (calendar-sync--extract-tzid prop-line)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--extract-tzid-boundary-underscore-in-name ()
  "Test extracting TZID with underscore (America/New_York)."
  (let ((prop-line "DTSTART;TZID=America/New_York:20260202T190000"))
    (should (string= "America/New_York"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-boundary-etc-gmt-plus ()
  "Test extracting TZID with plus sign (Etc/GMT+5)."
  (let ((prop-line "DTSTART;TZID=Etc/GMT+5:20260202T190000"))
    (should (string= "Etc/GMT+5"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-boundary-etc-gmt-minus ()
  "Test extracting TZID with minus sign (Etc/GMT-5)."
  (let ((prop-line "DTSTART;TZID=Etc/GMT-5:20260202T190000"))
    (should (string= "Etc/GMT-5"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-boundary-tzid-after-other-params ()
  "Test extracting TZID when it appears after other parameters."
  (let ((prop-line "DTSTART;VALUE=DATE-TIME;TZID=Asia/Tokyo:20260202T190000"))
    (should (string= "Asia/Tokyo"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-boundary-tzid-before-other-params ()
  "Test extracting TZID when it appears before other parameters."
  (let ((prop-line "DTSTART;TZID=Pacific/Auckland;VALUE=DATE-TIME:20260202T190000"))
    (should (string= "Pacific/Auckland"
                     (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-boundary-long-timezone-name ()
  "Test extracting TZID with long timezone name."
  (let ((prop-line "DTSTART;TZID=America/Argentina/Buenos_Aires:20260202T190000"))
    (should (string= "America/Argentina/Buenos_Aires"
                     (calendar-sync--extract-tzid prop-line)))))

;;; Error Cases

(ert-deftest test-calendar-sync--extract-tzid-error-no-tzid-utc ()
  "Test that UTC timestamp (with Z) returns nil for TZID."
  (let ((prop-line "DTSTART:20260202T190000Z"))
    (should (null (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-error-no-tzid-local ()
  "Test that local timestamp (no Z, no TZID) returns nil."
  (let ((prop-line "DTSTART:20260202T190000"))
    (should (null (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-error-empty-string ()
  "Test that empty string returns nil."
  (should (null (calendar-sync--extract-tzid ""))))

(ert-deftest test-calendar-sync--extract-tzid-error-nil-input ()
  "Test that nil input returns nil."
  (should (null (calendar-sync--extract-tzid nil))))

(ert-deftest test-calendar-sync--extract-tzid-error-value-date-only ()
  "Test that VALUE=DATE (all-day event) returns nil."
  (let ((prop-line "DTSTART;VALUE=DATE:20260202"))
    (should (null (calendar-sync--extract-tzid prop-line)))))

(ert-deftest test-calendar-sync--extract-tzid-error-malformed-no-colon ()
  "Test that malformed line without colon returns nil."
  (let ((prop-line "DTSTART;TZID=Europe/Lisbon"))
    (should (null (calendar-sync--extract-tzid prop-line)))))

(provide 'test-calendar-sync--extract-tzid)
;;; test-calendar-sync--extract-tzid.el ends here
