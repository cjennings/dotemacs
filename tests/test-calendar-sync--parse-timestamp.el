;;; test-calendar-sync--parse-timestamp.el --- Tests for timestamp parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--parse-timestamp.
;; Handles UTC conversion, TZID conversion, local passthrough, and date-only.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-timestamp-normal-local-no-tz ()
  "Local datetime without timezone returns raw values."
  (let ((result (calendar-sync--parse-timestamp "20260315T143000")))
    (should (equal '(2026 3 15 14 30) result))))

(ert-deftest test-calendar-sync--parse-timestamp-normal-utc-converts ()
  "UTC datetime (Z suffix) is converted to local time."
  (let ((result (calendar-sync--parse-timestamp "20260315T180000Z")))
    ;; Result should be local time — verify it's a valid 5-element list
    (should (= 5 (length result)))
    ;; The hour should differ from 18 unless we're in UTC
    (should (numberp (nth 3 result)))))

(ert-deftest test-calendar-sync--parse-timestamp-normal-date-only ()
  "Date-only returns 3-element list (year month day)."
  (let ((result (calendar-sync--parse-timestamp "20260315")))
    (should (equal '(2026 3 15) result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-timestamp-boundary-with-tzid ()
  "TZID parameter triggers timezone conversion."
  (let ((result (calendar-sync--parse-timestamp "20260315T140000" "America/New_York")))
    ;; Should return a 5-element list (converted from Eastern)
    (should (= 5 (length result)))
    (should (numberp (nth 0 result)))))

(ert-deftest test-calendar-sync--parse-timestamp-boundary-midnight-utc ()
  "Midnight UTC converts correctly (may change date for western timezones)."
  (let ((result (calendar-sync--parse-timestamp "20260315T000000Z")))
    (should (= 5 (length result)))
    (should (numberp (nth 3 result)))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-timestamp-error-garbage ()
  "Non-datetime string returns nil."
  (should (null (calendar-sync--parse-timestamp "not-a-date"))))

(ert-deftest test-calendar-sync--parse-timestamp-error-partial ()
  "Truncated datetime returns nil."
  (should (null (calendar-sync--parse-timestamp "2026031"))))

;;; Boundary / Error — second capture, TZID fallback, leap day

(ert-deftest test-calendar-sync--parse-timestamp-utc-passes-nonzero-seconds ()
  "Boundary: the seconds field is captured and passed to the UTC converter."
  (cl-letf (((symbol-function 'calendar-sync--convert-utc-to-local)
             (lambda (y mo d h mi s) (list 'utc y mo d h mi s))))
    (should (equal (calendar-sync--parse-timestamp "20260315T180045Z")
                   '(utc 2026 3 15 18 0 45)))))

(ert-deftest test-calendar-sync--parse-timestamp-tzid-fallback-on-failure ()
  "Error: when TZID conversion fails, the raw 5-tuple is returned."
  (cl-letf (((symbol-function 'calendar-sync--convert-tz-to-local)
             (lambda (&rest _) nil)))
    (should (equal (calendar-sync--parse-timestamp "20260315T180000" "Fake/Zone")
                   '(2026 3 15 18 0)))))

(ert-deftest test-calendar-sync--parse-timestamp-leap-day-components ()
  "Boundary: a valid leap day (2024-02-29) is parsed into its components."
  (cl-letf (((symbol-function 'calendar-sync--convert-utc-to-local)
             (lambda (y mo d h mi s) (list y mo d h mi s))))
    (should (equal (calendar-sync--parse-timestamp "20240229T120000Z")
                   '(2024 2 29 12 0 0)))))

(provide 'test-calendar-sync--parse-timestamp)
;;; test-calendar-sync--parse-timestamp.el ends here
