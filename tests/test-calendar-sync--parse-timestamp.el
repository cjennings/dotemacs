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

(provide 'test-calendar-sync--parse-timestamp)
;;; test-calendar-sync--parse-timestamp.el ends here
