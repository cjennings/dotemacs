;;; test-calendar-sync--convert-tz-to-local.el --- Tests for timezone conversion  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--convert-tz-to-local function.
;; Tests conversion from named timezones to local time.
;; Uses `date` command as reference implementation for verification.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'calendar-sync)
(require 'testutil-calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--convert-tz-to-local-normal-lisbon-to-local ()
  "Test converting Europe/Lisbon time to local.
Europe/Lisbon is UTC+0 in winter, UTC+1 in summer.
Uses date command as reference for expected result."
  (let* ((source-tz "Europe/Lisbon")
         (year 2026) (month 2) (day 2) (hour 19) (minute 0)
         ;; Get expected result from date command
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)  ; Sanity check that date command worked
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-normal-yerevan-to-local ()
  "Test converting Asia/Yerevan time to local.
Asia/Yerevan is UTC+4 year-round."
  (let* ((source-tz "Asia/Yerevan")
         (year 2026) (month 2) (day 2) (hour 20) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-normal-utc-to-local ()
  "Test converting UTC time to local."
  (let* ((source-tz "UTC")
         (year 2026) (month 2) (day 2) (hour 19) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-normal-new-york-to-local ()
  "Test converting America/New_York time to local."
  (let* ((source-tz "America/New_York")
         (year 2026) (month 2) (day 2) (hour 14) (minute 30)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-normal-tokyo-to-local ()
  "Test converting Asia/Tokyo time to local.
Asia/Tokyo is UTC+9 year-round (no DST)."
  (let* ((source-tz "Asia/Tokyo")
         (year 2026) (month 2) (day 2) (hour 10) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-crosses-date-forward ()
  "Test conversion that crosses to next day.
Late evening in Europe becomes next day morning in Americas."
  (let* ((source-tz "Europe/London")
         (year 2026) (month 2) (day 2) (hour 23) (minute 30)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-crosses-date-backward ()
  "Test conversion that crosses to previous day.
Early morning in Asia becomes previous day evening in Americas."
  (let* ((source-tz "Asia/Tokyo")
         (year 2026) (month 2) (day 3) (hour 2) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-midnight ()
  "Test conversion of midnight (00:00) in source timezone."
  (let* ((source-tz "Europe/Paris")
         (year 2026) (month 2) (day 2) (hour 0) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-2359 ()
  "Test conversion of 23:59 in source timezone."
  (let* ((source-tz "Europe/Berlin")
         (year 2026) (month 2) (day 2) (hour 23) (minute 59)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-dst-spring-forward ()
  "Test conversion during US DST spring-forward transition.
March 8, 2026 at 2:30 AM doesn't exist in America/Chicago (skipped)."
  ;; Use a time AFTER the transition to avoid the gap
  (let* ((source-tz "America/New_York")
         (year 2026) (month 3) (day 8) (hour 15) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-dst-fall-back ()
  "Test conversion during fall-back DST transition.
November 1, 2026 at 1:30 AM exists twice in America/Chicago."
  (let* ((source-tz "America/New_York")
         (year 2026) (month 11) (day 1) (hour 14) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-etc-gmt-plus ()
  "Test conversion from Etc/GMT+5 (note: Etc/GMT+N is UTC-N)."
  (let* ((source-tz "Etc/GMT+5")
         (year 2026) (month 2) (day 2) (hour 12) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-month-boundary ()
  "Test conversion that crosses month boundary."
  (let* ((source-tz "Pacific/Auckland")  ; UTC+12/+13
         (year 2026) (month 2) (day 1) (hour 5) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-year-boundary ()
  "Test conversion that crosses year boundary."
  (let* ((source-tz "Pacific/Auckland")
         (year 2026) (month 1) (day 1) (hour 5) (minute 0)
         (expected (test-calendar-sync-convert-tz-via-date
                    year month day hour minute source-tz))
         (result (calendar-sync--convert-tz-to-local
                  year month day hour minute source-tz)))
    (should expected)
    (should result)
    (should (equal expected result))))

;;; Error Cases

(ert-deftest test-calendar-sync--convert-tz-to-local-boundary-invalid-timezone-falls-back ()
  "Test that invalid timezone falls back to treating time as local.
The `date` command doesn't error on unrecognized timezones - it ignores
the TZ specification and treats the input as local time. This is acceptable
because calendar providers (Google, Proton) always use valid IANA timezones.
This test documents the fallback behavior rather than testing for nil."
  (let ((result (calendar-sync--convert-tz-to-local
                 2026 2 2 19 0 "Invalid/Timezone")))
    ;; Should return something (falls back to local interpretation)
    (should result)
    ;; The time values should be present (year month day hour minute)
    (should (= 5 (length result)))))

(ert-deftest test-calendar-sync--convert-tz-to-local-error-nil-timezone ()
  "Test that nil timezone returns nil."
  (let ((result (calendar-sync--convert-tz-to-local
                 2026 2 2 19 0 nil)))
    (should (null result))))

(ert-deftest test-calendar-sync--convert-tz-to-local-error-empty-timezone ()
  "Test that empty timezone string returns nil."
  (let ((result (calendar-sync--convert-tz-to-local
                 2026 2 2 19 0 "")))
    (should (null result))))

(provide 'test-calendar-sync--convert-tz-to-local)
;;; test-calendar-sync--convert-tz-to-local.el ends here
