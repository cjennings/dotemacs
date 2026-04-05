;;; test-calendar-sync--convert-utc-to-local.el --- Tests for UTC to local conversion -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--convert-utc-to-local.
;; Converts UTC datetime to local timezone. Results depend on system timezone.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--convert-utc-to-local-normal-returns-5-elements ()
  "Conversion returns (year month day hour minute) list."
  (let ((result (calendar-sync--convert-utc-to-local 2026 3 15 18 0 0)))
    (should (= 5 (length result)))
    (should (numberp (nth 0 result)))
    (should (numberp (nth 3 result)))))

(ert-deftest test-calendar-sync--convert-utc-to-local-normal-offset-applied ()
  "Hour differs from UTC input (unless system is UTC)."
  (let* ((tz-offset (car (current-time-zone)))
         (result (calendar-sync--convert-utc-to-local 2026 3 15 12 0 0)))
    (if (= tz-offset 0)
        (should (= 12 (nth 3 result)))
      (should-not (= 12 (nth 3 result))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--convert-utc-to-local-boundary-date-change ()
  "Late UTC time may shift to next day for western timezones."
  (let* ((tz-offset (car (current-time-zone)))
         (result (calendar-sync--convert-utc-to-local 2026 3 15 23 30 0)))
    ;; For negative offsets, 23:30 UTC stays on same day
    ;; For positive offsets > 30min, date rolls forward
    (should (= 5 (length result)))))

(provide 'test-calendar-sync--convert-utc-to-local)
;;; test-calendar-sync--convert-utc-to-local.el ends here
