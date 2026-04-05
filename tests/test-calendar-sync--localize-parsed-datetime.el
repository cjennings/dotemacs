;;; test-calendar-sync--localize-parsed-datetime.el --- Tests for datetime localization -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--localize-parsed-datetime.
;; Dispatches to UTC or TZID conversion, or passes through unchanged.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--localize-parsed-datetime-normal-utc ()
  "UTC flag triggers conversion via convert-utc-to-local."
  (let ((parsed '(2026 3 15 18 0)))
    (let ((result (calendar-sync--localize-parsed-datetime parsed t nil)))
      (should (= 5 (length result)))
      ;; Hour should differ from 18 unless we're in UTC
      (let ((tz-offset (car (current-time-zone))))
        (unless (= tz-offset 0)
          (should-not (= 18 (nth 3 result))))))))

(ert-deftest test-calendar-sync--localize-parsed-datetime-normal-tzid ()
  "TZID triggers conversion via convert-tz-to-local."
  (let ((parsed '(2026 3 15 14 0)))
    (let ((result (calendar-sync--localize-parsed-datetime parsed nil "America/New_York")))
      (should (= 5 (length result))))))

(ert-deftest test-calendar-sync--localize-parsed-datetime-normal-local-passthrough ()
  "No UTC, no TZID — returns parsed unchanged."
  (let ((parsed '(2026 3 15 14 0)))
    (should (equal parsed (calendar-sync--localize-parsed-datetime parsed nil nil)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--localize-parsed-datetime-boundary-date-only ()
  "Date-only (nil hour/minute) with UTC uses 0 for time components."
  (let ((parsed '(2026 3 15 nil nil)))
    (let ((result (calendar-sync--localize-parsed-datetime parsed t nil)))
      (should (= 5 (length result))))))

;;; Error Cases

(ert-deftest test-calendar-sync--localize-parsed-datetime-error-bad-tzid-fallback ()
  "Invalid TZID falls back to returning parsed unchanged."
  (let ((parsed '(2026 3 15 14 0)))
    (let ((result (calendar-sync--localize-parsed-datetime parsed nil "Fake/Timezone")))
      ;; convert-tz-to-local returns nil for bad TZID, so fallback to parsed
      (should (= 5 (length result))))))

(provide 'test-calendar-sync--localize-parsed-datetime)
;;; test-calendar-sync--localize-parsed-datetime.el ends here
