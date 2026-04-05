;;; test-calendar-sync--format-timestamp.el --- Tests for org timestamp formatting -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--format-timestamp.
;; Converts parsed start/end times to org-mode timestamp strings.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--format-timestamp-normal-with-time ()
  "Timed event produces '<YYYY-MM-DD Day HH:MM-HH:MM>'."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) '(2026 3 15 15 30))))
    (should (string-match-p "^<2026-03-15 .* 14:00-15:30>$" result))))

(ert-deftest test-calendar-sync--format-timestamp-normal-all-day ()
  "All-day event (nil hours) produces '<YYYY-MM-DD Day>'."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 nil nil) '(2026 3 16 nil nil))))
    (should (string-match-p "^<2026-03-15 .*>$" result))
    (should-not (string-match-p "[0-9]:[0-9]" result))))

(ert-deftest test-calendar-sync--format-timestamp-normal-includes-weekday ()
  "Timestamp includes abbreviated weekday name."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) '(2026 3 15 15 0))))
    ;; 2026-03-15 is a Sunday
    (should (string-match-p "Sun" result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--format-timestamp-boundary-midnight ()
  "Midnight start and end are formatted as 00:00."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 1 1 0 0) '(2026 1 1 1 0))))
    (should (string-match-p "00:00-01:00" result))))

(ert-deftest test-calendar-sync--format-timestamp-boundary-nil-end ()
  "Nil end with timed start still produces time range."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 14 0) nil)))
    ;; No end time means no time range
    (should (string-match-p "^<2026-03-15" result))))

;;; Error Cases

(ert-deftest test-calendar-sync--format-timestamp-error-start-no-time-end-has-time ()
  "All-day start with timed end produces date-only (no time range)."
  (let ((result (calendar-sync--format-timestamp
                 '(2026 3 15 nil nil) '(2026 3 15 15 0))))
    ;; start-hour is nil so time-str should be nil
    (should-not (string-match-p "[0-9][0-9]:[0-9][0-9]-" result))))

(provide 'test-calendar-sync--format-timestamp)
;;; test-calendar-sync--format-timestamp.el ends here
