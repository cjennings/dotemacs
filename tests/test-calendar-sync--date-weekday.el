;;; test-calendar-sync--date-weekday.el --- Tests for date weekday calculation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--date-weekday. Returns 1 (Mon) through 7 (Sun).

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--date-weekday-normal-known-date ()
  "2026-03-15 is a Sunday (7)."
  (should (= 7 (calendar-sync--date-weekday '(2026 3 15)))))

(ert-deftest test-calendar-sync--date-weekday-normal-monday ()
  "2026-03-16 is a Monday (1)."
  (should (= 1 (calendar-sync--date-weekday '(2026 3 16)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--date-weekday-boundary-sunday-is-7 ()
  "Sunday returns 7, not 0 (Emacs decode-time returns 0 for Sunday)."
  ;; 2026-01-04 is a Sunday
  (should (= 7 (calendar-sync--date-weekday '(2026 1 4)))))

(ert-deftest test-calendar-sync--date-weekday-boundary-saturday ()
  "Saturday returns 6."
  ;; 2026-01-03 is a Saturday
  (should (= 6 (calendar-sync--date-weekday '(2026 1 3)))))

(provide 'test-calendar-sync--date-weekday)
;;; test-calendar-sync--date-weekday.el ends here
