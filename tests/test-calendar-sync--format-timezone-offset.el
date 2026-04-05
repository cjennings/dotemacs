;;; test-calendar-sync--format-timezone-offset.el --- Tests for timezone offset formatting -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--format-timezone-offset.
;; Converts offset seconds to human-readable "UTC±H:MM" strings.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--format-timezone-offset-normal-negative ()
  "US Central (-6 hours) formats as UTC-6."
  (should (equal "UTC-6" (calendar-sync--format-timezone-offset -21600))))

(ert-deftest test-calendar-sync--format-timezone-offset-normal-positive ()
  "India (+5:30) formats as UTC+5:30."
  (should (equal "UTC+5:30" (calendar-sync--format-timezone-offset 19800))))

(ert-deftest test-calendar-sync--format-timezone-offset-normal-utc ()
  "Zero offset formats as UTC+0."
  (should (equal "UTC+0" (calendar-sync--format-timezone-offset 0))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--format-timezone-offset-boundary-half-hour ()
  "Newfoundland (-3:30) includes minutes."
  (should (equal "UTC-3:30" (calendar-sync--format-timezone-offset -12600))))

(ert-deftest test-calendar-sync--format-timezone-offset-boundary-large-offset ()
  "UTC+14 (Line Islands) formats correctly."
  (should (equal "UTC+14" (calendar-sync--format-timezone-offset 50400))))

;;; Error Cases

(ert-deftest test-calendar-sync--format-timezone-offset-error-nil ()
  "Nil offset returns 'unknown'."
  (should (equal "unknown" (calendar-sync--format-timezone-offset nil))))

(provide 'test-calendar-sync--format-timezone-offset)
;;; test-calendar-sync--format-timezone-offset.el ends here
