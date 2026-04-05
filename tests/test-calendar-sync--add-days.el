;;; test-calendar-sync--add-days.el --- Tests for day arithmetic -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--add-days. Uses noon internally for DST safety.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--add-days-normal-forward ()
  "Adding 7 days advances by one week."
  (should (equal '(2026 3 22) (calendar-sync--add-days '(2026 3 15) 7))))

(ert-deftest test-calendar-sync--add-days-normal-backward ()
  "Negative days go backward."
  (should (equal '(2026 3 8) (calendar-sync--add-days '(2026 3 15) -7))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--add-days-boundary-month-rollover ()
  "Adding days past end of month rolls into next month."
  (should (equal '(2026 4 1) (calendar-sync--add-days '(2026 3 31) 1))))

(ert-deftest test-calendar-sync--add-days-boundary-leap-year ()
  "Feb 28 + 1 = Feb 29 in leap year 2028."
  (should (equal '(2028 2 29) (calendar-sync--add-days '(2028 2 28) 1))))

(ert-deftest test-calendar-sync--add-days-boundary-zero ()
  "Adding zero days returns same date."
  (should (equal '(2026 3 15) (calendar-sync--add-days '(2026 3 15) 0))))

;;; Error Cases

(ert-deftest test-calendar-sync--add-days-error-large-offset ()
  "Adding 365 days crosses into next year."
  (let ((result (calendar-sync--add-days '(2026 1 1) 365)))
    (should (equal 2027 (nth 0 result)))))

(provide 'test-calendar-sync--add-days)
;;; test-calendar-sync--add-days.el ends here
