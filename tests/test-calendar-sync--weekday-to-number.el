;;; test-calendar-sync--weekday-to-number.el --- Tests for weekday mapping -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--weekday-to-number. Pure string→number mapping.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--weekday-to-number-normal-monday ()
  "MO maps to 1."
  (should (= 1 (calendar-sync--weekday-to-number "MO"))))

(ert-deftest test-calendar-sync--weekday-to-number-normal-all-days ()
  "All seven days map to 1-7."
  (should (equal '(1 2 3 4 5 6 7)
                 (mapcar #'calendar-sync--weekday-to-number
                         '("MO" "TU" "WE" "TH" "FR" "SA" "SU")))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--weekday-to-number-boundary-sunday ()
  "SU maps to 7 (not 0)."
  (should (= 7 (calendar-sync--weekday-to-number "SU"))))

;;; Error Cases

(ert-deftest test-calendar-sync--weekday-to-number-error-invalid ()
  "Invalid weekday string returns nil."
  (should (null (calendar-sync--weekday-to-number "XX"))))

(provide 'test-calendar-sync--weekday-to-number)
;;; test-calendar-sync--weekday-to-number.el ends here
