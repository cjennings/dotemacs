;;; test-calendar-sync--nth-weekday-of-month.el --- Tests for calendar-sync--nth-weekday-of-month  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the nth-weekday-of-month helper backing monthly/yearly BYDAY
;; expansion.  Fixed dates are safe here: the function is pure calendar
;; arithmetic with no relation to the current time.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--nth-weekday-of-month-normal-second-wednesday ()
  "Normal: 2nd Wednesday of Jan 2026 is the 14th."
  ;; Jan 2026: Jan 1 is a Thursday; Wednesdays fall on 7, 14, 21, 28.
  (should (= (calendar-sync--nth-weekday-of-month 2026 1 3 2) 14)))

(ert-deftest test-calendar-sync--nth-weekday-of-month-normal-first-monday ()
  "Normal: 1st Monday of Feb 2026 is the 2nd."
  ;; Feb 2026: Feb 1 is a Sunday; Mondays fall on 2, 9, 16, 23.
  (should (= (calendar-sync--nth-weekday-of-month 2026 2 1 1) 2)))

(ert-deftest test-calendar-sync--nth-weekday-of-month-normal-last-tuesday ()
  "Normal: last Tuesday of Mar 2026 is the 31st (negative ordinal)."
  ;; Mar 2026: Tuesdays fall on 3, 10, 17, 24, 31.
  (should (= (calendar-sync--nth-weekday-of-month 2026 3 2 -1) 31)))

(ert-deftest test-calendar-sync--nth-weekday-of-month-normal-second-to-last-friday ()
  "Normal: -2 ordinal picks the second-to-last Friday."
  ;; May 2026: Fridays fall on 1, 8, 15, 22, 29.
  (should (= (calendar-sync--nth-weekday-of-month 2026 5 5 -2) 22)))

;;; Boundary Cases

(ert-deftest test-calendar-sync--nth-weekday-of-month-boundary-fifth-occurrence-exists ()
  "Boundary: 5th Friday exists in May 2026."
  (should (= (calendar-sync--nth-weekday-of-month 2026 5 5 5) 29)))

(ert-deftest test-calendar-sync--nth-weekday-of-month-boundary-fifth-occurrence-missing ()
  "Boundary: 5th Wednesday of Feb 2026 does not exist -- returns nil."
  ;; Feb 2026 has four Wednesdays (4, 11, 18, 25).
  (should (null (calendar-sync--nth-weekday-of-month 2026 2 3 5))))

(ert-deftest test-calendar-sync--nth-weekday-of-month-boundary-first-day-is-target ()
  "Boundary: the 1st of the month itself is the 1st occurrence."
  ;; Apr 2026: Apr 1 is a Wednesday.
  (should (= (calendar-sync--nth-weekday-of-month 2026 4 3 1) 1)))

(ert-deftest test-calendar-sync--nth-weekday-of-month-boundary-leap-february ()
  "Boundary: leap-year February (2028) handled -- last Tuesday is the 29th."
  ;; Feb 2028: Feb 29 exists and is a Tuesday.
  (should (= (calendar-sync--nth-weekday-of-month 2028 2 2 -1) 29)))

;;; Error Cases

(ert-deftest test-calendar-sync--nth-weekday-of-month-error-zero-ordinal-nil ()
  "Error: ordinal 0 is meaningless -- returns nil."
  (should (null (calendar-sync--nth-weekday-of-month 2026 1 3 0))))

(ert-deftest test-calendar-sync--nth-weekday-of-month-error-out-of-range-negative-nil ()
  "Error: -6th occurrence never exists in a month -- returns nil."
  (should (null (calendar-sync--nth-weekday-of-month 2026 1 3 -6))))

(provide 'test-calendar-sync--nth-weekday-of-month)
;;; test-calendar-sync--nth-weekday-of-month.el ends here
