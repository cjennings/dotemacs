;;; test-calendar-sync--expand-monthly.el --- Tests for calendar-sync--expand-monthly  -*- lexical-binding: t; -*-

;;; Commentary:
;; Characterization tests for calendar-sync--expand-monthly.
;; Captures current behavior before refactoring into unified expand function.
;; Uses dynamic timestamps to avoid hardcoded dates.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--expand-monthly-normal-generates-occurrences ()
  "Test expanding monthly event generates occurrences within range."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Monthly Review"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    ;; ~12-15 months of range
    (should (> (length occurrences) 10))
    (should (< (length occurrences) 20))))

(ert-deftest test-calendar-sync--expand-monthly-normal-preserves-day-of-month ()
  "Test that each occurrence falls on the same day of month."
  (let* ((start-date (test-calendar-sync-time-days-from-now 5 10 0))
         (end-date (test-calendar-sync-time-days-from-now 5 11 0))
         (expected-day (nth 2 start-date))
         (base-event (list :summary "Monthly"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (should (> (length occurrences) 0))
    ;; Day of month should be consistent (may clamp for short months)
    (dolist (occ occurrences)
      (let ((day (nth 2 (plist-get occ :start))))
        (should (<= day expected-day))))))

(ert-deftest test-calendar-sync--expand-monthly-normal-interval-two ()
  "Test expanding bi-monthly event."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 9 0))
         (end-date (test-calendar-sync-time-days-from-now 1 10 0))
         (base-event (list :summary "Bi-Monthly"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 2))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    ;; ~15 months / 2 = ~7-8 occurrences
    (should (>= (length occurrences) 5))
    (should (<= (length occurrences) 10))))

(ert-deftest test-calendar-sync--expand-monthly-normal-preserves-time ()
  "Test that each occurrence preserves the original event time."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 15 45))
         (end-date (test-calendar-sync-time-days-from-now 1 16 45))
         (base-event (list :summary "Timed Monthly"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1 :count 3))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (dolist (occ occurrences)
      (let ((s (plist-get occ :start)))
        (should (= (nth 3 s) 15))
        (should (= (nth 4 s) 45))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--expand-monthly-boundary-count-limits-occurrences ()
  "Test that COUNT limits the total number of occurrences."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Limited"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1 :count 4))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (should (= (length occurrences) 4))))

(ert-deftest test-calendar-sync--expand-monthly-boundary-until-limits-occurrences ()
  "Test that UNTIL date stops expansion."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (until-date (test-calendar-sync-time-date-only 120))
         (base-event (list :summary "Until-Limited"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1 :until until-date))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    ;; ~4 months of monthly occurrences
    (should (>= (length occurrences) 3))
    (should (<= (length occurrences) 5))))

(ert-deftest test-calendar-sync--expand-monthly-boundary-count-one-returns-single ()
  "Test that COUNT=1 returns exactly one occurrence."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Once"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1 :count 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (should (= (length occurrences) 1))))

(ert-deftest test-calendar-sync--expand-monthly-boundary-respects-date-range ()
  "Test that occurrences outside date range are excluded."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Ranged"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1))
         (range (test-calendar-sync-narrow-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range))
         (range-start (nth 0 range))
         (range-end (nth 1 range)))
    (dolist (occ occurrences)
      (let* ((s (plist-get occ :start))
             (occ-time (test-calendar-sync-date-to-time-value s)))
        (should (time-less-p range-start occ-time))
        (should (time-less-p occ-time range-end))))))

(ert-deftest test-calendar-sync--expand-monthly-boundary-preserves-summary ()
  "Test that each occurrence carries the original summary."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Team Sync"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1 :count 3))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (dolist (occ occurrences)
      (should (equal (plist-get occ :summary) "Team Sync")))))

;;; Error Cases

(ert-deftest test-calendar-sync--expand-monthly-error-past-until-returns-empty ()
  "Test that UNTIL in the past produces no occurrences in future range."
  (let* ((start-date (test-calendar-sync-time-days-ago 200 10 0))
         (end-date (test-calendar-sync-time-days-ago 200 11 0))
         (until-date (test-calendar-sync-time-date-only-ago 100))
         (base-event (list :summary "Past"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'monthly :interval 1 :until until-date))
         (range (list (time-subtract (current-time) (* 30 86400))
                      (time-add (current-time) (* 365 86400))))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (should (= (length occurrences) 0))))

(ert-deftest test-calendar-sync--expand-monthly-error-no-end-time-still-works ()
  "Test that event without :end still generates occurrences."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (base-event (list :summary "No End" :start start-date))
         (rrule (list :freq 'monthly :interval 1 :count 3))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (should (= (length occurrences) 3))))

(provide 'test-calendar-sync--expand-monthly)
;;; test-calendar-sync--expand-monthly.el ends here
