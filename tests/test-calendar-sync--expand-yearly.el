;;; test-calendar-sync--expand-yearly.el --- Tests for calendar-sync--expand-yearly  -*- lexical-binding: t; -*-

;;; Commentary:
;; Characterization tests for calendar-sync--expand-yearly.
;; Captures current behavior before refactoring into unified expand function.
;; Uses dynamic timestamps to avoid hardcoded dates.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--expand-yearly-normal-generates-occurrences ()
  "Test expanding yearly event generates occurrences within range."
  (let* ((start-date (test-calendar-sync-time-days-ago 800 10 0))
         (end-date (test-calendar-sync-time-days-ago 800 11 0))
         (base-event (list :summary "Birthday"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    ;; Range covers ~1.25 years, event started ~2.2 years ago
    ;; Should see 1-2 occurrences in range
    (should (>= (length occurrences) 1))
    (should (<= (length occurrences) 3))))

(ert-deftest test-calendar-sync--expand-yearly-normal-preserves-date ()
  "Test that each occurrence falls on the same month and day."
  (let* ((start-date (test-calendar-sync-time-days-ago 1100 10 0))
         (end-date (test-calendar-sync-time-days-ago 1100 11 0))
         (expected-month (nth 1 start-date))
         (expected-day (nth 2 start-date))
         (base-event (list :summary "Anniversary"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    (should (> (length occurrences) 0))
    (dolist (occ occurrences)
      (let ((s (plist-get occ :start)))
        (should (= (nth 1 s) expected-month))
        (should (= (nth 2 s) expected-day))))))

(ert-deftest test-calendar-sync--expand-yearly-normal-preserves-time ()
  "Test that each occurrence preserves the original event time."
  (let* ((start-date (test-calendar-sync-time-days-ago 400 15 30))
         (end-date (test-calendar-sync-time-days-ago 400 16 30))
         (base-event (list :summary "Yearly"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1 :count 5))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    (dolist (occ occurrences)
      (let ((s (plist-get occ :start)))
        (should (= (nth 3 s) 15))
        (should (= (nth 4 s) 30))))))

(ert-deftest test-calendar-sync--expand-yearly-normal-interval-two ()
  "Test expanding bi-yearly event."
  (let* ((start-date (test-calendar-sync-time-days-ago 2000 10 0))
         (end-date (test-calendar-sync-time-days-ago 2000 11 0))
         (base-event (list :summary "Bi-Yearly"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 2))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    ;; Over ~5.5 years, bi-yearly = ~2-3 total, 0-1 in range
    (should (<= (length occurrences) 2))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--expand-yearly-boundary-count-limits-occurrences ()
  "Test that COUNT limits the total number of occurrences."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Limited"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1 :count 3))
         ;; Use a very wide range to capture all 3 years
         (range (list (time-subtract (current-time) (* 90 86400))
                      (time-add (current-time) (* 1460 86400))))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    (should (= (length occurrences) 3))))

(ert-deftest test-calendar-sync--expand-yearly-boundary-until-limits-occurrences ()
  "Test that UNTIL date stops expansion."
  (let* ((start-date (test-calendar-sync-time-days-ago 1000 10 0))
         (end-date (test-calendar-sync-time-days-ago 1000 11 0))
         (until-date (test-calendar-sync-time-date-only 365))
         (base-event (list :summary "Until-Limited"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1 :until until-date))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    ;; Should have occurrences within range but stopped by UNTIL
    (should (>= (length occurrences) 1))
    (should (<= (length occurrences) 3))))

(ert-deftest test-calendar-sync--expand-yearly-boundary-count-one-returns-single ()
  "Test that COUNT=1 returns exactly one occurrence."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Once"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1 :count 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    (should (= (length occurrences) 1))))

(ert-deftest test-calendar-sync--expand-yearly-boundary-respects-date-range ()
  "Test that occurrences outside date range are excluded."
  (let* ((start-date (test-calendar-sync-time-days-ago 1500 10 0))
         (end-date (test-calendar-sync-time-days-ago 1500 11 0))
         (base-event (list :summary "Ranged"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1))
         (range (test-calendar-sync-narrow-range))
         (occurrences (calendar-sync--expand-yearly base-event rrule range))
         (range-start (nth 0 range))
         (range-end (nth 1 range)))
    (dolist (occ occurrences)
      (let* ((s (plist-get occ :start))
             (occ-time (test-calendar-sync-date-to-time-value s)))
        (should (time-less-p range-start occ-time))
        (should (time-less-p occ-time range-end))))))

(ert-deftest test-calendar-sync--expand-yearly-boundary-preserves-summary ()
  "Test that each occurrence carries the original summary."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Annual Gala"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1 :count 2))
         (range (list (time-subtract (current-time) (* 90 86400))
                      (time-add (current-time) (* 1460 86400))))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    (dolist (occ occurrences)
      (should (equal (plist-get occ :summary) "Annual Gala")))))

;;; Error Cases

(ert-deftest test-calendar-sync--expand-yearly-error-past-until-returns-empty ()
  "Test that UNTIL in the past produces no occurrences in future range."
  (let* ((start-date (test-calendar-sync-time-days-ago 1500 10 0))
         (end-date (test-calendar-sync-time-days-ago 1500 11 0))
         (until-date (test-calendar-sync-time-date-only-ago 365))
         (base-event (list :summary "Past"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'yearly :interval 1 :until until-date))
         (range (list (time-subtract (current-time) (* 30 86400))
                      (time-add (current-time) (* 365 86400))))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    (should (= (length occurrences) 0))))

(ert-deftest test-calendar-sync--expand-yearly-error-no-end-time-still-works ()
  "Test that event without :end still generates occurrences."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (base-event (list :summary "No End" :start start-date))
         (rrule (list :freq 'yearly :interval 1 :count 2))
         (range (list (time-subtract (current-time) (* 90 86400))
                      (time-add (current-time) (* 1460 86400))))
         (occurrences (calendar-sync--expand-yearly base-event rrule range)))
    (should (= (length occurrences) 2))))

(provide 'test-calendar-sync--expand-yearly)
;;; test-calendar-sync--expand-yearly.el ends here
