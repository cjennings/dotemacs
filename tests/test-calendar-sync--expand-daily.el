;;; test-calendar-sync--expand-daily.el --- Tests for calendar-sync--expand-daily  -*- lexical-binding: t; -*-

;;; Commentary:
;; Characterization tests for calendar-sync--expand-daily.
;; Captures current behavior before refactoring into unified expand function.
;; Uses dynamic timestamps to avoid hardcoded dates.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--expand-daily-normal-generates-occurrences ()
  "Test expanding daily event generates occurrences within range."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Daily Standup"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    ;; ~365 days future + ~90 days past = ~456 days of occurrences
    (should (> (length occurrences) 300))
    (should (< (length occurrences) 500))))

(ert-deftest test-calendar-sync--expand-daily-normal-preserves-time ()
  "Test that each occurrence preserves the original event time."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 14 30))
         (end-date (test-calendar-sync-time-days-from-now 1 15 30))
         (base-event (list :summary "Afternoon Check"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 1))
         (range (test-calendar-sync-narrow-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    (should (> (length occurrences) 0))
    (dolist (occ occurrences)
      (let ((s (plist-get occ :start)))
        (should (= (nth 3 s) 14))
        (should (= (nth 4 s) 30))))))

(ert-deftest test-calendar-sync--expand-daily-normal-interval-two ()
  "Test expanding every-other-day event."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 9 0))
         (end-date (test-calendar-sync-time-days-from-now 1 10 0))
         (base-event (list :summary "Every Other Day"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 2))
         (range (test-calendar-sync-narrow-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    ;; 30 days / 2 = ~15 occurrences
    (should (>= (length occurrences) 12))
    (should (<= (length occurrences) 18))))

(ert-deftest test-calendar-sync--expand-daily-normal-consecutive-dates-increase ()
  "Test that occurrences are on consecutive dates spaced by interval."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Daily"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 3))
         (range (test-calendar-sync-narrow-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    (when (> (length occurrences) 1)
      (let ((prev (plist-get (car occurrences) :start)))
        (dolist (occ (cdr occurrences))
          (let ((curr (plist-get occ :start)))
            ;; Days between consecutive occurrences should be ~3
            (let ((days (test-calendar-sync-days-between prev curr)))
              (should (>= days 2.5))
              (should (<= days 3.5)))
            (setq prev curr)))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--expand-daily-boundary-count-limits-occurrences ()
  "Test that COUNT limits the total number of occurrences."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Limited"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 1 :count 7))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    (should (= (length occurrences) 7))))

(ert-deftest test-calendar-sync--expand-daily-boundary-until-limits-occurrences ()
  "Test that UNTIL date stops expansion."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (until-date (test-calendar-sync-time-date-only 15))
         (base-event (list :summary "Until-Limited"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 1 :until until-date))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    ;; ~14 days worth of daily occurrences
    (should (>= (length occurrences) 12))
    (should (<= (length occurrences) 16))))

(ert-deftest test-calendar-sync--expand-daily-boundary-respects-date-range ()
  "Test that occurrences are within or at the date range boundaries."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Ranged"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 1))
         (range (test-calendar-sync-narrow-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range))
         (range-start (nth 0 range))
         ;; Add 1 day buffer — date-in-range-p checks date portion only
         (range-end-padded (time-add (nth 1 range) 86400)))
    (dolist (occ occurrences)
      (let* ((s (plist-get occ :start))
             (occ-time (test-calendar-sync-date-to-time-value s)))
        (should (time-less-p range-start occ-time))
        (should (time-less-p occ-time range-end-padded))))))

(ert-deftest test-calendar-sync--expand-daily-boundary-count-one-returns-single ()
  "Test that COUNT=1 returns exactly one occurrence."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "Once"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 1 :count 1))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    (should (= (length occurrences) 1))))

(ert-deftest test-calendar-sync--expand-daily-boundary-preserves-summary ()
  "Test that each occurrence carries the original summary."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         (base-event (list :summary "My Event"
                           :start start-date
                           :end end-date
                           :location "Room 5"))
         (rrule (list :freq 'daily :interval 1 :count 3))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    (dolist (occ occurrences)
      (should (equal (plist-get occ :summary) "My Event")))))

;;; Error Cases

(ert-deftest test-calendar-sync--expand-daily-error-past-until-returns-empty ()
  "Test that UNTIL in the past produces no occurrences in future range."
  (let* ((start-date (test-calendar-sync-time-days-ago 100 10 0))
         (end-date (test-calendar-sync-time-days-ago 100 11 0))
         (until-date (test-calendar-sync-time-date-only-ago 50))
         (base-event (list :summary "Past"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'daily :interval 1 :until until-date))
         (range (list (time-subtract (current-time) (* 30 86400))
                      (time-add (current-time) (* 365 86400))))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    (should (= (length occurrences) 0))))

(ert-deftest test-calendar-sync--expand-daily-error-no-end-time-still-works ()
  "Test that event without :end still generates occurrences."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (base-event (list :summary "No End" :start start-date))
         (rrule (list :freq 'daily :interval 1 :count 5))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-daily base-event rrule range)))
    (should (= (length occurrences) 5))))

(provide 'test-calendar-sync--expand-daily)
;;; test-calendar-sync--expand-daily.el ends here
