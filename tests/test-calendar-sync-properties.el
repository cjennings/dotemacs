;;; test-calendar-sync-properties.el --- Property-based tests for calendar-sync  -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for RRULE expansion functions.
;; These tests verify invariants hold across randomly generated inputs,
;; complementing the example-based tests in other test files.
;;
;; Each test runs multiple trials with random parameters to explore
;; the input space and find edge cases that example-based tests miss.
;;
;; Properties tested:
;; 1. COUNT always limits total occurrences
;; 2. UNTIL date bounds all occurrences
;; 3. BYDAY constrains weekly occurrences to specified weekdays
;; 4. INTERVAL creates correct spacing between occurrences
;; 5. All occurrences fall within the date range
;; 6. Expansion is deterministic (same inputs → same outputs)

;;; Code:

(require 'ert)
(require 'calendar-sync)
(require 'testutil-calendar-sync)

(defconst test-calendar-sync-property-trials 30
  "Number of random trials to run for each property test.
Higher values give more confidence but slower tests.")

;;; Property 1: COUNT Ceiling

(ert-deftest test-calendar-sync-property-count-limits-daily ()
  "Property: COUNT parameter limits daily occurrences.
For any COUNT value N, expansion never produces more than N occurrences."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((count (1+ (random 20)))
           (start-date (test-calendar-sync-random-future-date))
           (base-event (list :summary "Daily Test" :start start-date))
           (rrule (list :freq 'daily :interval 1 :count count))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-daily base-event rrule range)))
      (should (<= (length occurrences) count)))))

(ert-deftest test-calendar-sync-property-count-limits-weekly ()
  "Property: COUNT parameter limits weekly occurrences.
For any COUNT value N, expansion never produces more than N occurrences."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((count (1+ (random 20)))
           (weekdays (test-calendar-sync-random-weekday-subset))
           (start-date (test-calendar-sync-random-future-date))
           (base-event (list :summary "Weekly Test" :start start-date))
           (rrule (list :freq 'weekly :byday weekdays :interval 1 :count count))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-weekly base-event rrule range)))
      (should (<= (length occurrences) count)))))

(ert-deftest test-calendar-sync-property-count-limits-monthly ()
  "Property: COUNT parameter limits monthly occurrences."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((count (1+ (random 15)))
           (start-date (test-calendar-sync-random-future-date))
           (base-event (list :summary "Monthly Test" :start start-date))
           (rrule (list :freq 'monthly :interval 1 :count count))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-monthly base-event rrule range)))
      (should (<= (length occurrences) count)))))

(ert-deftest test-calendar-sync-property-count-limits-yearly ()
  "Property: COUNT parameter limits yearly occurrences."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((count (1+ (random 5)))
           (start-date (test-calendar-sync-random-future-date))
           (base-event (list :summary "Yearly Test" :start start-date))
           (rrule (list :freq 'yearly :interval 1 :count count))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-yearly base-event rrule range)))
      (should (<= (length occurrences) count)))))

;;; Property 2: UNTIL Boundary

(ert-deftest test-calendar-sync-property-until-bounds-daily ()
  "Property: No daily occurrence starts on or after UNTIL date."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
           (until-days (+ 10 (random 60)))
           ;; UNTIL must be date-only (3 elements) for calendar-sync--before-date-p
           (until-date (test-calendar-sync-time-date-only until-days))
           (base-event (list :summary "Until Test" :start start-date))
           (rrule (list :freq 'daily :interval 1 :until until-date))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-daily base-event rrule range)))
      (dolist (occ occurrences)
        (let ((occ-start (plist-get occ :start)))
          (should (calendar-sync--before-date-p
                   (list (nth 0 occ-start) (nth 1 occ-start) (nth 2 occ-start))
                   until-date)))))))

(ert-deftest test-calendar-sync-property-until-bounds-weekly ()
  "Property: No weekly occurrence starts on or after UNTIL date."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
           (until-days (+ 14 (random 60)))
           ;; UNTIL must be date-only (3 elements) for calendar-sync--before-date-p
           (until-date (test-calendar-sync-time-date-only until-days))
           (weekdays (test-calendar-sync-random-weekday-subset))
           (base-event (list :summary "Until Test" :start start-date))
           (rrule (list :freq 'weekly :byday weekdays :interval 1 :until until-date))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-weekly base-event rrule range)))
      (dolist (occ occurrences)
        (let ((occ-start (plist-get occ :start)))
          (should (calendar-sync--before-date-p
                   (list (nth 0 occ-start) (nth 1 occ-start) (nth 2 occ-start))
                   until-date)))))))

;;; Property 3: BYDAY Constraint

(ert-deftest test-calendar-sync-property-byday-constrains-weekdays ()
  "Property: Weekly occurrences only fall on BYDAY weekdays.
Every generated occurrence must be on one of the specified weekdays."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((weekdays (test-calendar-sync-random-weekday-subset))
           (weekday-nums (mapcar #'calendar-sync--weekday-to-number weekdays))
           (start-date (test-calendar-sync-random-future-date))
           (base-event (list :summary "BYDAY Test" :start start-date))
           (rrule (list :freq 'weekly :byday weekdays :interval 1))
           (range (test-calendar-sync-narrow-range))
           (occurrences (calendar-sync--expand-weekly base-event rrule range)))
      (dolist (occ occurrences)
        (let* ((occ-start (plist-get occ :start))
               (occ-weekday (calendar-sync--date-weekday
                             (list (nth 0 occ-start) (nth 1 occ-start) (nth 2 occ-start)))))
          (should (member occ-weekday weekday-nums)))))))

;;; Property 4: INTERVAL Spacing

(ert-deftest test-calendar-sync-property-interval-spacing-daily ()
  "Property: Daily occurrences are spaced INTERVAL days apart.
Consecutive occurrences should be exactly INTERVAL days apart."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((interval (1+ (random 5)))
           (start-date (test-calendar-sync-time-days-from-now 1 10 0))
           (base-event (list :summary "Interval Test" :start start-date))
           (rrule (list :freq 'daily :interval interval :count 10))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-daily base-event rrule range)))
      (when (> (length occurrences) 1)
        (let ((dates (mapcar (lambda (o) (plist-get o :start)) occurrences)))
          (cl-loop for i from 0 below (1- (length dates))
                   for d1 = (nth i dates)
                   for d2 = (nth (1+ i) dates)
                   do (let ((gap (round (test-calendar-sync-days-between d1 d2))))
                        (should (= interval gap)))))))))

(ert-deftest test-calendar-sync-property-interval-spacing-weekly-single-day ()
  "Property: Weekly single-day occurrences are spaced INTERVAL weeks apart."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((interval (1+ (random 3)))
           (weekday (nth (random 7) '("MO" "TU" "WE" "TH" "FR" "SA" "SU")))
           (start-date (test-calendar-sync-time-days-from-now 1 10 0))
           (base-event (list :summary "Weekly Interval Test" :start start-date))
           (rrule (list :freq 'weekly :byday (list weekday) :interval interval :count 8))
           (range (test-calendar-sync-wide-range))
           (occurrences (calendar-sync--expand-weekly base-event rrule range)))
      (when (> (length occurrences) 1)
        (let ((dates (mapcar (lambda (o) (plist-get o :start)) occurrences)))
          (cl-loop for i from 0 below (1- (length dates))
                   for d1 = (nth i dates)
                   for d2 = (nth (1+ i) dates)
                   do (let ((gap (round (test-calendar-sync-days-between d1 d2))))
                        (should (= (* 7 interval) gap)))))))))

;;; Property 5: Range Containment

(ert-deftest test-calendar-sync-property-occurrences-within-range ()
  "Property: All occurrences fall within the date range.
No occurrence should be before range start or after range end."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((range-start-days (random 30))
           (range-end-days (+ range-start-days 30 (random 60)))
           (range (list (time-add (current-time) (* range-start-days 86400))
                        (time-add (current-time) (* range-end-days 86400))))
           (start-date (test-calendar-sync-time-days-from-now (1+ range-start-days) 10 0))
           (base-event (list :summary "Range Test" :start start-date))
           (rrule (list :freq 'daily :interval 1))
           (occurrences (calendar-sync--expand-daily base-event rrule range)))
      (dolist (occ occurrences)
        (let ((occ-start (plist-get occ :start)))
          (should (calendar-sync--date-in-range-p occ-start range)))))))

(ert-deftest test-calendar-sync-property-weekly-occurrences-within-range ()
  "Property: All weekly occurrences fall within the date range."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((range (test-calendar-sync-narrow-range))
           (start-date (test-calendar-sync-time-days-from-now 1 10 0))
           (weekdays (test-calendar-sync-random-weekday-subset))
           (base-event (list :summary "Range Test" :start start-date))
           (rrule (list :freq 'weekly :byday weekdays :interval 1))
           (occurrences (calendar-sync--expand-weekly base-event rrule range)))
      (dolist (occ occurrences)
        (let ((occ-start (plist-get occ :start)))
          (should (calendar-sync--date-in-range-p occ-start range)))))))

;;; Property 6: Determinism

(ert-deftest test-calendar-sync-property-expansion-deterministic-daily ()
  "Property: Same inputs produce identical outputs for daily expansion."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((interval (1+ (random 3)))
           (count (+ 5 (random 10)))
           (start-date (test-calendar-sync-time-days-from-now 1 10 0))
           (base-event (list :summary "Determinism Test" :start start-date))
           (rrule (list :freq 'daily :interval interval :count count))
           (range (test-calendar-sync-wide-range))
           (result1 (calendar-sync--expand-daily base-event rrule range))
           (result2 (calendar-sync--expand-daily base-event rrule range)))
      (should (= (length result1) (length result2)))
      (cl-loop for o1 in result1
               for o2 in result2
               do (should (equal (plist-get o1 :start) (plist-get o2 :start)))))))

(ert-deftest test-calendar-sync-property-expansion-deterministic-weekly ()
  "Property: Same inputs produce identical outputs for weekly expansion."
  (dotimes (_ test-calendar-sync-property-trials)
    (let* ((interval (1+ (random 2)))
           (weekdays (test-calendar-sync-random-weekday-subset))
           (count (+ 5 (random 10)))
           (start-date (test-calendar-sync-time-days-from-now 1 10 0))
           (base-event (list :summary "Determinism Test" :start start-date))
           (rrule (list :freq 'weekly :byday weekdays :interval interval :count count))
           (range (test-calendar-sync-wide-range))
           (result1 (calendar-sync--expand-weekly base-event rrule range))
           (result2 (calendar-sync--expand-weekly base-event rrule range)))
      (should (= (length result1) (length result2)))
      (cl-loop for o1 in result1
               for o2 in result2
               do (should (equal (plist-get o1 :start) (plist-get o2 :start)))))))

(provide 'test-calendar-sync-properties)
;;; test-calendar-sync-properties.el ends here
