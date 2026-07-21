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

;;; BYDAY (nth weekday) Cases
;;
;; Fixed dates are deterministic here: the expansion range is an explicit
;; parameter, not derived from the current time, so these never age out.

(defun test-calendar-sync--expand-monthly-range-2026 ()
  "Fixed expansion range covering calendar year 2026."
  (list (encode-time 0 0 0 1 1 2026) (encode-time 0 0 0 31 12 2026)))

(ert-deftest test-calendar-sync--expand-monthly-byday-second-wednesday ()
  "Normal: BYDAY=2WE lands on the 2nd Wednesday of each month, not the
day-of-month of DTSTART. This is the live Craig/Ryan series shape."
  (let* ((base-event (list :summary "2nd Wednesday"
                           :start '(2026 1 14 10 0)
                           :end '(2026 1 14 11 0)))
         (rrule (list :freq 'monthly :interval 1 :byday '("2WE")))
         (range (test-calendar-sync--expand-monthly-range-2026))
         (occurrences (calendar-sync--expand-monthly base-event rrule range))
         (days (mapcar (lambda (occ)
                         (let ((s (plist-get occ :start)))
                           (list (nth 1 s) (nth 2 s))))
                       occurrences)))
    (should (equal days '((1 14) (2 11) (3 11) (4 8) (5 13) (6 10)
                          (7 8) (8 12) (9 9) (10 14) (11 11) (12 9))))
    ;; Every occurrence is a Wednesday (weekday 3), never a fixed day-of-month.
    (dolist (occ occurrences)
      (let ((s (plist-get occ :start)))
        (should (= 3 (calendar-sync--date-weekday
                      (list (nth 0 s) (nth 1 s) (nth 2 s)))))))))

(ert-deftest test-calendar-sync--expand-monthly-byday-last-tuesday ()
  "Normal: BYDAY=-1TU lands on the last Tuesday of each month."
  (let* ((base-event (list :summary "Last Tuesday"
                           :start '(2026 1 27 9 0)
                           :end '(2026 1 27 10 0)))
         (rrule (list :freq 'monthly :interval 1 :byday '("-1TU") :count 3))
         (range (test-calendar-sync--expand-monthly-range-2026))
         (occurrences (calendar-sync--expand-monthly base-event rrule range))
         (days (mapcar (lambda (occ)
                         (let ((s (plist-get occ :start)))
                           (list (nth 1 s) (nth 2 s))))
                       occurrences)))
    (should (equal days '((1 27) (2 24) (3 31))))))

(ert-deftest test-calendar-sync--expand-monthly-byday-bysetpos-second-sunday ()
  "Normal: BYDAY=SU with BYSETPOS=2 lands on the 2nd Sunday (Proton shape)."
  (let* ((base-event (list :summary "2nd Sunday"
                           :start '(2026 1 11 8 0)
                           :end '(2026 1 11 9 0)))
         (rrule (list :freq 'monthly :interval 1 :byday '("SU") :bysetpos 2 :count 3))
         (range (test-calendar-sync--expand-monthly-range-2026))
         (occurrences (calendar-sync--expand-monthly base-event rrule range))
         (days (mapcar (lambda (occ)
                         (let ((s (plist-get occ :start)))
                           (list (nth 1 s) (nth 2 s))))
                       occurrences)))
    (should (equal days '((1 11) (2 8) (3 8))))))

(ert-deftest test-calendar-sync--expand-monthly-byday-until-inclusive-and-reached ()
  "Boundary: with BYDAY, UNTIL is inclusive and the series reaches it.
Upper bound alone can't catch a dropped final occurrence -- assert both."
  (let* ((base-event (list :summary "Bounded"
                           :start '(2026 1 14 10 0)
                           :end '(2026 1 14 11 0)))
         (rrule (list :freq 'monthly :interval 1 :byday '("2WE")
                      :until '(2026 5 13)))
         (range (test-calendar-sync--expand-monthly-range-2026))
         (occurrences (calendar-sync--expand-monthly base-event rrule range))
         (last-start (plist-get (car (last occurrences)) :start)))
    (should (= (length occurrences) 5))
    ;; Reach: the occurrence landing exactly on UNTIL is kept.
    (should (equal (list (nth 0 last-start) (nth 1 last-start) (nth 2 last-start))
                   '(2026 5 13)))))

(ert-deftest test-calendar-sync--expand-monthly-byday-count-limits ()
  "Boundary: COUNT caps a BYDAY series."
  (let* ((base-event (list :summary "Counted"
                           :start '(2026 1 14 10 0)
                           :end '(2026 1 14 11 0)))
         (rrule (list :freq 'monthly :interval 1 :byday '("2WE") :count 4))
         (range (test-calendar-sync--expand-monthly-range-2026))
         (occurrences (calendar-sync--expand-monthly base-event rrule range)))
    (should (= (length occurrences) 4))))

(ert-deftest test-calendar-sync--expand-monthly-byday-fifth-weekday-skips-short-months ()
  "Boundary: BYDAY=5WE only lands in months that have a 5th Wednesday."
  (let* ((base-event (list :summary "5th Wednesday"
                           :start '(2026 4 29 10 0)
                           :end '(2026 4 29 11 0)))
         (rrule (list :freq 'monthly :interval 1 :byday '("5WE")))
         (range (test-calendar-sync--expand-monthly-range-2026))
         (occurrences (calendar-sync--expand-monthly base-event rrule range))
         (days (mapcar (lambda (occ)
                         (let ((s (plist-get occ :start)))
                           (list (nth 1 s) (nth 2 s))))
                       occurrences)))
    ;; 2026 months (Apr on) with a 5th Wednesday: Apr 29, Jul 29, Sep 30, Dec 30.
    (should (equal days '((4 29) (7 29) (9 30) (12 30))))))

(ert-deftest test-calendar-sync--expand-monthly-on-31st-skips-short-months ()
  "Boundary: a plain monthly rule on the 31st skips months without a 31st.
The stepper kept day-of-month verbatim, so Jan 31 stepped to Feb 31,
which encode-time normalizes to Mar 3 -- phantom mis-dated occurrences
instead of the RFC 5545 skip."
  (let* ((base-event (list :summary "Monthly on the 31st"
                           :start '(2030 1 31 10 0)
                           :end '(2030 1 31 11 0)))
         (rrule (list :freq 'monthly :interval 1))
         ;; End the range past Dec 31: the range end is midnight, so ending
         ;; ON the 31st would exclude that day's 10:00 occurrence.
         (range (list (calendar-sync--date-to-time '(2030 1 1))
                      (calendar-sync--date-to-time '(2031 1 1))))
         (occurrences (calendar-sync--expand-monthly base-event rrule range))
         (months (mapcar (lambda (o) (nth 1 (plist-get o :start))) occurrences))
         (days (mapcar (lambda (o) (nth 2 (plist-get o :start))) occurrences)))
    ;; Only the seven 31-day months of 2030, each on the 31st.
    (should (equal months '(1 3 5 7 8 10 12)))
    (should (equal days '(31 31 31 31 31 31 31)))))

(provide 'test-calendar-sync--expand-monthly)
;;; test-calendar-sync--expand-monthly.el ends here
