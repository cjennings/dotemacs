;;; test-calendar-sync--date-to-time.el --- Tests for date-to-time and before-date-p  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--date-to-time and calendar-sync--before-date-p.
;; Covers the fix for variable-length date lists (3, 5, 6 elements)
;; caused by UTC UNTIL values parsed via parse-timestamp returning
;; 5-element lists that broke the original (reverse date) implementation.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; calendar-sync--date-to-time — Normal Cases

(ert-deftest test-calendar-sync--date-to-time-normal-3-element-matches-encode-time ()
  "3-element (year month day) list matches explicit encode-time."
  (let* ((date (test-calendar-sync-time-date-only 30))
         (result (calendar-sync--date-to-time date))
         (expected (encode-time 0 0 0 (nth 2 date) (nth 1 date) (nth 0 date))))
    (should (equal result expected))))

(ert-deftest test-calendar-sync--date-to-time-normal-5-element-matches-3-element ()
  "5-element (year month day hour minute) produces same time as 3-element.
This is the bug case: UTC UNTIL values from parse-timestamp are 5 elements."
  (let* ((date-3 (test-calendar-sync-time-date-only 30))
         (date-5 (append date-3 '(14 59)))
         (time-3 (calendar-sync--date-to-time date-3))
         (time-5 (calendar-sync--date-to-time date-5)))
    (should (equal time-3 time-5))))

;;; calendar-sync--date-to-time — Boundary Cases

(ert-deftest test-calendar-sync--date-to-time-boundary-6-element-ignores-extras ()
  "6-element (year month day hour minute second) ignores hour/min/sec."
  (let* ((date-3 (test-calendar-sync-time-date-only 30))
         (date-6 (append date-3 '(23 59 59)))
         (time-3 (calendar-sync--date-to-time date-3))
         (time-6 (calendar-sync--date-to-time date-6)))
    (should (equal time-3 time-6))))

(ert-deftest test-calendar-sync--date-to-time-boundary-extra-elements-do-not-affect-result ()
  "Lists differing only in extra elements produce equal time values."
  (let* ((date-3 (test-calendar-sync-time-date-only 60))
         (date-5a (append date-3 '(0 0)))
         (date-5b (append date-3 '(23 59))))
    (should (equal (calendar-sync--date-to-time date-5a)
                   (calendar-sync--date-to-time date-5b)))))

;;; calendar-sync--before-date-p — Normal Cases

(ert-deftest test-calendar-sync--before-date-p-normal-3-element-before-5-element ()
  "3-element date before later 5-element date returns t.
This is the exact comparison that failed before the fix."
  (let* ((earlier (test-calendar-sync-time-date-only 10))
         (later-5 (append (test-calendar-sync-time-date-only 20) '(19 59))))
    (should (calendar-sync--before-date-p earlier later-5))))

(ert-deftest test-calendar-sync--before-date-p-normal-5-element-before-3-element ()
  "5-element date before later 3-element date returns t."
  (let* ((earlier-5 (append (test-calendar-sync-time-date-only 10) '(14 0)))
         (later (test-calendar-sync-time-date-only 20)))
    (should (calendar-sync--before-date-p earlier-5 later))))

;;; calendar-sync--before-date-p — Boundary Cases

(ert-deftest test-calendar-sync--before-date-p-boundary-same-date-mixed-lengths ()
  "Same date as 3-element and 5-element returns nil (not before)."
  (let* ((date-3 (test-calendar-sync-time-date-only 30))
         (date-5 (append date-3 '(19 59))))
    (should-not (calendar-sync--before-date-p date-3 date-5))
    (should-not (calendar-sync--before-date-p date-5 date-3))))

;;; calendar-sync--expand-weekly with 5-element UNTIL — Normal Cases

(ert-deftest test-calendar-sync--expand-weekly-normal-5-element-until-produces-occurrences ()
  "Weekly event with 5-element UNTIL (from UTC parsing) produces correct count."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         ;; 5-element UNTIL simulating parse-timestamp on UTC "UNTIL=...Z"
         (until-5 (append (test-calendar-sync-time-date-only 60) '(7 59)))
         (base-event (list :summary "Weekly Sync"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'weekly
                      :byday '("MO")
                      :interval 1
                      :until until-5))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-weekly base-event rrule range)))
    ;; ~8 Mondays in 60 days
    (should (> (length occurrences) 5))
    (should (< (length occurrences) 12))))

;;; calendar-sync--expand-weekly with 5-element UNTIL — Boundary Cases

(ert-deftest test-calendar-sync--expand-weekly-boundary-single-week-5-element-until ()
  "Single-week series with 5-element UNTIL produces exactly 1 occurrence.
Mirrors the real DeepSat Mgmt Sync case: RRULE with UNTIL ~6 days after start."
  (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
         (end-date (test-calendar-sync-time-days-from-now 1 11 0))
         ;; UNTIL is 6 days after start (same week, before next occurrence)
         (until-5 (append (test-calendar-sync-time-date-only 7) '(19 59)))
         (start-weekday (calendar-sync--date-weekday
                         (list (nth 0 start-date) (nth 1 start-date) (nth 2 start-date))))
         (weekday-str (nth start-weekday '("SU" "MO" "TU" "WE" "TH" "FR" "SA")))
         (base-event (list :summary "Short-Lived Series"
                           :start start-date
                           :end end-date))
         (rrule (list :freq 'weekly
                      :byday (list weekday-str)
                      :interval 1
                      :until until-5))
         (range (test-calendar-sync-wide-range))
         (occurrences (calendar-sync--expand-weekly base-event rrule range)))
    (should (= (length occurrences) 1))
    (should (equal (plist-get (car occurrences) :start) start-date))))

(provide 'test-calendar-sync--date-to-time)
;;; test-calendar-sync--date-to-time.el ends here
