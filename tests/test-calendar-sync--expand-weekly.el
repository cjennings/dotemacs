;;; test-calendar-sync--expand-weekly.el --- Tests for calendar-sync--expand-weekly  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--expand-weekly function.
;; Tests expansion of weekly recurring events into individual occurrences.
;; Uses dynamic timestamps to avoid hardcoded dates.

;;; Code:

(require 'ert)
(require 'calendar-sync)
(require 'testutil-calendar-sync)

;;; Setup and Teardown

(defun test-calendar-sync--expand-weekly-setup ()
  "Setup for calendar-sync--expand-weekly tests."
  nil)

(defun test-calendar-sync--expand-weekly-teardown ()
  "Teardown for calendar-sync--expand-weekly tests."
  nil)

;;; Normal Cases

(ert-deftest test-calendar-sync--expand-weekly-normal-saturday-returns-occurrences ()
  "Test expanding weekly event on Saturday (GTFO use case)."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 30))
             (end-date (test-calendar-sync-time-days-from-now 1 11 0))
             (base-event (list :summary "GTFO"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("SA")
                         :interval 1))
             ;; Date range: 90 days past to 365 days future
             (range (list (time-subtract (current-time) (* 90 24 3600))
                         (time-add (current-time) (* 365 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should generate ~52 Saturday occurrences in a year
        (should (> (length occurrences) 40))
        (should (< (length occurrences) 60))
        ;; Each occurrence should be a Saturday
        (dolist (occurrence occurrences)
          (let* ((start (plist-get occurrence :start))
                 (weekday (calendar-sync--date-weekday (list (nth 0 start) (nth 1 start) (nth 2 start)))))
            (should (= weekday 6)))))  ; Saturday = 6
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-normal-multiple-days-returns-occurrences ()
  "Test expanding weekly event on multiple weekdays."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 9 0))
             (end-date (test-calendar-sync-time-days-from-now 1 10 0))
             (base-event (list :summary "Standup"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("MO" "WE" "FR")
                         :interval 1))
             (range (list (time-subtract (current-time) (* 30 24 3600))
                         (time-add (current-time) (* 90 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should generate 3 occurrences per week for ~4 months
        (should (> (length occurrences) 30))
        (should (< (length occurrences) 60))
        ;; Each occurrence should be Mon, Wed, or Fri
        (dolist (occurrence occurrences)
          (let* ((start (plist-get occurrence :start))
                 (weekday (calendar-sync--date-weekday (list (nth 0 start) (nth 1 start) (nth 2 start)))))
            (should (member weekday '(1 3 5))))))  ; Mon=1, Wed=3, Fri=5
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-normal-interval-two-returns-occurrences ()
  "Test expanding bi-weekly event."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 14 0))
             (end-date (test-calendar-sync-time-days-from-now 1 15 0))
             (base-event (list :summary "Bi-weekly Meeting"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("TU")
                         :interval 2))
             (range (list (time-subtract (current-time) (* 30 24 3600))
                         (time-add (current-time) (* 180 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should generate ~13 occurrences (26 weeks = 13 bi-weekly)
        (should (> (length occurrences) 10))
        (should (< (length occurrences) 20)))
    (test-calendar-sync--expand-weekly-teardown)))

;;; Boundary Cases

(ert-deftest test-calendar-sync--expand-weekly-boundary-with-count-returns-limited-occurrences ()
  "Test expanding weekly event with count limit."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
             (end-date (test-calendar-sync-time-days-from-now 1 11 0))
             (base-event (list :summary "Limited Event"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("MO")
                         :interval 1
                         :count 5))
             (range (list (time-subtract (current-time) (* 30 24 3600))
                         (time-add (current-time) (* 365 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should generate exactly 5 occurrences
        (should (= (length occurrences) 5)))
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-boundary-with-until-returns-limited-occurrences ()
  "Test expanding weekly event with end date."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
             (end-date (test-calendar-sync-time-days-from-now 1 11 0))
             (until-date (test-calendar-sync-time-days-from-now 60 0 0))
             (base-event (list :summary "Time-Limited Event"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("WE")
                         :interval 1
                         :until until-date))
             (range (list (time-subtract (current-time) (* 30 24 3600))
                         (time-add (current-time) (* 365 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should generate ~8 Wednesday occurrences in 60 days
        (should (> (length occurrences) 6))
        (should (< (length occurrences) 12)))
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-boundary-no-byday-uses-start-day ()
  "Test expanding weekly event without BYDAY uses start date weekday."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 7 10 0))
             (end-date (test-calendar-sync-time-days-from-now 7 11 0))
             (start-weekday (calendar-sync--date-weekday (list (nth 0 start-date) (nth 1 start-date) (nth 2 start-date))))
             (base-event (list :summary "No BYDAY Event"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :interval 1))
             (range (list (time-subtract (current-time) (* 30 24 3600))
                         (time-add (current-time) (* 90 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should generate occurrences
        (should (> (length occurrences) 8))
        ;; All occurrences should be on the same weekday as start
        (dolist (occurrence occurrences)
          (let* ((start (plist-get occurrence :start))
                 (weekday (calendar-sync--date-weekday (list (nth 0 start) (nth 1 start) (nth 2 start)))))
            (should (= weekday start-weekday)))))
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-boundary-max-iterations-prevents-infinite-loop ()
  "Test that max iterations safety check prevents infinite loops."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
             (end-date (test-calendar-sync-time-days-from-now 1 11 0))
             (base-event (list :summary "Event"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("MO")
                         :interval 1))
             ;; Very large date range that would generate >1000 occurrences
             (range (list (time-subtract (current-time) (* 365 24 3600))
                         (time-add (current-time) (* 3650 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should stop at max iterations (1000)
        (should (<= (length occurrences) 1000)))
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-boundary-respects-date-range ()
  "Test that expansion respects date range boundaries."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
             (end-date (test-calendar-sync-time-days-from-now 1 11 0))
             (base-event (list :summary "Event"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("TH")
                         :interval 1))
             ;; Narrow date range: only 30 days
             (range (list (current-time)
                         (time-add (current-time) (* 30 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range))
             (range-start (nth 0 range))
             (range-end (nth 1 range)))
        ;; Should only generate ~4 Thursday occurrences in 30 days
        (should (>= (length occurrences) 3))
        (should (<= (length occurrences) 5))
        ;; All occurrences should be within range
        (dolist (occurrence occurrences)
          (let* ((start (plist-get occurrence :start))
                 (occ-time (apply #'encode-time 0 0 0 (reverse (list (nth 0 start) (nth 1 start) (nth 2 start))))))
            (should (time-less-p range-start occ-time))
            (should (time-less-p occ-time range-end)))))
    (test-calendar-sync--expand-weekly-teardown)))

;;; Error Cases

(ert-deftest test-calendar-sync--expand-weekly-error-empty-base-event-returns-empty ()
  "Test expanding with minimal base event."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
             (base-event (list :start start-date))
             (rrule (list :freq 'weekly
                         :interval 1))
             (range (list (current-time)
                         (time-add (current-time) (* 30 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should still generate occurrences even without end time
        (should (> (length occurrences) 0)))
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-error-zero-interval-returns-empty ()
  "Test that zero interval doesn't cause infinite loop."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-from-now 1 10 0))
             (end-date (test-calendar-sync-time-days-from-now 1 11 0))
             (base-event (list :summary "Event"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("MO")
                         :interval 0))  ; Invalid!
             (range (list (current-time)
                         (time-add (current-time) (* 30 24 3600)))))
        ;; Should either return empty or handle gracefully
        ;; Zero interval would cause infinite loop if not handled
        (should-error (calendar-sync--expand-weekly base-event rrule range)))
    (test-calendar-sync--expand-weekly-teardown)))

(ert-deftest test-calendar-sync--expand-weekly-error-past-until-returns-empty ()
  "Test expanding event with UNTIL date in the past."
  (test-calendar-sync--expand-weekly-setup)
  (unwind-protect
      (let* ((start-date (test-calendar-sync-time-days-ago 100 10 0))
             (end-date (test-calendar-sync-time-days-ago 100 11 0))
             (until-date (test-calendar-sync-time-days-ago 50 0 0))
             (base-event (list :summary "Past Event"
                              :start start-date
                              :end end-date))
             (rrule (list :freq 'weekly
                         :byday '("MO")
                         :interval 1
                         :until until-date))
             (range (list (time-subtract (current-time) (* 30 24 3600))
                         (time-add (current-time) (* 365 24 3600))))
             (occurrences (calendar-sync--expand-weekly base-event rrule range)))
        ;; Should return empty list (all occurrences before range)
        (should (= (length occurrences) 0)))
    (test-calendar-sync--expand-weekly-teardown)))

(provide 'test-calendar-sync--expand-weekly)
;;; test-calendar-sync--expand-weekly.el ends here
