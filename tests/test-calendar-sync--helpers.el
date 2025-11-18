;;; test-calendar-sync--helpers.el --- Tests for calendar-sync helper functions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for refactored helper functions.
;; Tests the helper functions that simplify RRULE expansion logic.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Setup and Teardown

(defun test-calendar-sync--helpers-setup ()
  "Setup for helper function tests."
  nil)

(defun test-calendar-sync--helpers-teardown ()
  "Teardown for helper function tests."
  nil)

;;; calendar-sync--date-to-time Tests

(ert-deftest test-calendar-sync--date-to-time-converts-date-to-time ()
  "Test converting date to time value."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let* ((date '(2025 11 18))  ; Nov 18, 2025
             (time-val (calendar-sync--date-to-time date)))
        ;; Should return a valid time value
        (should (numberp (time-convert time-val 'integer))))
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--date-to-time-handles-different-dates ()
  "Test date-to-time with various dates."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let ((date1 '(2025 1 1))
            (date2 '(2025 12 31)))
        ;; Different dates should produce different time values
        (should (not (equal (calendar-sync--date-to-time date1)
                           (calendar-sync--date-to-time date2)))))
    (test-calendar-sync--helpers-teardown)))

;;; calendar-sync--before-date-p Tests

(ert-deftest test-calendar-sync--before-date-p-returns-true-for-earlier-date ()
  "Test that earlier dates return true."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let ((earlier '(2025 11 17))
            (later '(2025 11 18)))
        (should (calendar-sync--before-date-p earlier later)))
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--before-date-p-returns-false-for-later-date ()
  "Test that later dates return false."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let ((earlier '(2025 11 17))
            (later '(2025 11 18)))
        (should-not (calendar-sync--before-date-p later earlier)))
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--before-date-p-returns-false-for-same-date ()
  "Test that same dates return false."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let ((date '(2025 11 18)))
        (should-not (calendar-sync--before-date-p date date)))
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--before-date-p-handles-month-boundaries ()
  "Test date comparison across month boundaries."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let ((november '(2025 11 30))
            (december '(2025 12 1)))
        (should (calendar-sync--before-date-p november december))
        (should-not (calendar-sync--before-date-p december november)))
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--before-date-p-handles-year-boundaries ()
  "Test date comparison across year boundaries."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let ((dec-2025 '(2025 12 31))
            (jan-2026 '(2026 1 1)))
        (should (calendar-sync--before-date-p dec-2025 jan-2026))
        (should-not (calendar-sync--before-date-p jan-2026 dec-2025)))
    (test-calendar-sync--helpers-teardown)))

;;; calendar-sync--create-occurrence Tests

(ert-deftest test-calendar-sync--create-occurrence-creates-new-event ()
  "Test creating occurrence from base event."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let* ((base-event '(:summary "Test Event"
                          :start (2025 11 1 10 0 0)
                          :end (2025 11 1 11 0 0)))
             (new-date '(2025 11 15 10 0 0))
             (occurrence (calendar-sync--create-occurrence base-event new-date)))
        ;; Should have same summary
        (should (equal (plist-get occurrence :summary) "Test Event"))
        ;; Should have new start date
        (should (equal (plist-get occurrence :start) new-date))
        ;; Should have end date with same day as start
        (let ((end (plist-get occurrence :end)))
          (should (= (nth 0 end) 2025))
          (should (= (nth 1 end) 11))
          (should (= (nth 2 end) 15))))
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--create-occurrence-preserves-time ()
  "Test that occurrence preserves time from base event."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let* ((base-event '(:summary "Morning Meeting"
                          :start (2025 11 1 9 30 0)
                          :end (2025 11 1 10 30 0)))
             (new-date '(2025 11 15 9 30 0))
             (occurrence (calendar-sync--create-occurrence base-event new-date)))
        ;; End time should preserve hours/minutes from base event
        (let ((end (plist-get occurrence :end)))
          (should (= (nth 3 end) 10))  ; hour
          (should (= (nth 4 end) 30)))) ; minute
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--create-occurrence-handles-no-end-time ()
  "Test creating occurrence when base event has no end time."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let* ((base-event '(:summary "All Day Event"
                          :start (2025 11 1 0 0 0)))
             (new-date '(2025 11 15 0 0 0))
             (occurrence (calendar-sync--create-occurrence base-event new-date)))
        ;; Should have start but no end
        (should (equal (plist-get occurrence :start) new-date))
        (should (null (plist-get occurrence :end))))
    (test-calendar-sync--helpers-teardown)))

(ert-deftest test-calendar-sync--create-occurrence-does-not-modify-original ()
  "Test that creating occurrence doesn't modify base event."
  (test-calendar-sync--helpers-setup)
  (unwind-protect
      (let* ((original-start '(2025 11 1 10 0 0))
             (base-event (list :summary "Test"
                              :start original-start))
             (new-date '(2025 11 15 10 0 0)))
        (calendar-sync--create-occurrence base-event new-date)
        ;; Original should be unchanged
        (should (equal (plist-get base-event :start) original-start)))
    (test-calendar-sync--helpers-teardown)))

(provide 'test-calendar-sync--helpers)
;;; test-calendar-sync--helpers.el ends here
