;;; test-calendar-sync--event-start-time.el --- Tests for event start time extraction -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--event-start-time. Extracts comparable time value from event plist.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--event-start-time-normal-timed ()
  "Timed event returns non-zero time value."
  (let ((event (list :start '(2026 3 15 14 30) :summary "Test")))
    (should (not (equal 0 (calendar-sync--event-start-time event))))))

(ert-deftest test-calendar-sync--event-start-time-normal-ordering ()
  "Earlier event has smaller time value than later event."
  (let ((early (list :start '(2026 3 15 9 0)))
        (late (list :start '(2026 3 15 17 0))))
    (should (time-less-p (calendar-sync--event-start-time early)
                         (calendar-sync--event-start-time late)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--event-start-time-boundary-all-day ()
  "All-day event (nil hour/minute) uses 0 for time components."
  (let ((event (list :start '(2026 3 15 nil nil))))
    (should (not (equal 0 (calendar-sync--event-start-time event))))))

;;; Error Cases

(ert-deftest test-calendar-sync--event-start-time-error-no-start ()
  "Event without :start returns 0."
  (let ((event (list :summary "No start")))
    (should (equal 0 (calendar-sync--event-start-time event)))))

(provide 'test-calendar-sync--event-start-time)
;;; test-calendar-sync--event-start-time.el ends here
