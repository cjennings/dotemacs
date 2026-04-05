;;; test-calendar-sync--split-events.el --- Tests for VEVENT block splitter -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--split-events.
;; Splits raw .ics content into individual VEVENT block strings.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--split-events-normal-single ()
  "Single VEVENT returns one-element list."
  (let* ((ics "BEGIN:VCALENDAR\nBEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT\nEND:VCALENDAR")
         (result (calendar-sync--split-events ics)))
    (should (= 1 (length result)))
    (should (string-match-p "SUMMARY:Test" (car result)))))

(ert-deftest test-calendar-sync--split-events-normal-multiple ()
  "Multiple VEVENTs return corresponding list."
  (let* ((ics (concat "BEGIN:VCALENDAR\n"
                      "BEGIN:VEVENT\nSUMMARY:First\nEND:VEVENT\n"
                      "BEGIN:VEVENT\nSUMMARY:Second\nEND:VEVENT\n"
                      "END:VCALENDAR"))
         (result (calendar-sync--split-events ics)))
    (should (= 2 (length result)))
    (should (string-match-p "First" (nth 0 result)))
    (should (string-match-p "Second" (nth 1 result)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--split-events-boundary-empty-calendar ()
  "Calendar with no VEVENTs returns empty list."
  (let ((ics "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR"))
    (should (null (calendar-sync--split-events ics)))))

(ert-deftest test-calendar-sync--split-events-boundary-preserves-content ()
  "Each extracted block contains BEGIN:VEVENT through END:VEVENT."
  (let* ((ics "BEGIN:VEVENT\nUID:abc\nSUMMARY:Test\nEND:VEVENT")
         (result (calendar-sync--split-events ics)))
    (should (string-prefix-p "BEGIN:VEVENT" (car result)))
    (should (string-suffix-p "END:VEVENT" (car result)))))

;;; Error Cases

(ert-deftest test-calendar-sync--split-events-error-empty-string ()
  "Empty string returns empty list."
  (should (null (calendar-sync--split-events ""))))

(provide 'test-calendar-sync--split-events)
;;; test-calendar-sync--split-events.el ends here
