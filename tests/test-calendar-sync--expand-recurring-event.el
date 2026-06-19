;;; test-calendar-sync--expand-recurring-event.el --- Tests for recurrence dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--expand-recurring-event — the dispatcher that maps
;; an RRULE frequency to the matching expander and applies EXDATE filtering.
;; The individual expanders, parser, and exdate helpers have their own tests;
;; here they are stubbed at the boundary so only the dispatch and the
;; exdate-vs-no-exdate branch are exercised.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

(defmacro test-cs-ere--with (overrides &rest body)
  "Run BODY with the recurrence helpers stubbed.
OVERRIDES is an extra list of cl-letf* bindings layered on the defaults:
RRULE present, parse-event returns 'BASE, no exdates, and every expander
errors if called (each test re-binds the one it expects).  cl-letf* is
sequential, so a re-bound place in OVERRIDES wins over the default."
  (declare (indent 1))
  `(cl-letf* (((symbol-function 'calendar-sync--get-property)
              (lambda (_e prop) (when (string= prop "RRULE") "R")))
             ((symbol-function 'calendar-sync--parse-event) (lambda (_e) 'BASE))
             ((symbol-function 'calendar-sync--collect-exdates) (lambda (_e) nil))
             ((symbol-function 'calendar-sync--expand-daily)
              (lambda (&rest _) (error "daily should not be called")))
             ((symbol-function 'calendar-sync--expand-weekly)
              (lambda (&rest _) (error "weekly should not be called")))
             ((symbol-function 'calendar-sync--expand-monthly)
              (lambda (&rest _) (error "monthly should not be called")))
             ((symbol-function 'calendar-sync--expand-yearly)
              (lambda (&rest _) (error "yearly should not be called")))
             ((symbol-function 'calendar-sync--filter-exdates)
              (lambda (&rest _) (error "filter-exdates should not be called")))
             ,@overrides)
     ,@body))

;;; Normal Cases — frequency dispatch

(ert-deftest test-calendar-sync--expand-recurring-event-dispatches-daily ()
  "Normal: FREQ=DAILY routes to the daily expander."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--parse-rrule) (lambda (_r) '(:freq daily)))
       ((symbol-function 'calendar-sync--expand-daily) (lambda (&rest _) '(DAILY))))
    (should (equal (calendar-sync--expand-recurring-event "evt" 'range) '(DAILY)))))

(ert-deftest test-calendar-sync--expand-recurring-event-dispatches-monthly ()
  "Normal: FREQ=MONTHLY routes to the monthly expander."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--parse-rrule) (lambda (_r) '(:freq monthly)))
       ((symbol-function 'calendar-sync--expand-monthly) (lambda (&rest _) '(MONTHLY))))
    (should (equal (calendar-sync--expand-recurring-event "evt" 'range) '(MONTHLY)))))

(ert-deftest test-calendar-sync--expand-recurring-event-dispatches-yearly ()
  "Normal: FREQ=YEARLY routes to the yearly expander."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--parse-rrule) (lambda (_r) '(:freq yearly)))
       ((symbol-function 'calendar-sync--expand-yearly) (lambda (&rest _) '(YEARLY))))
    (should (equal (calendar-sync--expand-recurring-event "evt" 'range) '(YEARLY)))))

;;; Boundary / Error Cases

(ert-deftest test-calendar-sync--expand-recurring-event-unsupported-freq-nil ()
  "Error: an unsupported frequency expands to nil, no expander called."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--parse-rrule) (lambda (_r) '(:freq hourly))))
    (should-not (calendar-sync--expand-recurring-event "evt" 'range))))

(ert-deftest test-calendar-sync--expand-recurring-event-no-rrule-nil ()
  "Boundary: an event with no RRULE returns nil (not a recurring event)."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--get-property) (lambda (&rest _) nil)))
    (should-not (calendar-sync--expand-recurring-event "evt" 'range))))

(ert-deftest test-calendar-sync--expand-recurring-event-unparseable-base-nil ()
  "Boundary: when the base event fails to parse, expansion returns nil."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--parse-rrule) (lambda (_r) '(:freq daily)))
       ((symbol-function 'calendar-sync--parse-event) (lambda (_e) nil)))
    (should-not (calendar-sync--expand-recurring-event "evt" 'range))))

;;; EXDATE branch

(ert-deftest test-calendar-sync--expand-recurring-event-applies-exdate-filter ()
  "Normal: with exdates present, occurrences pass through the exdate filter."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--parse-rrule) (lambda (_r) '(:freq daily)))
       ((symbol-function 'calendar-sync--expand-daily) (lambda (&rest _) '(O1 O2)))
       ((symbol-function 'calendar-sync--collect-exdates) (lambda (_e) '(EX)))
       ((symbol-function 'calendar-sync--filter-exdates)
        (lambda (occs _ex) (remq 'O2 occs))))
    (should (equal (calendar-sync--expand-recurring-event "evt" 'range) '(O1)))))

(ert-deftest test-calendar-sync--expand-recurring-event-no-exdate-skips-filter ()
  "Boundary: with no exdates, the filter is skipped and occurrences pass through."
  (test-cs-ere--with
      (((symbol-function 'calendar-sync--parse-rrule) (lambda (_r) '(:freq daily)))
       ((symbol-function 'calendar-sync--expand-daily) (lambda (&rest _) '(O1 O2))))
    ;; filter-exdates stays the error stub; it must not be called here
    (should (equal (calendar-sync--expand-recurring-event "evt" 'range) '(O1 O2)))))

(provide 'test-calendar-sync--expand-recurring-event)
;;; test-calendar-sync--expand-recurring-event.el ends here
