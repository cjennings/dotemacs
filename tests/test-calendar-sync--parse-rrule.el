;;; test-calendar-sync--parse-rrule.el --- Tests for calendar-sync--parse-rrule  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--parse-rrule function.
;; Tests parsing of iCalendar RRULE strings into plist format.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Setup and Teardown

(defun test-calendar-sync--parse-rrule-setup ()
  "Setup for calendar-sync--parse-rrule tests."
  ;; No setup required for pure parsing tests
  nil)

(defun test-calendar-sync--parse-rrule-teardown ()
  "Teardown for calendar-sync--parse-rrule tests."
  ;; No teardown required
  nil)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-rrule-normal-weekly-returns-plist ()
  "Test parsing simple weekly recurrence rule."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=WEEKLY")))
        (should (eq (plist-get result :freq) 'weekly))
        (should (= (plist-get result :interval) 1)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-normal-weekly-with-byday-returns-plist ()
  "Test parsing weekly recurrence with specific weekdays."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=WEEKLY;BYDAY=SA")))
        (should (eq (plist-get result :freq) 'weekly))
        (should (equal (plist-get result :byday) '("SA")))
        (should (= (plist-get result :interval) 1)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-normal-daily-returns-plist ()
  "Test parsing daily recurrence rule."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=DAILY")))
        (should (eq (plist-get result :freq) 'daily))
        (should (= (plist-get result :interval) 1)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-normal-monthly-returns-plist ()
  "Test parsing monthly recurrence rule."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=MONTHLY")))
        (should (eq (plist-get result :freq) 'monthly))
        (should (= (plist-get result :interval) 1)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-normal-yearly-returns-plist ()
  "Test parsing yearly recurrence rule."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=YEARLY")))
        (should (eq (plist-get result :freq) 'yearly))
        (should (= (plist-get result :interval) 1)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-normal-with-interval-returns-plist ()
  "Test parsing recurrence rule with custom interval."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=WEEKLY;INTERVAL=2")))
        (should (eq (plist-get result :freq) 'weekly))
        (should (= (plist-get result :interval) 2)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-normal-with-count-returns-plist ()
  "Test parsing recurrence rule with count limit."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=DAILY;COUNT=10")))
        (should (eq (plist-get result :freq) 'daily))
        (should (= (plist-get result :count) 10)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-normal-with-until-returns-plist ()
  "Test parsing recurrence rule with end date."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let* ((result (calendar-sync--parse-rrule "FREQ=WEEKLY;UNTIL=20261118T120000Z"))
             (until (plist-get result :until)))
        (should (eq (plist-get result :freq) 'weekly))
        (should (listp until))
        (should (= (nth 0 until) 2026))  ; year
        (should (= (nth 1 until) 11))    ; month
        ;; Day might be 17 or 18 depending on timezone conversion
        (should (member (nth 2 until) '(17 18))))
    (test-calendar-sync--parse-rrule-teardown)))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-rrule-boundary-multiple-byday-returns-list ()
  "Test parsing BYDAY with multiple weekdays."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=WEEKLY;BYDAY=MO,WE,FR")))
        (should (eq (plist-get result :freq) 'weekly))
        (should (equal (plist-get result :byday) '("MO" "WE" "FR"))))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-boundary-all-parameters-returns-plist ()
  "Test parsing RRULE with all supported parameters."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=WEEKLY;INTERVAL=2;BYDAY=SA;UNTIL=20261118T000000Z;COUNT=52")))
        (should (eq (plist-get result :freq) 'weekly))
        (should (= (plist-get result :interval) 2))
        (should (equal (plist-get result :byday) '("SA")))
        (should (plist-get result :until))
        (should (= (plist-get result :count) 52)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-boundary-interval-one-returns-default ()
  "Test that default interval is 1 when not specified."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=DAILY")))
        (should (= (plist-get result :interval) 1)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-boundary-large-interval-returns-number ()
  "Test parsing RRULE with large interval value."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=MONTHLY;INTERVAL=12")))
        (should (= (plist-get result :interval) 12)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-boundary-large-count-returns-number ()
  "Test parsing RRULE with large count value."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=DAILY;COUNT=365")))
        (should (= (plist-get result :count) 365)))
    (test-calendar-sync--parse-rrule-teardown)))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-rrule-error-empty-string-returns-plist ()
  "Test parsing empty RRULE string returns plist with defaults."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "")))
        (should (listp result))
        (should (= (plist-get result :interval) 1)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-error-unsupported-freq-returns-symbol ()
  "Test parsing RRULE with unsupported frequency."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=HOURLY")))
        (should (eq (plist-get result :freq) 'hourly)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-error-invalid-until-returns-nil ()
  "Test parsing RRULE with malformed UNTIL date."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=DAILY;UNTIL=invalid")))
        (should (eq (plist-get result :freq) 'daily))
        (should (null (plist-get result :until))))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-error-invalid-count-returns-zero ()
  "Test parsing RRULE with non-numeric COUNT."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=DAILY;COUNT=abc")))
        (should (eq (plist-get result :freq) 'daily))
        (should (= (plist-get result :count) 0)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-error-invalid-interval-returns-zero ()
  "Test parsing RRULE with non-numeric INTERVAL."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "FREQ=WEEKLY;INTERVAL=xyz")))
        (should (eq (plist-get result :freq) 'weekly))
        (should (= (plist-get result :interval) 0)))
    (test-calendar-sync--parse-rrule-teardown)))

(ert-deftest test-calendar-sync--parse-rrule-error-missing-freq-returns-plist ()
  "Test parsing RRULE without FREQ parameter."
  (test-calendar-sync--parse-rrule-setup)
  (unwind-protect
      (let ((result (calendar-sync--parse-rrule "INTERVAL=2;COUNT=10")))
        (should (listp result))
        (should (null (plist-get result :freq)))
        (should (= (plist-get result :interval) 2))
        (should (= (plist-get result :count) 10)))
    (test-calendar-sync--parse-rrule-teardown)))

(provide 'test-calendar-sync--parse-rrule)
;;; test-calendar-sync--parse-rrule.el ends here
