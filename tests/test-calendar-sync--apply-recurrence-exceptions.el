;;; test-calendar-sync--apply-recurrence-exceptions.el --- Tests for applying exceptions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--apply-recurrence-exceptions function.
;; Tests applying RECURRENCE-ID exceptions to expanded occurrences.
;; Following quality-engineer.org guidelines: one function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Test Data Helpers

(defun test-make-occurrence (year month day hour minute summary &optional uid)
  "Create an occurrence plist for testing.
Mirrors the format used by calendar-sync expansion functions.
Uses :start (year month day hour minute) format."
  (list :start (list year month day hour minute)
        :end (list year month day (1+ hour) minute)
        :summary summary
        :uid (or uid "test-event@google.com")))

(defun test-make-exception-data (rec-year rec-month rec-day rec-hour rec-minute
                                  new-year new-month new-day new-hour new-minute
                                  &optional summary)
  "Create exception data plist for testing.
REC-* are the original occurrence date/time (recurrence-id).
NEW-* values are the rescheduled time."
  (list :recurrence-id (list rec-year rec-month rec-day rec-hour rec-minute)
        :start (list new-year new-month new-day new-hour new-minute)
        :end (list new-year new-month new-day (1+ new-hour) new-minute)
        :summary (or summary "Rescheduled")))

;;; Normal Cases

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-normal-single-replacement ()
  "Test replacing single occurrence with exception."
  (let* ((occurrences (list (test-make-occurrence 2026 2 3 9 0 "Weekly Meeting")))
         (exceptions (make-hash-table :test 'equal)))
    ;; Exception: Feb 3 at 9:00 → Feb 3 at 10:00
    (puthash "test-event@google.com"
             (list (test-make-exception-data 2026 2 3 9 0  2026 2 3 10 0))
             exceptions)
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences exceptions)))
      (should (= 1 (length result)))
      ;; Time should be updated to 10:00
      (should (= 10 (nth 3 (plist-get (car result) :start)))))))

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-normal-multiple-some-replaced ()
  "Test multiple occurrences with one exception."
  (let* ((occurrences (list (test-make-occurrence 2026 2 3 9 0 "Weekly Meeting")
                            (test-make-occurrence 2026 2 10 9 0 "Weekly Meeting")
                            (test-make-occurrence 2026 2 17 9 0 "Weekly Meeting")))
         (exceptions (make-hash-table :test 'equal)))
    ;; Only Feb 10 is rescheduled
    (puthash "test-event@google.com"
             (list (test-make-exception-data 2026 2 10 9 0  2026 2 10 11 0))
             exceptions)
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences exceptions)))
      (should (= 3 (length result)))
      ;; Feb 3: unchanged (9:00)
      (should (= 9 (nth 3 (plist-get (nth 0 result) :start))))
      ;; Feb 10: rescheduled (11:00)
      (should (= 11 (nth 3 (plist-get (nth 1 result) :start))))
      ;; Feb 17: unchanged (9:00)
      (should (= 9 (nth 3 (plist-get (nth 2 result) :start)))))))

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-normal-date-change ()
  "Test exception that changes the date, not just time."
  (let* ((occurrences (list (test-make-occurrence 2026 2 3 9 0 "Weekly Meeting")))
         (exceptions (make-hash-table :test 'equal)))
    ;; Feb 3 rescheduled to Feb 4
    (puthash "test-event@google.com"
             (list (test-make-exception-data 2026 2 3 9 0  2026 2 4 10 0))
             exceptions)
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences exceptions)))
      (should (= 1 (length result)))
      (let ((new-start (plist-get (car result) :start)))
        (should (= 4 (nth 2 new-start)))   ; day
        (should (= 10 (nth 3 new-start))))))) ; hour

;;; Boundary Cases

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-boundary-no-exceptions ()
  "Test occurrences returned unchanged when no exceptions."
  (let* ((occurrences (list (test-make-occurrence 2026 2 3 9 0 "Weekly Meeting")
                            (test-make-occurrence 2026 2 10 9 0 "Weekly Meeting")))
         (exceptions (make-hash-table :test 'equal)))
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences exceptions)))
      (should (= 2 (length result)))
      (should (= 9 (nth 3 (plist-get (nth 0 result) :start))))
      (should (= 9 (nth 3 (plist-get (nth 1 result) :start)))))))

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-boundary-exception-no-match ()
  "Test exception for different UID doesn't affect occurrences."
  (let* ((occurrences (list (test-make-occurrence 2026 2 3 9 0 "Weekly Meeting" "event-a@google.com")))
         (exceptions (make-hash-table :test 'equal)))
    ;; Exception is for different UID
    (puthash "event-b@google.com"
             (list (test-make-exception-data 2026 2 3 9 0  2026 2 3 11 0))
             exceptions)
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences exceptions)))
      (should (= 1 (length result)))
      ;; Should remain at 9:00, not 11:00
      (should (= 9 (nth 3 (plist-get (car result) :start)))))))

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-boundary-multiple-uids ()
  "Test exceptions for multiple different recurring events."
  (let* ((occurrences (list (test-make-occurrence 2026 2 3 9 0 "Meeting A" "event-a@google.com")
                            (test-make-occurrence 2026 2 3 14 0 "Meeting B" "event-b@google.com")))
         (exceptions (make-hash-table :test 'equal)))
    ;; Different exceptions for each UID
    (puthash "event-a@google.com"
             (list (test-make-exception-data 2026 2 3 9 0  2026 2 3 10 0))
             exceptions)
    (puthash "event-b@google.com"
             (list (test-make-exception-data 2026 2 3 14 0  2026 2 3 15 0))
             exceptions)
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences exceptions)))
      (should (= 2 (length result)))
      ;; Each should have its respective exception applied
      (should (= 10 (nth 3 (plist-get (nth 0 result) :start))))
      (should (= 15 (nth 3 (plist-get (nth 1 result) :start)))))))

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-boundary-empty-occurrences ()
  "Test empty occurrences list returns empty."
  (let* ((occurrences '())
         (exceptions (make-hash-table :test 'equal)))
    (puthash "test@google.com"
             (list (test-make-exception-data 2026 2 3 9 0  2026 2 3 10 0))
             exceptions)
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences exceptions)))
      (should (= 0 (length result))))))

;;; Error Cases

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-error-nil-occurrences ()
  "Test nil occurrences handled gracefully."
  (let ((exceptions (make-hash-table :test 'equal)))
    (let ((result (calendar-sync--apply-recurrence-exceptions nil exceptions)))
      (should (or (null result) (= 0 (length result)))))))

(ert-deftest test-calendar-sync--apply-recurrence-exceptions-error-nil-exceptions ()
  "Test nil exceptions handled gracefully."
  (let ((occurrences (list (test-make-occurrence 2026 2 3 9 0 "Meeting"))))
    (let ((result (calendar-sync--apply-recurrence-exceptions occurrences nil)))
      ;; Should return occurrences unchanged or handle nil
      (should (or (null result)
                  (and (= 1 (length result))
                       (= 9 (nth 3 (plist-get (car result) :start)))))))))

(provide 'test-calendar-sync--apply-recurrence-exceptions)
;;; test-calendar-sync--apply-recurrence-exceptions.el ends here
