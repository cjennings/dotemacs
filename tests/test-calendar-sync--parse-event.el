;;; test-calendar-sync--parse-event.el --- Tests for VEVENT parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--parse-event.
;; Parses VEVENT string into plist with :uid :summary :start :end etc.
;; Uses dynamic timestamps via testutil-calendar-sync helpers.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-event-normal-basic-fields ()
  "Basic event returns plist with summary, start, end."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (vevent (test-calendar-sync-make-vevent "Team Standup" start end)))
    (let ((result (calendar-sync--parse-event vevent)))
      (should result)
      (should (equal "Team Standup" (plist-get result :summary)))
      (should (plist-get result :start))
      (should (plist-get result :end)))))

(ert-deftest test-calendar-sync--parse-event-normal-optional-fields ()
  "Event with description and location includes them."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (vevent (test-calendar-sync-make-vevent
                  "Meeting" start end "Discuss roadmap" "Room 42")))
    (let ((result (calendar-sync--parse-event vevent)))
      (should (equal "Discuss roadmap" (plist-get result :description)))
      (should (equal "Room 42" (plist-get result :location))))))

(ert-deftest test-calendar-sync--parse-event-normal-all-day ()
  "All-day event (date-only) is parsed correctly."
  (let* ((start (test-calendar-sync-time-date-only 5))
         (end (test-calendar-sync-time-date-only 6))
         (vevent (test-calendar-sync-make-vevent "Holiday" start end)))
    (let ((result (calendar-sync--parse-event vevent)))
      (should result)
      (should (= 3 (length (plist-get result :start)))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-event-boundary-recurrence-id-skipped ()
  "Events with RECURRENCE-ID are skipped (return nil)."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (vevent (concat "BEGIN:VEVENT\n"
                         "SUMMARY:Modified Instance\n"
                         "DTSTART:" (test-calendar-sync-ics-datetime start) "\n"
                         "RECURRENCE-ID:" (test-calendar-sync-ics-datetime start) "\n"
                         "END:VEVENT")))
    (should (null (calendar-sync--parse-event vevent)))))

(ert-deftest test-calendar-sync--parse-event-boundary-no-end-time ()
  "Event without DTEND still parses (end is nil)."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (vevent (concat "BEGIN:VEVENT\n"
                         "SUMMARY:Open-ended\n"
                         "DTSTART:" (test-calendar-sync-ics-datetime start) "\n"
                         "END:VEVENT")))
    (let ((result (calendar-sync--parse-event vevent)))
      (should result)
      (should (null (plist-get result :end))))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-event-error-no-summary ()
  "Event without SUMMARY returns nil."
  (let ((vevent "BEGIN:VEVENT\nDTSTART:20260315T140000Z\nEND:VEVENT"))
    (should (null (calendar-sync--parse-event vevent)))))

(ert-deftest test-calendar-sync--parse-event-error-no-dtstart ()
  "Event without DTSTART returns nil."
  (let ((vevent "BEGIN:VEVENT\nSUMMARY:Orphan\nEND:VEVENT"))
    (should (null (calendar-sync--parse-event vevent)))))

(provide 'test-calendar-sync--parse-event)
;;; test-calendar-sync--parse-event.el ends here
