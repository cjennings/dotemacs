;;; test-calendar-sync--parse-exception-event.el --- Tests for one-event exception parsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--parse-exception-event, the per-VEVENT half of
;; calendar-sync--collect-recurrence-exceptions: it turns a single RECURRENCE-ID
;; override VEVENT into an exception plist (or nil).  One function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

(defun test-cs-parse-exc--override-event (start end)
  "Return a RECURRENCE-ID override VEVENT string for START..END."
  (concat "BEGIN:VEVENT\n"
          "UID:override@google.com\n"
          "RECURRENCE-ID:20260203T090000Z\n"
          "SUMMARY:Rescheduled Meeting\n"
          "DTSTART:" (test-calendar-sync-ics-datetime start) "\n"
          "DTEND:" (test-calendar-sync-ics-datetime end) "\n"
          "END:VEVENT"))

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-exception-event-normal-returns-plist ()
  "Normal: a RECURRENCE-ID override parses into a plist with its overridden times."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (plist (calendar-sync--parse-exception-event
                 (test-cs-parse-exc--override-event start end))))
    (should plist)
    (should (plist-get plist :recurrence-id))
    (should (equal "20260203T090000Z" (plist-get plist :recurrence-id-raw)))
    (should (plist-get plist :start))
    (should (plist-get plist :end))
    (should (equal "Rescheduled Meeting" (plist-get plist :summary)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-exception-event-boundary-no-recurrence-id ()
  "Boundary: a VEVENT with no RECURRENCE-ID is not an override and returns nil."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (event (test-calendar-sync-make-vevent "Regular Event" start end)))
    (should-not (calendar-sync--parse-exception-event event))))

;;; Normal Cases — attendee extraction (singly-declined occurrence)

(ert-deftest test-calendar-sync--parse-exception-event-extracts-attendees ()
  "Normal: a RECURRENCE-ID override carrying an ATTENDEE block parses :attendees,
so a singly-declined occurrence can have its user status re-derived downstream
by `calendar-sync--apply-single-exception'."
  (let* ((start (test-calendar-sync-time-days-from-now 7 10 0))
         (end (test-calendar-sync-time-days-from-now 7 11 0))
         (event (concat "BEGIN:VEVENT\n"
                        "UID:override@google.com\n"
                        "RECURRENCE-ID:20260203T090000Z\n"
                        "SUMMARY:1:1 with Hayk\n"
                        "ATTENDEE;CN=Craig;PARTSTAT=DECLINED:mailto:craig@example.com\n"
                        "DTSTART:" (test-calendar-sync-ics-datetime start) "\n"
                        "DTEND:" (test-calendar-sync-ics-datetime end) "\n"
                        "END:VEVENT"))
         (plist (calendar-sync--parse-exception-event event))
         (attendees (plist-get plist :attendees)))
    (should attendees)
    (should (equal "craig@example.com" (plist-get (car attendees) :email)))
    (should (equal "DECLINED" (plist-get (car attendees) :partstat)))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-exception-event-error-unparseable-times ()
  "Error: a RECURRENCE-ID override whose times do not parse returns nil rather
than a half-built plist."
  (let ((event (concat "BEGIN:VEVENT\n"
                       "UID:broken@google.com\n"
                       "RECURRENCE-ID:not-a-timestamp\n"
                       "SUMMARY:Broken Override\n"
                       "DTSTART:also-garbage\n"
                       "END:VEVENT")))
    (should-not (calendar-sync--parse-exception-event event))))

(provide 'test-calendar-sync--parse-exception-event)
;;; test-calendar-sync--parse-exception-event.el ends here
