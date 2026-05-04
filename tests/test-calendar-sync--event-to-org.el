;;; test-calendar-sync--event-to-org.el --- Tests for updated event-to-org formatter  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--event-to-org function (updated version).
;; Tests the new property drawer format with LOCATION, ORGANIZER, STATUS, URL.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

(defun test-calendar-sync--count-line-matches (regexp text)
  "Count lines in TEXT that match REGEXP."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count)))
      count)))

;;; Normal Cases

(ert-deftest test-calendar-sync--event-to-org-normal-all-fields ()
  "Test event with all fields produces property drawer + description."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Team Standup"
                      :start start
                      :end end
                      :description "Daily sync meeting"
                      :location "Conference Room A"
                      :organizer (list :cn "John Smith" :email "john@example.com")
                      :status "accepted"
                      :url "https://meet.google.com/abc-defg-hij")))
    (let ((result (calendar-sync--event-to-org event)))
      ;; Verify heading comes before timestamp
      (should (string-match "\\* Team Standup" result))
      (let ((heading-pos (match-beginning 0)))
        (should (string-match "<[0-9]" result))
        (should (< heading-pos (match-beginning 0))))
      (should (string-match-p ":PROPERTIES:" result))
      (should (string-match-p ":LOCATION: Conference Room A" result))
      (should (string-match-p ":ORGANIZER: John Smith" result))
      (should (string-match-p ":STATUS: accepted" result))
      (should (string-match-p ":URL: https://meet.google.com/abc-defg-hij" result))
      (should (string-match-p ":END:" result))
      (should (string-match-p "Daily sync meeting" result)))))

(ert-deftest test-calendar-sync--event-to-org-normal-summary-and-time-only ()
  "Test event with only summary and time (no drawer)."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Quick Chat"
                      :start start
                      :end end)))
    (let ((result (calendar-sync--event-to-org event)))
      (should (string-match-p "\\* Quick Chat" result))
      (should-not (string-match-p ":PROPERTIES:" result)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--event-to-org-boundary-no-description ()
  "Test event with location but no description."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Meeting"
                      :start start
                      :end end
                      :location "Room B")))
    (let ((result (calendar-sync--event-to-org event)))
      (should (string-match-p ":LOCATION: Room B" result))
      ;; After :END: there should be no body text
      (should-not (string-match-p ":END:\n." result)))))

(ert-deftest test-calendar-sync--event-to-org-boundary-no-location-no-attendees ()
  "Test event without location or attendees produces no drawer."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Simple Event"
                      :start start
                      :end end
                      :description "Just a note")))
    (let ((result (calendar-sync--event-to-org event)))
      (should (string-match-p "\\* Simple Event" result))
      ;; Description goes after timestamp, no drawer needed
      (should (string-match-p "Just a note" result)))))

(ert-deftest test-calendar-sync--event-to-org-boundary-only-status ()
  "Test event with only status produces drawer."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Status Only"
                      :start start
                      :end end
                      :status "tentative")))
    (let ((result (calendar-sync--event-to-org event)))
      (should (string-match-p ":PROPERTIES:" result))
      (should (string-match-p ":STATUS: tentative" result))
      (should (string-match-p ":END:" result)))))

(ert-deftest test-calendar-sync--event-to-org-boundary-description-with-asterisks ()
  "Test event description containing org-special asterisks are sanitized."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Meeting"
                      :start start
                      :end end
                      :description "* agenda item 1\n** sub-item")))
    (let ((result (calendar-sync--event-to-org event)))
      ;; Description content should be present
      (should (string-match-p "agenda item" result))
      ;; Leading asterisks replaced with dashes to prevent org heading conflicts
      (should (string-match-p "^- agenda item 1" result))
      (should (string-match-p "^-- sub-item" result))
      ;; Only the event heading should use *
      (should (= 1 (length (split-string result "^\\* " t)))))))

(ert-deftest test-calendar-sync--event-to-org-boundary-summary-structure-is-flattened ()
  "Test event summary cannot create additional Org headings."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "* Planning\n** Hidden task"
                      :start start
                      :end end)))
    (let ((result (calendar-sync--event-to-org event)))
      (should (string-match-p "^\\* - Planning -- Hidden task$" result))
      (should-not (string-match-p "^\\*\\* Hidden task" result))
      (should (= 1 (length (split-string result "^\\* " t)))))))

(ert-deftest test-calendar-sync--event-to-org-boundary-property-structure-is-flattened ()
  "Test property values cannot create extra drawer lines or close the drawer."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Meeting"
                      :start start
                      :end end
                      :location "Room 1\n:END:\n* Not a real heading"
                      :organizer (list :cn "Jane\n:STATUS: fake"
                                       :email "jane@example.com")
                      :status "accepted\n:LOCATION: fake"
                      :url "https://example.com/a\n:PROPERTIES:")))
    (let ((result (calendar-sync--event-to-org event)))
      (should (string-match-p ":LOCATION: Room 1 :END: \\* Not a real heading" result))
      (should (string-match-p ":ORGANIZER: Jane :STATUS: fake" result))
      (should (string-match-p ":STATUS: accepted :LOCATION: fake" result))
      (should (string-match-p ":URL: https://example.com/a :PROPERTIES:" result))
      (should (= 1 (test-calendar-sync--count-line-matches "^:LOCATION:" result)))
      (should (= 1 (test-calendar-sync--count-line-matches "^:STATUS:" result)))
      (should (= 1 (test-calendar-sync--count-line-matches "^:END:$" result)))
      (should-not (string-match-p "^\\* Not a real heading" result)))))

(ert-deftest test-calendar-sync--event-to-org-boundary-organizer-email-only ()
  "Test organizer without CN shows email."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary "Meeting"
                      :start start
                      :end end
                      :organizer (list :cn nil :email "org@example.com"))))
    (let ((result (calendar-sync--event-to-org event)))
      (should (string-match-p ":ORGANIZER: org@example.com" result)))))

;;; Error Cases

(ert-deftest test-calendar-sync--event-to-org-error-missing-summary ()
  "Test event with nil summary still produces output."
  (let* ((start (test-calendar-sync-time-days-from-now 5 14 0))
         (end (test-calendar-sync-time-days-from-now 5 15 0))
         (event (list :summary nil
                      :start start
                      :end end)))
    ;; Should not error - produce some output
    (should (stringp (calendar-sync--event-to-org event)))))

(provide 'test-calendar-sync--event-to-org)
;;; test-calendar-sync--event-to-org.el ends here
