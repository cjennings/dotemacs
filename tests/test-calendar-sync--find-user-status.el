;;; test-calendar-sync--find-user-status.el --- Tests for find-user-status  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--find-user-status function.
;; Given attendees list and user emails, returns user's PARTSTAT as lowercase string.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Test Data

(defun test-find-user-status--make-attendees ()
  "Create sample attendees list for testing."
  (list (list :cn "Alice" :email "alice@example.com" :partstat "ACCEPTED" :role "REQ-PARTICIPANT")
        (list :cn "Craig" :email "craig@example.com" :partstat "ACCEPTED" :role "REQ-PARTICIPANT")
        (list :cn "Bob" :email "bob@example.com" :partstat "DECLINED" :role "OPT-PARTICIPANT")))

;;; Normal Cases

(ert-deftest test-calendar-sync--find-user-status-normal-accepted ()
  "Test finding user who accepted."
  (let ((attendees (test-find-user-status--make-attendees))
        (emails '("craig@example.com")))
    (should (string= "accepted"
                      (calendar-sync--find-user-status attendees emails)))))

(ert-deftest test-calendar-sync--find-user-status-normal-declined ()
  "Test finding user who declined."
  (let ((attendees (test-find-user-status--make-attendees))
        (emails '("bob@example.com")))
    (should (string= "declined"
                      (calendar-sync--find-user-status attendees emails)))))

(ert-deftest test-calendar-sync--find-user-status-normal-tentative ()
  "Test finding user with tentative status."
  (let ((attendees (list (list :cn "Test" :email "test@example.com" :partstat "TENTATIVE")))
        (emails '("test@example.com")))
    (should (string= "tentative"
                      (calendar-sync--find-user-status attendees emails)))))

(ert-deftest test-calendar-sync--find-user-status-normal-needs-action ()
  "Test finding user with needs-action status."
  (let ((attendees (list (list :cn "Test" :email "test@example.com" :partstat "NEEDS-ACTION")))
        (emails '("test@example.com")))
    (should (string= "needs-action"
                      (calendar-sync--find-user-status attendees emails)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--find-user-status-boundary-not-in-list ()
  "Test user not in attendee list returns nil."
  (let ((attendees (test-find-user-status--make-attendees))
        (emails '("stranger@example.com")))
    (should (null (calendar-sync--find-user-status attendees emails)))))

(ert-deftest test-calendar-sync--find-user-status-boundary-empty-attendees ()
  "Test empty attendee list returns nil."
  (should (null (calendar-sync--find-user-status '() '("test@example.com")))))

(ert-deftest test-calendar-sync--find-user-status-boundary-multiple-emails ()
  "Test matching on second email in user emails list."
  (let ((attendees (test-find-user-status--make-attendees))
        (emails '("primary@other.com" "craig@example.com")))
    (should (string= "accepted"
                      (calendar-sync--find-user-status attendees emails)))))

(ert-deftest test-calendar-sync--find-user-status-boundary-case-insensitive ()
  "Test case-insensitive email matching."
  (let ((attendees (list (list :cn "Test" :email "Craig@Example.COM" :partstat "ACCEPTED")))
        (emails '("craig@example.com")))
    (should (string= "accepted"
                      (calendar-sync--find-user-status attendees emails)))))

;;; Error Cases

(ert-deftest test-calendar-sync--find-user-status-error-nil-attendees ()
  "Test nil attendees returns nil."
  (should (null (calendar-sync--find-user-status nil '("test@example.com")))))

(ert-deftest test-calendar-sync--find-user-status-error-nil-emails ()
  "Test nil emails list returns nil."
  (should (null (calendar-sync--find-user-status (test-find-user-status--make-attendees) nil))))

(provide 'test-calendar-sync--find-user-status)
;;; test-calendar-sync--find-user-status.el ends here
