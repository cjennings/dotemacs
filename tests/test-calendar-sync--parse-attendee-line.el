;;; test-calendar-sync--parse-attendee-line.el --- Tests for attendee parsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--parse-attendee-line function.
;; Parses single ATTENDEE line into plist (:cn NAME :email EMAIL :partstat STATUS :role ROLE).
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-attendee-line-normal-accepted ()
  "Test parsing attendee with PARTSTAT=ACCEPTED."
  (let ((line "ATTENDEE;CN=Alice Smith;PARTSTAT=ACCEPTED;ROLE=REQ-PARTICIPANT:mailto:alice@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "Alice Smith" (plist-get result :cn)))
      (should (string= "alice@example.com" (plist-get result :email)))
      (should (string= "ACCEPTED" (plist-get result :partstat)))
      (should (string= "REQ-PARTICIPANT" (plist-get result :role))))))

(ert-deftest test-calendar-sync--parse-attendee-line-normal-declined ()
  "Test parsing attendee with PARTSTAT=DECLINED."
  (let ((line "ATTENDEE;CN=Bob Jones;PARTSTAT=DECLINED:mailto:bob@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "Bob Jones" (plist-get result :cn)))
      (should (string= "bob@example.com" (plist-get result :email)))
      (should (string= "DECLINED" (plist-get result :partstat))))))

(ert-deftest test-calendar-sync--parse-attendee-line-normal-tentative ()
  "Test parsing attendee with PARTSTAT=TENTATIVE."
  (let ((line "ATTENDEE;CN=Carol;PARTSTAT=TENTATIVE:mailto:carol@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "TENTATIVE" (plist-get result :partstat))))))

(ert-deftest test-calendar-sync--parse-attendee-line-normal-needs-action ()
  "Test parsing attendee with PARTSTAT=NEEDS-ACTION."
  (let ((line "ATTENDEE;CN=Dave;PARTSTAT=NEEDS-ACTION:mailto:dave@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "NEEDS-ACTION" (plist-get result :partstat))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-attendee-line-boundary-no-cn ()
  "Test attendee without CN uses email as fallback."
  (let ((line "ATTENDEE;PARTSTAT=ACCEPTED:mailto:noCN@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (null (plist-get result :cn)))
      (should (string= "noCN@example.com" (plist-get result :email))))))

(ert-deftest test-calendar-sync--parse-attendee-line-boundary-no-partstat ()
  "Test attendee without PARTSTAT."
  (let ((line "ATTENDEE;CN=Eve:mailto:eve@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "Eve" (plist-get result :cn)))
      (should (string= "eve@example.com" (plist-get result :email)))
      (should (null (plist-get result :partstat))))))

(ert-deftest test-calendar-sync--parse-attendee-line-boundary-minimal ()
  "Test minimal attendee line with just mailto."
  (let ((line "ATTENDEE:mailto:min@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "min@example.com" (plist-get result :email))))))

(ert-deftest test-calendar-sync--parse-attendee-line-boundary-cutype-resource ()
  "Test attendee with CUTYPE=RESOURCE."
  (let ((line "ATTENDEE;CN=Room A;CUTYPE=RESOURCE;PARTSTAT=ACCEPTED:mailto:room-a@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "Room A" (plist-get result :cn)))
      (should (string= "ACCEPTED" (plist-get result :partstat))))))

(ert-deftest test-calendar-sync--parse-attendee-line-boundary-extra-params ()
  "Test attendee with extra unknown parameters."
  (let ((line "ATTENDEE;CN=Frank;RSVP=TRUE;X-NUM-GUESTS=0;PARTSTAT=ACCEPTED:mailto:frank@example.com"))
    (let ((result (calendar-sync--parse-attendee-line line)))
      (should (string= "Frank" (plist-get result :cn)))
      (should (string= "ACCEPTED" (plist-get result :partstat))))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-attendee-line-error-nil ()
  "Test nil input returns nil."
  (should (null (calendar-sync--parse-attendee-line nil))))

(ert-deftest test-calendar-sync--parse-attendee-line-error-empty ()
  "Test empty string returns nil."
  (should (null (calendar-sync--parse-attendee-line ""))))

(ert-deftest test-calendar-sync--parse-attendee-line-error-malformed ()
  "Test malformed line returns nil."
  (should (null (calendar-sync--parse-attendee-line "not an attendee line"))))

(provide 'test-calendar-sync--parse-attendee-line)
;;; test-calendar-sync--parse-attendee-line.el ends here
