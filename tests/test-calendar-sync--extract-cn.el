;;; test-calendar-sync--extract-cn.el --- Tests for CN parameter extraction -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--extract-cn.
;; Extracts and dequotes the CN= parameter from iCal property lines.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--extract-cn-normal-unquoted ()
  "Extracts unquoted CN value."
  (should (equal "Craig Jennings"
                 (calendar-sync--extract-cn
                  "ATTENDEE;CN=Craig Jennings;PARTSTAT=ACCEPTED:mailto:c@test.com"))))

(ert-deftest test-calendar-sync--extract-cn-normal-quoted ()
  "Strips surrounding quotes from CN value."
  (should (equal "Craig Jennings"
                 (calendar-sync--extract-cn
                  "ATTENDEE;CN=\"Craig Jennings\";PARTSTAT=ACCEPTED:mailto:c@test.com"))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--extract-cn-boundary-cn-at-end ()
  "CN as last parameter before colon."
  (should (equal "Bob"
                 (calendar-sync--extract-cn "ORGANIZER;CN=Bob:mailto:bob@test.com"))))

(ert-deftest test-calendar-sync--extract-cn-boundary-special-chars ()
  "CN with accented characters."
  (should (equal "José García"
                 (calendar-sync--extract-cn
                  "ATTENDEE;CN=José García;PARTSTAT=ACCEPTED:mailto:j@test.com"))))

;;; Error Cases

(ert-deftest test-calendar-sync--extract-cn-error-no-cn ()
  "Line without CN= returns nil."
  (should (null (calendar-sync--extract-cn
                 "ATTENDEE;PARTSTAT=ACCEPTED:mailto:c@test.com"))))

(provide 'test-calendar-sync--extract-cn)
;;; test-calendar-sync--extract-cn.el ends here
