;;; test-calendar-sync--extract-email.el --- Tests for email extraction -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--extract-email.
;; Extracts email address from mailto: values in iCal property lines.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--extract-email-normal-standard ()
  "Extracts email from standard mailto: value."
  (should (equal "craig@test.com"
                 (calendar-sync--extract-email
                  "ATTENDEE;CN=Craig:mailto:craig@test.com"))))

(ert-deftest test-calendar-sync--extract-email-normal-organizer ()
  "Works on ORGANIZER lines too."
  (should (equal "boss@corp.com"
                 (calendar-sync--extract-email
                  "ORGANIZER;CN=Boss:mailto:boss@corp.com"))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--extract-email-boundary-plus-addressing ()
  "Handles plus-addressed emails."
  (should (equal "user+tag@test.com"
                 (calendar-sync--extract-email
                  "ATTENDEE:mailto:user+tag@test.com"))))

;;; Error Cases

(ert-deftest test-calendar-sync--extract-email-error-no-mailto ()
  "Line without mailto: returns nil."
  (should (null (calendar-sync--extract-email "ATTENDEE;CN=Craig:urn:invalid"))))

(provide 'test-calendar-sync--extract-email)
;;; test-calendar-sync--extract-email.el ends here
