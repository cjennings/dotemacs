;;; test-calendar-sync--sanitize-org-body.el --- Tests for org body sanitization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--sanitize-org-body.
;; Ensures description text with org-special syntax (leading asterisks)
;; is escaped to prevent corruption of the org file structure.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--sanitize-org-body-normal-single-asterisk ()
  "Single leading asterisk replaced with dash."
  (should (equal "- item one" (calendar-sync--sanitize-org-body "* item one"))))

(ert-deftest test-calendar-sync--sanitize-org-body-normal-double-asterisk ()
  "Double leading asterisks replaced with double dashes."
  (should (equal "-- sub-item" (calendar-sync--sanitize-org-body "** sub-item"))))

(ert-deftest test-calendar-sync--sanitize-org-body-normal-triple-asterisk ()
  "Triple leading asterisks replaced with triple dashes."
  (should (equal "--- deep item" (calendar-sync--sanitize-org-body "*** deep item"))))

(ert-deftest test-calendar-sync--sanitize-org-body-normal-multiline ()
  "Multiple lines with asterisks all get sanitized."
  (let ((input "Format:\n* What did you do yesterday?\n* What are you doing today?\n* Is anything in your way?")
        (expected "Format:\n- What did you do yesterday?\n- What are you doing today?\n- Is anything in your way?"))
    (should (equal expected (calendar-sync--sanitize-org-body input)))))

(ert-deftest test-calendar-sync--sanitize-org-body-normal-mixed-lines ()
  "Only lines starting with asterisks are changed."
  (let ((input "Normal line\n* Bullet line\nAnother normal line"))
        (should (equal "Normal line\n- Bullet line\nAnother normal line"
                       (calendar-sync--sanitize-org-body input)))))

(ert-deftest test-calendar-sync--sanitize-org-body-normal-mixed-levels ()
  "Lines with different asterisk counts are each handled."
  (let ((input "* Top\n** Middle\n*** Bottom"))
    (should (equal "- Top\n-- Middle\n--- Bottom"
                   (calendar-sync--sanitize-org-body input)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--sanitize-org-body-boundary-nil-input ()
  "Nil input returns nil."
  (should (null (calendar-sync--sanitize-org-body nil))))

(ert-deftest test-calendar-sync--sanitize-org-body-boundary-empty-string ()
  "Empty string returns empty string."
  (should (equal "" (calendar-sync--sanitize-org-body ""))))

(ert-deftest test-calendar-sync--sanitize-org-body-boundary-no-asterisks ()
  "Text without leading asterisks is returned unchanged."
  (let ((input "Just a normal description\nwith multiple lines"))
    (should (equal input (calendar-sync--sanitize-org-body input)))))

(ert-deftest test-calendar-sync--sanitize-org-body-boundary-asterisk-mid-line ()
  "Asterisks not at line start are left alone."
  (should (equal "Use * for emphasis" (calendar-sync--sanitize-org-body "Use * for emphasis"))))

(ert-deftest test-calendar-sync--sanitize-org-body-boundary-asterisk-no-space ()
  "Asterisk at line start without trailing space is not a heading — left alone."
  (should (equal "*bold text*" (calendar-sync--sanitize-org-body "*bold text*"))))

(ert-deftest test-calendar-sync--sanitize-org-body-boundary-asterisk-only ()
  "Lone asterisk with space at start of line is sanitized."
  (should (equal "- " (calendar-sync--sanitize-org-body "* "))))

;;; Heading and Property Sanitizers

(ert-deftest test-calendar-sync--sanitize-org-heading-flattens-newlines ()
  "Heading text should stay on one Org heading line."
  (should (equal "Planning Agenda"
                 (calendar-sync--sanitize-org-heading "Planning\nAgenda"))))

(ert-deftest test-calendar-sync--sanitize-org-heading-replaces-leading-stars ()
  "Heading text should not start with Org heading stars."
  (should (equal "- Planning -- Hidden"
                 (calendar-sync--sanitize-org-heading "* Planning\n** Hidden"))))

(ert-deftest test-calendar-sync--sanitize-org-property-value-flattens-structure ()
  "Property values should not create extra property drawer lines."
  (should (equal "Room 1 :END: * Not a heading"
                 (calendar-sync--sanitize-org-property-value
                  "Room 1\n:END:\n* Not a heading"))))

(ert-deftest test-calendar-sync--sanitize-org-property-value-trims-and-collapses ()
  "Property values should be compact single-line values."
  (should (equal "alpha beta gamma"
                 (calendar-sync--sanitize-org-property-value
                  " alpha\t beta\n\n gamma "))))

(provide 'test-calendar-sync--sanitize-org-body)
;;; test-calendar-sync--sanitize-org-body.el ends here
