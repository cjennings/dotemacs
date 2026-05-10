;;; test-cj-org-text-sanitize.el --- Tests for the Org-safe text sanitizers -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/org-sanitize-body-text', `cj/org-sanitize-property-value',
;; and `cj/org-sanitize-heading' in cj-org-text.el.  Pure string helpers
;; for safely composing Org content from external sources (calendar
;; bodies, web-clipped HTML, mail subjects, AI transcripts).  Originally
;; lived in calendar-sync.el under `calendar-sync--sanitize-*' names.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'cj-org-text-lib)

;;; cj/org-sanitize-body-text -- Normal cases

(ert-deftest test-cj-org-sanitize-body-single-asterisk ()
  "Normal: single leading asterisk replaced with dash."
  (should (equal "- item one" (cj/org-sanitize-body-text "* item one"))))

(ert-deftest test-cj-org-sanitize-body-double-asterisk ()
  "Normal: double leading asterisks replaced with double dashes."
  (should (equal "-- sub-item" (cj/org-sanitize-body-text "** sub-item"))))

(ert-deftest test-cj-org-sanitize-body-triple-asterisk ()
  "Normal: triple leading asterisks replaced with triple dashes."
  (should (equal "--- deep item" (cj/org-sanitize-body-text "*** deep item"))))

(ert-deftest test-cj-org-sanitize-body-multiline ()
  "Normal: multiple lines with leading stars all get sanitized."
  (let ((input "Format:\n* What did you do yesterday?\n* What are you doing today?\n* Is anything in your way?")
        (expected "Format:\n- What did you do yesterday?\n- What are you doing today?\n- Is anything in your way?"))
    (should (equal expected (cj/org-sanitize-body-text input)))))

(ert-deftest test-cj-org-sanitize-body-mixed-lines ()
  "Normal: only lines starting with asterisks change."
  (let ((input "Normal line\n* Bullet line\nAnother normal line"))
    (should (equal "Normal line\n- Bullet line\nAnother normal line"
                   (cj/org-sanitize-body-text input)))))

(ert-deftest test-cj-org-sanitize-body-mixed-levels ()
  "Normal: lines with different asterisk counts each handled independently."
  (let ((input "* Top\n** Middle\n*** Bottom"))
    (should (equal "- Top\n-- Middle\n--- Bottom"
                   (cj/org-sanitize-body-text input)))))

;;; cj/org-sanitize-body-text -- Boundary cases

(ert-deftest test-cj-org-sanitize-body-nil-input ()
  "Boundary: nil input returns nil."
  (should (null (cj/org-sanitize-body-text nil))))

(ert-deftest test-cj-org-sanitize-body-empty-string ()
  "Boundary: empty string returns empty string."
  (should (equal "" (cj/org-sanitize-body-text ""))))

(ert-deftest test-cj-org-sanitize-body-no-asterisks ()
  "Boundary: text without leading asterisks is returned unchanged."
  (let ((input "Just a normal description\nwith multiple lines"))
    (should (equal input (cj/org-sanitize-body-text input)))))

(ert-deftest test-cj-org-sanitize-body-asterisk-mid-line ()
  "Boundary: asterisks not at line start are left alone."
  (should (equal "Use * for emphasis" (cj/org-sanitize-body-text "Use * for emphasis"))))

(ert-deftest test-cj-org-sanitize-body-asterisk-no-space ()
  "Boundary: asterisk at line start without trailing space is not a heading -- left alone."
  (should (equal "*bold text*" (cj/org-sanitize-body-text "*bold text*"))))

(ert-deftest test-cj-org-sanitize-body-asterisk-only ()
  "Boundary: lone asterisk with space at start of line is sanitized."
  (should (equal "- " (cj/org-sanitize-body-text "* "))))

;;; cj/org-sanitize-heading

(ert-deftest test-cj-org-sanitize-heading-flattens-newlines ()
  "Normal: heading text stays on one Org heading line."
  (should (equal "Planning Agenda"
                 (cj/org-sanitize-heading "Planning\nAgenda"))))

(ert-deftest test-cj-org-sanitize-heading-replaces-leading-stars ()
  "Normal: heading text doesn't start with Org heading stars."
  (should (equal "- Planning -- Hidden"
                 (cj/org-sanitize-heading "* Planning\n** Hidden"))))

(ert-deftest test-cj-org-sanitize-heading-nil-input ()
  "Boundary: nil input returns nil."
  (should (null (cj/org-sanitize-heading nil))))

;;; cj/org-sanitize-property-value

(ert-deftest test-cj-org-sanitize-property-value-flattens-structure ()
  "Normal: property values do not create extra property drawer lines."
  (should (equal "Room 1 :END: * Not a heading"
                 (cj/org-sanitize-property-value
                  "Room 1\n:END:\n* Not a heading"))))

(ert-deftest test-cj-org-sanitize-property-value-trims-and-collapses ()
  "Normal: property values are compact single-line values."
  (should (equal "alpha beta gamma"
                 (cj/org-sanitize-property-value
                  " alpha\t beta\n\n gamma "))))

(ert-deftest test-cj-org-sanitize-property-value-nil-input ()
  "Boundary: nil input returns nil."
  (should (null (cj/org-sanitize-property-value nil))))

(provide 'test-cj-org-text-sanitize)
;;; test-cj-org-text-sanitize.el ends here
