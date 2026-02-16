;;; test-org-capture-config-date-prefix.el --- Tests for cj/org-capture--date-prefix -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cj/org-capture--date-prefix function from org-capture-config.el.
;;
;; Pure function: takes an org timestamp string, returns "YY-MM-DD: " prefix
;; or nil if the timestamp is unparseable. Extracted from
;; cj/org-capture-format-event-headline for testability.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

;;; Normal Cases

(ert-deftest test-org-capture-config-date-prefix-normal-standard-timestamp ()
  "Standard org timestamp should produce YY-MM-DD prefix."
  (should (equal (cj/org-capture--date-prefix "<2026-02-15 Sun>")
                 "26-02-15: ")))

(ert-deftest test-org-capture-config-date-prefix-normal-timestamp-with-time ()
  "Timestamp with time component should still produce date-only prefix."
  (should (equal (cj/org-capture--date-prefix "<2026-02-15 Sun 14:30>")
                 "26-02-15: ")))

(ert-deftest test-org-capture-config-date-prefix-normal-weekday-irrelevant ()
  "Different weekday abbreviations should not affect the date prefix."
  (should (equal (cj/org-capture--date-prefix "<2026-03-01 Mon>")
                 "26-03-01: ")))

(ert-deftest test-org-capture-config-date-prefix-normal-year-2000 ()
  "Year 2000 should produce 00 prefix."
  (should (equal (cj/org-capture--date-prefix "<2000-06-15 Thu>")
                 "00-06-15: ")))

(ert-deftest test-org-capture-config-date-prefix-normal-end-of-year ()
  "December 31 should format correctly."
  (should (equal (cj/org-capture--date-prefix "<2026-12-31 Wed>")
                 "26-12-31: ")))

(ert-deftest test-org-capture-config-date-prefix-normal-start-of-year ()
  "January 1 should format correctly."
  (should (equal (cj/org-capture--date-prefix "<2026-01-01 Thu>")
                 "26-01-01: ")))

;;; Boundary Cases

(ert-deftest test-org-capture-config-date-prefix-boundary-no-day-name ()
  "Timestamp without day name should still parse."
  (should (equal (cj/org-capture--date-prefix "<2026-02-15>")
                 "26-02-15: ")))

(ert-deftest test-org-capture-config-date-prefix-boundary-single-digit-month-day ()
  "Single-digit month and day should be zero-padded."
  (should (equal (cj/org-capture--date-prefix "<2026-01-05 Mon>")
                 "26-01-05: ")))

(ert-deftest test-org-capture-config-date-prefix-boundary-year-wraps-at-100 ()
  "Year 2099 should produce 99; year 2100 should produce 00."
  (should (equal (cj/org-capture--date-prefix "<2099-06-15 Sun>")
                 "99-06-15: "))
  (should (equal (cj/org-capture--date-prefix "<2100-06-15 Mon>")
                 "00-06-15: ")))

(ert-deftest test-org-capture-config-date-prefix-boundary-timestamp-with-range ()
  "Timestamp with time range should still extract the date."
  (should (equal (cj/org-capture--date-prefix "<2026-02-15 Sun 09:00-17:00>")
                 "26-02-15: ")))

;;; Error Cases

(ert-deftest test-org-capture-config-date-prefix-error-nil-returns-nil ()
  "Nil input should return nil."
  (should (null (cj/org-capture--date-prefix nil))))

(ert-deftest test-org-capture-config-date-prefix-error-empty-string-returns-nil ()
  "Empty string should return nil."
  (should (null (cj/org-capture--date-prefix ""))))

(ert-deftest test-org-capture-config-date-prefix-error-garbage-string-returns-nil ()
  "Non-timestamp string should return nil."
  (should (null (cj/org-capture--date-prefix "not a timestamp"))))

(provide 'test-org-capture-config-date-prefix)
;;; test-org-capture-config-date-prefix.el ends here
