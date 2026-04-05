;;; test-calendar-sync--unfold-continuation.el --- Tests for RFC 5545 line unfolding -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--unfold-continuation.
;; Unfolds RFC 5545 continuation lines (lines starting with space/tab after newline).
;; Returns (unfolded-value . new-position) cons.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--unfold-continuation-normal-single-continuation ()
  "Single continuation line is appended to value."
  ;; Simulate how get-property calls unfold-continuation: match-end is
  ;; the position right after the matched value on the first line.
  ;; "DESCRIPTION:First part" is 22 chars, so match-end for the value = 22.
  ;; The \n at position 22 starts the continuation check.
  (let* ((text "DESCRIPTION:First part\n Second part\nNEXT:value"))
    ;; Use get-property which exercises unfold-continuation with correct positions
    (should (equal "First partSecond part"
                   (calendar-sync--get-property text "DESCRIPTION")))))

(ert-deftest test-calendar-sync--unfold-continuation-normal-multiple-continuations ()
  "Multiple continuation lines are all appended."
  (let* ((text "DESC:Line1\n Line2\n Line3\nNEXT:x"))
    (should (equal "Line1Line2Line3"
                   (calendar-sync--get-property text "DESC")))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--unfold-continuation-boundary-no-continuation ()
  "No continuation lines returns value unchanged."
  (let* ((text "SUMMARY:Test\nDTSTART:20260315"))
    (should (equal "Test" (calendar-sync--get-property text "SUMMARY")))))

(ert-deftest test-calendar-sync--unfold-continuation-boundary-tab-continuation ()
  "Tab-indented continuation lines are also unfolded."
  (let* ((text "DESC:Start\n\tTabbed\nNEXT:x"))
    (should (equal "StartTabbed" (calendar-sync--get-property text "DESC")))))

;;; Error Cases

(ert-deftest test-calendar-sync--unfold-continuation-error-start-at-end ()
  "Value at end of text with no continuation returns unchanged."
  (let* ((text "SUMMARY:Test"))
    (should (equal "Test" (calendar-sync--get-property text "SUMMARY")))))

(provide 'test-calendar-sync--unfold-continuation)
;;; test-calendar-sync--unfold-continuation.el ends here
