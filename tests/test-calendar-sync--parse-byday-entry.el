;;; test-calendar-sync--parse-byday-entry.el --- Tests for calendar-sync--parse-byday-entry  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for parsing a single RRULE BYDAY entry ("2WE", "-1TU", "SU") into
;; an (ordinal . weekday-number) cons.  Ordinal is nil for a bare weekday.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--parse-byday-entry-normal-positive-ordinal ()
  "Normal: \"2WE\" parses to ordinal 2, Wednesday (3)."
  (should (equal (calendar-sync--parse-byday-entry "2WE") '(2 . 3))))

(ert-deftest test-calendar-sync--parse-byday-entry-normal-negative-ordinal ()
  "Normal: \"-1TU\" parses to ordinal -1, Tuesday (2)."
  (should (equal (calendar-sync--parse-byday-entry "-1TU") '(-1 . 2))))

(ert-deftest test-calendar-sync--parse-byday-entry-normal-bare-weekday ()
  "Normal: \"SU\" parses to nil ordinal, Sunday (7)."
  (should (equal (calendar-sync--parse-byday-entry "SU") '(nil . 7))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--parse-byday-entry-boundary-double-digit-ordinal ()
  "Boundary: \"53MO\" (yearly-scale ordinal) parses without truncation."
  (should (equal (calendar-sync--parse-byday-entry "53MO") '(53 . 1))))

;;; Error Cases

(ert-deftest test-calendar-sync--parse-byday-entry-error-garbage-nil ()
  "Error: an unrecognizable entry returns nil."
  (should (null (calendar-sync--parse-byday-entry "XX")))
  (should (null (calendar-sync--parse-byday-entry "")))
  (should (null (calendar-sync--parse-byday-entry nil))))

(provide 'test-calendar-sync--parse-byday-entry)
;;; test-calendar-sync--parse-byday-entry.el ends here
