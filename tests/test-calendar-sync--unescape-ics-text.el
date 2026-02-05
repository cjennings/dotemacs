;;; test-calendar-sync--unescape-ics-text.el --- Tests for ICS text unescaping  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--unescape-ics-text function.
;; RFC 5545 defines escape sequences: \n→newline, \,→comma, \\→backslash, \;→semicolon.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--unescape-ics-text-normal-newline ()
  "Test \\n escape is converted to actual newline."
  (should (string= "line1\nline2"
                    (calendar-sync--unescape-ics-text "line1\\nline2"))))

(ert-deftest test-calendar-sync--unescape-ics-text-normal-comma ()
  "Test \\, escape is converted to comma."
  (should (string= "one, two"
                    (calendar-sync--unescape-ics-text "one\\, two"))))

(ert-deftest test-calendar-sync--unescape-ics-text-normal-backslash ()
  "Test \\\\ escape is converted to single backslash."
  (should (string= "path\\file"
                    (calendar-sync--unescape-ics-text "path\\\\file"))))

(ert-deftest test-calendar-sync--unescape-ics-text-normal-semicolon ()
  "Test \\; escape is converted to semicolon."
  (should (string= "a;b"
                    (calendar-sync--unescape-ics-text "a\\;b"))))

(ert-deftest test-calendar-sync--unescape-ics-text-normal-mixed ()
  "Test multiple different escapes in one string."
  (should (string= "Hello, World\nPath\\to;file"
                    (calendar-sync--unescape-ics-text "Hello\\, World\\nPath\\\\to\\;file"))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--unescape-ics-text-boundary-empty-string ()
  "Test empty string returns empty string."
  (should (string= "" (calendar-sync--unescape-ics-text ""))))

(ert-deftest test-calendar-sync--unescape-ics-text-boundary-no-escapes ()
  "Test string with no escapes passes through unchanged."
  (should (string= "plain text"
                    (calendar-sync--unescape-ics-text "plain text"))))

(ert-deftest test-calendar-sync--unescape-ics-text-boundary-escape-at-start ()
  "Test escape sequence at string start."
  (should (string= "\nfoo"
                    (calendar-sync--unescape-ics-text "\\nfoo"))))

(ert-deftest test-calendar-sync--unescape-ics-text-boundary-escape-at-end ()
  "Test escape sequence at string end."
  (should (string= "foo\n"
                    (calendar-sync--unescape-ics-text "foo\\n"))))

(ert-deftest test-calendar-sync--unescape-ics-text-boundary-consecutive-escapes ()
  "Test consecutive escape sequences."
  (should (string= "\n\n"
                    (calendar-sync--unescape-ics-text "\\n\\n"))))

(ert-deftest test-calendar-sync--unescape-ics-text-boundary-only-escapes ()
  "Test string composed entirely of escapes."
  (should (string= ",;\n\\"
                    (calendar-sync--unescape-ics-text "\\,\\;\\n\\\\"))))

;;; Error Cases

(ert-deftest test-calendar-sync--unescape-ics-text-error-nil-input ()
  "Test nil input returns nil."
  (should (null (calendar-sync--unescape-ics-text nil))))

(provide 'test-calendar-sync--unescape-ics-text)
;;; test-calendar-sync--unescape-ics-text.el ends here
