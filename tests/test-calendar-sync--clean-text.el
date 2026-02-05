;;; test-calendar-sync--clean-text.el --- Tests for clean-text composition  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--clean-text function.
;; Composes unescape-ics-text + strip-html, trims whitespace. Returns nil for nil.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--clean-text-normal-both-html-and-ics ()
  "Test text with both HTML tags and ICS escapes."
  (should (string= "Hello, World\nNext line"
                    (calendar-sync--clean-text "Hello\\, World<br>Next line"))))

(ert-deftest test-calendar-sync--clean-text-normal-pure-ics-escapes ()
  "Test text with only ICS escapes."
  (should (string= "a, b; c"
                    (calendar-sync--clean-text "a\\, b\\; c"))))

(ert-deftest test-calendar-sync--clean-text-normal-pure-html ()
  "Test text with only HTML."
  (should (string= "bold and italic"
                    (calendar-sync--clean-text "<b>bold</b> and <i>italic</i>"))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--clean-text-boundary-already-clean ()
  "Test already-clean text passes through unchanged."
  (should (string= "no escapes here"
                    (calendar-sync--clean-text "no escapes here"))))

(ert-deftest test-calendar-sync--clean-text-boundary-empty-string ()
  "Test empty string returns empty string."
  (should (string= "" (calendar-sync--clean-text ""))))

(ert-deftest test-calendar-sync--clean-text-boundary-whitespace-only ()
  "Test whitespace-only string returns empty after trim."
  (should (string= "" (calendar-sync--clean-text "   \n  \t  "))))

(ert-deftest test-calendar-sync--clean-text-boundary-leading-trailing-whitespace ()
  "Test leading/trailing whitespace is trimmed."
  (should (string= "content"
                    (calendar-sync--clean-text "  content  "))))

;;; Error Cases

(ert-deftest test-calendar-sync--clean-text-error-nil-input ()
  "Test nil input returns nil."
  (should (null (calendar-sync--clean-text nil))))

(provide 'test-calendar-sync--clean-text)
;;; test-calendar-sync--clean-text.el ends here
