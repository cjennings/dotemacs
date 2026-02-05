;;; test-calendar-sync--strip-html.el --- Tests for HTML stripping  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--strip-html function.
;; Converts <br>/<br/> to newline, strips all other tags, decodes HTML entities.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--strip-html-normal-br-to-newline ()
  "Test <br> converted to newline."
  (should (string= "line1\nline2"
                    (calendar-sync--strip-html "line1<br>line2"))))

(ert-deftest test-calendar-sync--strip-html-normal-br-self-closing ()
  "Test <br/> converted to newline."
  (should (string= "line1\nline2"
                    (calendar-sync--strip-html "line1<br/>line2"))))

(ert-deftest test-calendar-sync--strip-html-normal-br-space-self-closing ()
  "Test <br /> converted to newline."
  (should (string= "line1\nline2"
                    (calendar-sync--strip-html "line1<br />line2"))))

(ert-deftest test-calendar-sync--strip-html-normal-strip-p-tags ()
  "Test <p> and </p> tags are stripped."
  (should (string= "paragraph text"
                    (calendar-sync--strip-html "<p>paragraph text</p>"))))

(ert-deftest test-calendar-sync--strip-html-normal-strip-bold ()
  "Test <b> and </b> tags are stripped."
  (should (string= "bold text"
                    (calendar-sync--strip-html "<b>bold text</b>"))))

(ert-deftest test-calendar-sync--strip-html-normal-combined-tags ()
  "Test mixed tags are handled."
  (should (string= "Hello\nWorld"
                    (calendar-sync--strip-html "<p>Hello</p><br><b>World</b>"))))

(ert-deftest test-calendar-sync--strip-html-normal-entity-amp ()
  "Test &amp; decoded to &."
  (should (string= "A & B"
                    (calendar-sync--strip-html "A &amp; B"))))

(ert-deftest test-calendar-sync--strip-html-normal-entity-lt ()
  "Test &lt; decoded to <."
  (should (string= "a < b"
                    (calendar-sync--strip-html "a &lt; b"))))

(ert-deftest test-calendar-sync--strip-html-normal-entity-gt ()
  "Test &gt; decoded to >."
  (should (string= "a > b"
                    (calendar-sync--strip-html "a &gt; b"))))

(ert-deftest test-calendar-sync--strip-html-normal-entity-quot ()
  "Test &quot; decoded to double quote."
  (should (string= "say \"hello\""
                    (calendar-sync--strip-html "say &quot;hello&quot;"))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--strip-html-boundary-empty-string ()
  "Test empty string returns empty string."
  (should (string= "" (calendar-sync--strip-html ""))))

(ert-deftest test-calendar-sync--strip-html-boundary-no-html ()
  "Test plain text passes through unchanged."
  (should (string= "just plain text"
                    (calendar-sync--strip-html "just plain text"))))

(ert-deftest test-calendar-sync--strip-html-boundary-only-tags ()
  "Test string of only tags returns empty."
  (should (string= ""
                    (calendar-sync--strip-html "<p><b></b></p>"))))

(ert-deftest test-calendar-sync--strip-html-boundary-multiple-br ()
  "Test multiple consecutive <br> collapse."
  (should (string-match-p "^line1\n+line2$"
                           (calendar-sync--strip-html "line1<br><br><br>line2"))))

(ert-deftest test-calendar-sync--strip-html-boundary-nested-tags ()
  "Test nested tags are stripped correctly."
  (should (string= "nested text"
                    (calendar-sync--strip-html "<div><p><b>nested text</b></p></div>"))))

;;; Error Cases

(ert-deftest test-calendar-sync--strip-html-error-nil-input ()
  "Test nil input returns nil."
  (should (null (calendar-sync--strip-html nil))))

(provide 'test-calendar-sync--strip-html)
;;; test-calendar-sync--strip-html.el ends here
