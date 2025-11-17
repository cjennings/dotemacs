;;; test-calendar-sync--normalize-line-endings.el --- Tests for calendar-sync--normalize-line-endings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--normalize-line-endings function.
;; Tests conversion of various line ending formats to Unix LF-only format.
;; Covers Normal, Boundary, and Error cases.
;;
;; The iCalendar format (RFC 5545) uses CRLF line endings (\r\n),
;; but Emacs and org-mode expect LF only (\n). This function ensures
;; consistent line endings throughout the parsing pipeline.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--normalize-line-endings-normal-crlf-to-lf ()
  "Test that CRLF line endings are converted to LF only.

Input: String with \\r\\n (Windows/DOS line endings)
Expected: String with \\n only (Unix line endings)"
  (let* ((input "line1\r\nline2\r\nline3\r\n")
         (expected "line1\nline2\nline3\n")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-normal-lf-unchanged ()
  "Test that LF-only content is returned unchanged.

Input: String with \\n only (already Unix format)
Expected: Same string (no modification needed)"
  (let* ((input "line1\nline2\nline3\n")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= input result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-normal-mixed-endings ()
  "Test that mixed line endings are normalized to LF only.

Input: String with both \\r\\n (CRLF) and \\n (LF)
Expected: String with \\n only everywhere"
  (let* ((input "line1\r\nline2\nline3\r\nline4\n")
         (expected "line1\nline2\nline3\nline4\n")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-normal-ics-vevent-block ()
  "Test normalization of realistic iCalendar VEVENT block with CRLF.

Input: VEVENT block with CRLF line endings (as per RFC 5545)
Expected: Same structure with LF only"
  (let* ((input "BEGIN:VEVENT\r\nSUMMARY:Test Event\r\nDTSTART:20251116T140000Z\r\nEND:VEVENT\r\n")
         (expected "BEGIN:VEVENT\nSUMMARY:Test Event\nDTSTART:20251116T140000Z\nEND:VEVENT\n")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-empty-string ()
  "Test that empty string is handled correctly.

Input: Empty string
Expected: Empty string (no crash)"
  (let ((result (calendar-sync--normalize-line-endings "")))
    (should (string= "" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-no-line-endings ()
  "Test that string with no line endings is unchanged.

Input: Plain text with no \\r or \\n
Expected: Same string unchanged"
  (let* ((input "no line endings here")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= input result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-only-cr ()
  "Test that bare CR characters (old Mac format) are removed.

Input: String with \\r only (classic Mac OS line endings)
Expected: String with \\r removed (results in run-together text)"
  (let* ((input "line1\rline2\rline3\r")
         (expected "line1line2line3")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-cr-in-middle ()
  "Test that CR characters in middle of content are removed.

Input: String with \\r not followed by \\n (unusual but possible)
Expected: All \\r removed regardless of position"
  (let* ((input "line1\r\ntext\rwith\rmiddle\r\nline2")
         (expected "line1\ntextwithmiddle\nline2")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-multiple-cr ()
  "Test that multiple consecutive CR characters are all removed.

Input: String with \\r\\r or \\r\\r\\n sequences
Expected: All \\r characters removed"
  (let* ((input "line1\r\r\nline2\r\r\r\nline3")
         (expected "line1\nline2\nline3")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-single-line ()
  "Test normalization of single line with trailing CRLF.

Input: Single line of text ending with \\r\\n
Expected: Single line ending with \\n"
  (let* ((input "single line\r\n")
         (expected "single line\n")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-only-line-endings ()
  "Test string containing only line ending characters.

Input: String of only \\r\\n sequences
Expected: String of only \\n (CR stripped)"
  (let* ((input "\r\n\r\n\r\n")
         (expected "\n\n\n")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-unicode-content ()
  "Test normalization preserves Unicode characters.

Input: String with Unicode and CRLF line endings
Expected: Unicode preserved, only CR removed"
  (let* ((input "emoji 🎉\r\nchinese 中文\r\narabic العربية\r\n")
         (expected "emoji 🎉\nchinese 中文\narabic العربية\n")
         (result (calendar-sync--normalize-line-endings input)))
    (should (string= expected result))
    (should-not (string-match-p "\r" result))))

(ert-deftest test-calendar-sync--normalize-line-endings-boundary-very-long-string ()
  "Test normalization of large string with many line endings.

Input: String with 1000 lines with CRLF
Expected: Same content with LF only, performance acceptable"
  (let* ((line "This is line content with some text\r\n")
         (input (apply #'concat (make-list 1000 line)))
         (result (calendar-sync--normalize-line-endings input)))
    (should (= (length input) (+ (length result) 1000))) ; 1000 \r removed
    (should-not (string-match-p "\r" result))
    (should (string-match-p "^This is line content" result))))

;;; Error Cases

(ert-deftest test-calendar-sync--normalize-line-endings-error-nil-input ()
  "Test that nil input is handled gracefully.

Input: nil
Expected: nil (defensive programming, no crash)"
  (let ((result (calendar-sync--normalize-line-endings nil)))
    (should (null result))))

(ert-deftest test-calendar-sync--normalize-line-endings-error-non-string-input ()
  "Test that non-string input is returned unchanged.

Input: Integer (wrong type)
Expected: Same value returned (defensive, don't crash)"
  (let ((result (calendar-sync--normalize-line-endings 42)))
    (should (= 42 result))))

(ert-deftest test-calendar-sync--normalize-line-endings-error-symbol-input ()
  "Test that symbol input is handled gracefully.

Input: Symbol (wrong type)
Expected: Symbol returned unchanged"
  (let ((result (calendar-sync--normalize-line-endings 'some-symbol)))
    (should (eq 'some-symbol result))))

(provide 'test-calendar-sync--normalize-line-endings)
;;; test-calendar-sync--normalize-line-endings.el ends here
