;;; test-config-utilities--format-validation-report-section.el --- Tests for cj/--format-validation-report-section -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--format-validation-report-section'.  Pure helper that
;; takes a FILE path and a list of invalid-entry tuples and returns
;; the per-file org-formatted string.  Empty list yields a "No
;; invalid timestamps found." line; non-empty produces a bulleted
;; list with file: links, Property/Type, and the timestamp string.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)

(ert-deftest test-config-utilities-format-validation-no-entries-says-none-found ()
  "Boundary: an empty entries list produces the \"No invalid\" line."
  (let ((text (cj/--format-validation-report-section "/tmp/foo.org" nil)))
    (should (string-match-p "^\\* /tmp/foo\\.org$" text))
    (should (string-match-p "^No invalid timestamps found\\.$" text))))

(ert-deftest test-config-utilities-format-validation-single-entry-formats-bullet ()
  "Normal: one entry produces a file-link bullet plus property and timestamp lines."
  (let ((text (cj/--format-validation-report-section
               "/tmp/foo.org"
               '(("/tmp/foo.org" 42 "Bad heading" "DEADLINE" "<2030-13-45>")))))
    (should (string-match-p
             "- \\[\\[file:/tmp/foo\\.org::42\\]\\[Bad heading\\]\\]"
             text))
    (should (string-match-p "Property/Type: DEADLINE" text))
    (should (string-match-p "Invalid timestamp: \"<2030-13-45>\"" text))))

(ert-deftest test-config-utilities-format-validation-multiple-entries-preserves-order ()
  "Normal: multiple entries are formatted in input order."
  (let ((text (cj/--format-validation-report-section
               "/tmp/foo.org"
               '(("/tmp/foo.org" 1 "First" "DEADLINE" "<bad-1>")
                 ("/tmp/foo.org" 2 "Second" "SCHEDULED" "<bad-2>")
                 ("/tmp/foo.org" 3 "Third" "inline timestamp" "<bad-3>")))))
    (should (string-match-p "<bad-1>" text))
    (should (string-match-p "<bad-2>" text))
    (should (string-match-p "<bad-3>" text))
    ;; Order is preserved.
    (should (< (string-match "<bad-1>" text)
               (string-match "<bad-2>" text)))
    (should (< (string-match "<bad-2>" text)
               (string-match "<bad-3>" text)))))

(ert-deftest test-config-utilities-format-validation-section-ends-with-blank-line ()
  "Boundary: every section ends with a trailing blank line so successive
file sections are visually separated in the report."
  (let ((empty-section (cj/--format-validation-report-section "/x" nil))
        (full-section (cj/--format-validation-report-section
                       "/x" '(("/x" 1 "h" "DEADLINE" "<bad>")))))
    (should (string-suffix-p "\n\n" empty-section))
    (should (string-suffix-p "\n\n" full-section))))

(provide 'test-config-utilities--format-validation-report-section)
;;; test-config-utilities--format-validation-report-section.el ends here
