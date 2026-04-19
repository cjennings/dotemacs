;;; test-transcription-format-entry.el --- Tests for format-transcription-entry -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--format-transcription-entry', which renders a single
;; transcription entry for display in the *Transcriptions* buffer.

;;; Code:

(require 'ert)

(defvar cj/custom-keymap (make-sparse-keymap))

(unless (fboundp 'notifications-notify)
  (defun notifications-notify (&rest _args) nil))

(require 'transcription-config)

(defun test-format-entry-face-at (string needle)
  "Return the `face' property at the start of NEEDLE within STRING."
  (let ((pos (string-match-p (regexp-quote needle) string)))
    (and pos (get-text-property pos 'face string))))

;;; Normal Cases

(ert-deftest test-format-entry-normal-running-uses-warning-face ()
  "Running status renders with the `warning' face."
  (let* ((entry '(proc "/tmp/a.m4a" (0 0 0 0) running))
         (s (cj/--format-transcription-entry entry)))
    (should (string-match-p "running" s))
    (should (eq 'warning (test-format-entry-face-at s "running")))))

(ert-deftest test-format-entry-normal-complete-uses-success-face ()
  "Complete status renders with the `success' face."
  (let* ((entry '(proc "/tmp/a.m4a" (0 0 0 0) complete))
         (s (cj/--format-transcription-entry entry)))
    (should (string-match-p "complete" s))
    (should (eq 'success (test-format-entry-face-at s "complete")))))

(ert-deftest test-format-entry-normal-error-uses-error-face ()
  "Error status renders with the `error' face."
  (let* ((entry '(proc "/tmp/a.m4a" (0 0 0 0) error))
         (s (cj/--format-transcription-entry entry)))
    (should (string-match-p "error" s))
    (should (eq 'error (test-format-entry-face-at s "error")))))

(ert-deftest test-format-entry-normal-shows-basename-only ()
  "Only the file's basename appears; the directory path is stripped."
  (let* ((entry '(proc "/deep/nested/path/track.m4a" (0 0 0 0) running))
         (s (cj/--format-transcription-entry entry)))
    (should (string-match-p "track\\.m4a" s))
    (should-not (string-match-p "/deep/nested" s))))

(ert-deftest test-format-entry-normal-includes-duration-in-parens ()
  "Rendering includes a parenthesised duration."
  (let* ((entry '(proc "/a.m4a" (0 0 0 0) running))
         (s (cj/--format-transcription-entry entry)))
    (should (string-match-p "([0-9]+:[0-9][0-9])" s))))

(ert-deftest test-format-entry-normal-ends-with-newline ()
  "Each rendered entry terminates with a newline for buffer concatenation."
  (let* ((entry '(proc "/a.m4a" (0 0 0 0) running))
         (s (cj/--format-transcription-entry entry)))
    (should (string-suffix-p "\n" s))))

(provide 'test-transcription-format-entry)
;;; test-transcription-format-entry.el ends here
