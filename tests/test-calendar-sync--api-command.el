;;; test-calendar-sync--api-command.el --- Tests for API command builder  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the Google Calendar API fetch path's pure helpers:
;; `calendar-sync--api-script' (resolves the helper script path) and
;; `calendar-sync--api-command' (builds the make-process argument list).
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; calendar-sync--api-script

(ert-deftest test-calendar-sync--api-script-normal-resolves-to-helper ()
  "Normal: the script path ends with the helper filename and is absolute."
  (let ((path (calendar-sync--api-script)))
    (should (file-name-absolute-p path))
    (should (string-suffix-p "scripts/calendar_sync_api.py" path))))

(ert-deftest test-calendar-sync--api-script-normal-no-dotdot ()
  "Normal: the resolved path is collapsed (no literal ../ segment)."
  (let ((path (calendar-sync--api-script)))
    (should-not (string-match-p "\\.\\./" path))))

;;; calendar-sync--api-command — Normal

(ert-deftest test-calendar-sync--api-command-normal-structure ()
  "Normal: command starts with the python binary + script, then the flags."
  (let ((calendar-sync-python-command "python3")
        (calendar-sync-past-months 3)
        (calendar-sync-future-months 12)
        (calendar-sync-skip-declined t))
    (let ((cmd (calendar-sync--api-command "work" "primary" "/tmp/out.org")))
      (should (equal (nth 0 cmd) "python3"))
      (should (string-suffix-p "calendar_sync_api.py" (nth 1 cmd)))
      (should (member "--account" cmd))
      (should (member "work" cmd))
      (should (member "--calendar-id" cmd))
      (should (member "primary" cmd))
      (should (member "--output" cmd))
      (should (member "/tmp/out.org" cmd)))))

(ert-deftest test-calendar-sync--api-command-normal-flag-pairing ()
  "Normal: each value immediately follows its flag."
  (let ((calendar-sync-python-command "python3")
        (calendar-sync-skip-declined t))
    (let ((cmd (calendar-sync--api-command "personal" "abc123" "/tmp/gcal.org")))
      (should (equal "personal" (nth (1+ (cl-position "--account" cmd :test #'equal)) cmd)))
      (should (equal "abc123" (nth (1+ (cl-position "--calendar-id" cmd :test #'equal)) cmd)))
      (should (equal "/tmp/gcal.org" (nth (1+ (cl-position "--output" cmd :test #'equal)) cmd))))))

(ert-deftest test-calendar-sync--api-command-normal-window-from-defvars ()
  "Normal: past/future month flags reflect the configured defvars."
  (let ((calendar-sync-python-command "python3")
        (calendar-sync-past-months 6)
        (calendar-sync-future-months 9)
        (calendar-sync-skip-declined t))
    (let ((cmd (calendar-sync--api-command "work" "primary" "/tmp/out.org")))
      (should (equal "6" (nth (1+ (cl-position "--past-months" cmd :test #'equal)) cmd)))
      (should (equal "9" (nth (1+ (cl-position "--future-months" cmd :test #'equal)) cmd))))))

(ert-deftest test-calendar-sync--api-command-normal-honors-python-command ()
  "Normal: a custom python command is used as argv[0]."
  (let ((calendar-sync-python-command "/usr/bin/python3.14")
        (calendar-sync-skip-declined t))
    (let ((cmd (calendar-sync--api-command "work" "primary" "/tmp/out.org")))
      (should (equal "/usr/bin/python3.14" (nth 0 cmd))))))

;;; calendar-sync--api-command — Boundary (declined toggle)

(ert-deftest test-calendar-sync--api-command-boundary-skip-declined-omits-flag ()
  "Boundary: with skip-declined on (default), --keep-declined is absent.
The helper filters declines by default, matching the .ics path."
  (let ((calendar-sync-python-command "python3")
        (calendar-sync-skip-declined t))
    (let ((cmd (calendar-sync--api-command "work" "primary" "/tmp/out.org")))
      (should-not (member "--keep-declined" cmd)))))

(ert-deftest test-calendar-sync--api-command-boundary-keep-declined-adds-flag ()
  "Boundary: with skip-declined nil, --keep-declined is passed through.
This keeps the API path honoring the same toggle as the parser path."
  (let ((calendar-sync-python-command "python3")
        (calendar-sync-skip-declined nil))
    (let ((cmd (calendar-sync--api-command "work" "primary" "/tmp/out.org")))
      (should (member "--keep-declined" cmd)))))

;;; calendar-sync--api-command — Error

(ert-deftest test-calendar-sync--api-command-error-returns-string-list ()
  "Error: every element of the command list is a string (make-process safe)."
  (let ((calendar-sync-python-command "python3")
        (calendar-sync-skip-declined nil))
    (let ((cmd (calendar-sync--api-command "work" "primary" "/tmp/out.org")))
      (should (cl-every #'stringp cmd)))))

(provide 'test-calendar-sync--api-command)
;;; test-calendar-sync--api-command.el ends here
