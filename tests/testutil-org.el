;;; testutil-org.el --- Test utilities for org-mode tests -*- lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Utilities for testing org-mode related modules.
;; Provides dynamic timestamp generation for deterministic date-sensitive tests.
;; No hardcoded dates — all timestamps generated relative to current time.
;;
;; See also: testutil-calendar-sync.el for iCal-specific timestamp utilities.

;;; Code:

(defun test-org-timestamp-days-ago (days)
  "Generate org timestamp string for DAYS ago.
Returns string like \"<2026-02-10 Tue>\"."
  (format-time-string "<%Y-%m-%d %a>"
                      (time-subtract (current-time) (* days 86400))))

(defun test-org-timestamp-days-ahead (days)
  "Generate org timestamp string for DAYS from now.
Returns string like \"<2026-02-20 Fri>\"."
  (format-time-string "<%Y-%m-%d %a>"
                      (time-add (current-time) (* days 86400))))

(defun test-org-timestamp-today ()
  "Generate org timestamp string for today.
Returns string like \"<2026-02-15 Sun>\"."
  (format-time-string "<%Y-%m-%d %a>"))

(provide 'testutil-org)
;;; testutil-org.el ends here
