;;; test-calendar-sync--batch-failures.el --- Batch failure filter tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; `calendar-sync--batch-failures' picks the rows that did not finish cleanly.
;; The batch runner's exit code is derived from it, and systemd reads that exit
;; code, so the rule is deliberately strict: only `ok' passes.  A calendar left
;; `syncing' at the timeout, or one that never started, is a failure -- both
;; states mean the org file on disk is not the calendar's current contents,
;; which is exactly the silent staleness the timer exists to prevent.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calendar-sync)

(ert-deftest test-calendar-sync-batch-failures-keeps-only-non-ok ()
  "Normal: an errored calendar is returned and a healthy one is not."
  (should (equal (calendar-sync--batch-failures
                  '(("google" . ok) ("proton" . error)))
                 '(("proton" . error)))))

(ert-deftest test-calendar-sync-batch-failures-all-ok-is-empty ()
  "Normal: a fully successful run reports no failures."
  (should (equal (calendar-sync--batch-failures
                  '(("google" . ok) ("proton" . ok)))
                 '())))

(ert-deftest test-calendar-sync-batch-failures-empty-input-is-empty ()
  "Boundary: no rows in, no rows out."
  (should (equal (calendar-sync--batch-failures '()) '())))

(ert-deftest test-calendar-sync-batch-failures-timeout-counts-as-failure ()
  "Error: a calendar still `syncing' when the wait expired is a failure.
Its org file was not rewritten, so reporting success would hide the staleness."
  (should (equal (calendar-sync--batch-failures
                  '(("google" . ok) ("proton" . syncing)))
                 '(("proton" . syncing)))))

(ert-deftest test-calendar-sync-batch-failures-never-counts-as-failure ()
  "Error: a calendar that never started is a failure, not a skip."
  (should (equal (calendar-sync--batch-failures '(("google" . never)))
                 '(("google" . never)))))

(provide 'test-calendar-sync--batch-failures)
;;; test-calendar-sync--batch-failures.el ends here
