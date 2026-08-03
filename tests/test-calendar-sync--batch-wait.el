;;; test-calendar-sync--batch-wait.el --- Batch wait-loop tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; The sync pipeline is asynchronous end to end: curl runs in one process and
;; the org conversion in a second batch Emacs.  Under `emacs --batch' the
;; process exits as soon as the top-level form returns, killing both children
;; mid-flight -- a run that does nothing and reports success.
;;
;; `calendar-sync--batch-wait' is what stops that: it blocks until every
;; calendar has left the `syncing' state, or until the timeout expires.  These
;; tests drive it with a stubbed state predicate, so the loop's exit conditions
;; are covered without a live network fetch.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calendar-sync)

(ert-deftest test-calendar-sync-batch-wait-returns-when-nothing-in-flight ()
  "Normal: with no calendar syncing the wait returns success immediately."
  (let ((polls 0))
    (cl-letf (((symbol-function 'calendar-sync--syncing-p) (lambda (_) nil))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _) (setq polls (1+ polls)))))
      (should (calendar-sync--batch-wait '("google" "proton") 5))
      (should (= polls 0)))))

(ert-deftest test-calendar-sync-batch-wait-blocks-until-settled ()
  "Normal: the wait polls while a sync is in flight and returns once it lands."
  (let ((remaining 3)
        (polls 0))
    (cl-letf (((symbol-function 'calendar-sync--syncing-p)
               (lambda (_) (> remaining 0)))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _)
                 (setq polls (1+ polls))
                 (setq remaining (1- remaining)))))
      (should (calendar-sync--batch-wait '("google") 5))
      (should (= polls 3)))))

(ert-deftest test-calendar-sync-batch-wait-empty-names-returns-immediately ()
  "Boundary: no calendars to wait on settles at once."
  (let ((polls 0))
    (cl-letf (((symbol-function 'accept-process-output)
               (lambda (&rest _) (setq polls (1+ polls)))))
      (should (calendar-sync--batch-wait '() 5))
      (should (= polls 0)))))

(ert-deftest test-calendar-sync-batch-wait-times-out-when-stuck ()
  "Error: a sync that never settles returns nil once the timeout expires.
Returning nil is what lets the runner exit non-zero instead of reporting a
success it cannot vouch for."
  (let ((calendar-sync--batch-poll-seconds 0.01))
    (cl-letf (((symbol-function 'calendar-sync--syncing-p) (lambda (_) t))
              ((symbol-function 'accept-process-output) (lambda (&rest _) nil)))
      (should-not (calendar-sync--batch-wait '("google") 0.05)))))

(ert-deftest test-calendar-sync-batch-wait-zero-timeout-does-not-hang ()
  "Boundary: a zero timeout returns at once rather than looping forever."
  (cl-letf (((symbol-function 'calendar-sync--syncing-p) (lambda (_) t))
            ((symbol-function 'accept-process-output) (lambda (&rest _) nil)))
    (should-not (calendar-sync--batch-wait '("google") 0))))

(provide 'test-calendar-sync--batch-wait)
;;; test-calendar-sync--batch-wait.el ends here
