;;; test-calendar-sync--batch-results.el --- Batch result collection tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; `calendar-sync--batch-results' reads the per-calendar state table and
;; returns one (NAME . STATUS) pair per requested calendar.  The batch runner
;; turns that into an exit code, so a calendar that never reached the table at
;; all has to read as `never' rather than nil -- a nil status would compare
;; equal to nothing and quietly drop out of the failure count.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calendar-sync)

(defun test-calendar-sync-batch-results--with-states (states body)
  "Run BODY with STATES (an alist of NAME . PLIST) in the state table."
  (let ((calendar-sync--calendar-states (make-hash-table :test 'equal)))
    (dolist (entry states)
      (puthash (car entry) (cdr entry) calendar-sync--calendar-states))
    (funcall body)))

(ert-deftest test-calendar-sync-batch-results-reports-each-status ()
  "Normal: every requested calendar comes back with its recorded status."
  (test-calendar-sync-batch-results--with-states
   '(("google" . (:status ok))
     ("proton" . (:status error :last-error "boom")))
   (lambda ()
     (should (equal (calendar-sync--batch-results '("google" "proton"))
                    '(("google" . ok) ("proton" . error)))))))

(ert-deftest test-calendar-sync-batch-results-empty-names-is-empty ()
  "Boundary: no calendars requested yields no rows, not an error."
  (test-calendar-sync-batch-results--with-states
   '(("google" . (:status ok)))
   (lambda ()
     (should (equal (calendar-sync--batch-results '()) '())))))

(ert-deftest test-calendar-sync-batch-results-missing-calendar-reads-never ()
  "Error: a calendar absent from the state table reads `never', never nil.
A nil status would drop out of the failure count and report success for a
calendar that never ran."
  (test-calendar-sync-batch-results--with-states
   '(("google" . (:status ok)))
   (lambda ()
     (should (equal (calendar-sync--batch-results '("google" "absent"))
                    '(("google" . ok) ("absent" . never)))))))

(ert-deftest test-calendar-sync-batch-results-preserves-request-order ()
  "Boundary: rows come back in the order asked for, not hash order."
  (test-calendar-sync-batch-results--with-states
   '(("a" . (:status ok)) ("b" . (:status ok)) ("c" . (:status ok)))
   (lambda ()
     (should (equal (mapcar #'car (calendar-sync--batch-results '("c" "a" "b")))
                    '("c" "a" "b"))))))

(provide 'test-calendar-sync--batch-results)
;;; test-calendar-sync--batch-results.el ends here
