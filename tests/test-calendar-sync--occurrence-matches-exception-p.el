;;; test-calendar-sync--occurrence-matches-exception-p.el --- Tests for occurrence-exception matching -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--occurrence-matches-exception-p.
;; Compares occurrence :start against exception :recurrence-id on year/month/day/hour/minute.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--occurrence-matches-exception-p-normal-exact-match ()
  "Identical datetime in occurrence and exception returns t."
  (let ((occ (list :start '(2026 3 15 14 0)))
        (exc (list :recurrence-id '(2026 3 15 14 0))))
    (should (calendar-sync--occurrence-matches-exception-p occ exc))))

(ert-deftest test-calendar-sync--occurrence-matches-exception-p-normal-no-match ()
  "Different dates return nil."
  (let ((occ (list :start '(2026 3 15 14 0)))
        (exc (list :recurrence-id '(2026 3 16 14 0))))
    (should-not (calendar-sync--occurrence-matches-exception-p occ exc))))

(ert-deftest test-calendar-sync--occurrence-matches-exception-p-normal-all-day ()
  "All-day events (nil hour/minute) match when dates are equal."
  (let ((occ (list :start '(2026 3 15 nil nil)))
        (exc (list :recurrence-id '(2026 3 15 nil nil))))
    (should (calendar-sync--occurrence-matches-exception-p occ exc))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--occurrence-matches-exception-p-boundary-different-time ()
  "Same date but different hour returns nil."
  (let ((occ (list :start '(2026 3 15 14 0)))
        (exc (list :recurrence-id '(2026 3 15 15 0))))
    (should-not (calendar-sync--occurrence-matches-exception-p occ exc))))

(ert-deftest test-calendar-sync--occurrence-matches-exception-p-boundary-nil-minute-vs-zero ()
  "Nil minute treated as 0 for comparison."
  (let ((occ (list :start '(2026 3 15 14 nil)))
        (exc (list :recurrence-id '(2026 3 15 14 0))))
    (should (calendar-sync--occurrence-matches-exception-p occ exc))))

;;; Error Cases

(ert-deftest test-calendar-sync--occurrence-matches-exception-p-error-nil-start ()
  "Nil :start returns nil."
  (let ((occ (list :summary "No start"))
        (exc (list :recurrence-id '(2026 3 15 14 0))))
    (should-not (calendar-sync--occurrence-matches-exception-p occ exc))))

(provide 'test-calendar-sync--occurrence-matches-exception-p)
;;; test-calendar-sync--occurrence-matches-exception-p.el ends here
