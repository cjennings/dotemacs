;;; test-calendar-sync--exdate-matches-p.el --- Tests for EXDATE matching -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--exdate-matches-p.
;; Checks if an occurrence start matches an excluded date.
;; Date-only EXDATEs (nil hour) match any time on that day.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--exdate-matches-p-normal-exact-match ()
  "Exact datetime match returns t."
  (should (calendar-sync--exdate-matches-p '(2026 3 15 14 0) '(2026 3 15 14 0))))

(ert-deftest test-calendar-sync--exdate-matches-p-normal-no-match ()
  "Different date returns nil."
  (should-not (calendar-sync--exdate-matches-p '(2026 3 15 14 0) '(2026 3 16 14 0))))

(ert-deftest test-calendar-sync--exdate-matches-p-normal-date-only-wildcard ()
  "Date-only EXDATE (nil hour) matches any time on that day."
  (should (calendar-sync--exdate-matches-p '(2026 3 15 14 0) '(2026 3 15 nil nil)))
  (should (calendar-sync--exdate-matches-p '(2026 3 15 9 30) '(2026 3 15 nil nil))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--exdate-matches-p-boundary-different-time ()
  "Same date but different time with timed EXDATE returns nil."
  (should-not (calendar-sync--exdate-matches-p '(2026 3 15 14 0) '(2026 3 15 15 0))))

(ert-deftest test-calendar-sync--exdate-matches-p-boundary-nil-minute-vs-zero ()
  "Nil minute in occurrence treated as 0."
  (should (calendar-sync--exdate-matches-p '(2026 3 15 14 nil) '(2026 3 15 14 0))))

;;; Error Cases

(ert-deftest test-calendar-sync--exdate-matches-p-error-nil-inputs ()
  "Nil occurrence-start or exdate returns nil."
  (should-not (calendar-sync--exdate-matches-p nil '(2026 3 15 14 0)))
  (should-not (calendar-sync--exdate-matches-p '(2026 3 15 14 0) nil)))

(provide 'test-calendar-sync--exdate-matches-p)
;;; test-calendar-sync--exdate-matches-p.el ends here
