;;; test-calendar-sync--filter-exdates.el --- Tests for EXDATE filtering  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--filter-exdates function.
;; Tests filtering occurrences list to remove EXDATE matches.
;; Following quality-engineer.org guidelines: one function per file.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../modules" (file-name-directory load-file-name)))
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--filter-exdates-normal-single-match-removes-one ()
  "Test that single matching EXDATE removes one occurrence."
  (let ((occurrences (list (list :start '(2026 2 3 13 0) :summary "Meeting")
                           (list :start '(2026 2 10 13 0) :summary "Meeting")
                           (list :start '(2026 2 17 13 0) :summary "Meeting")))
        (exdates '((2026 2 10 13 0))))
    (let ((result (calendar-sync--filter-exdates occurrences exdates)))
      (should (= 2 (length result)))
      ;; Feb 10 should be removed
      (should-not (cl-find-if (lambda (occ)
                                (equal '(2026 2 10 13 0) (plist-get occ :start)))
                              result))
      ;; Feb 3 and Feb 17 should remain
      (should (cl-find-if (lambda (occ)
                            (equal '(2026 2 3 13 0) (plist-get occ :start)))
                          result))
      (should (cl-find-if (lambda (occ)
                            (equal '(2026 2 17 13 0) (plist-get occ :start)))
                          result)))))

(ert-deftest test-calendar-sync--filter-exdates-normal-multiple-matches-removes-all ()
  "Test that multiple EXDATEs remove all matching occurrences."
  (let ((occurrences (list (list :start '(2026 2 3 13 0) :summary "Meeting")
                           (list :start '(2026 2 10 13 0) :summary "Meeting")
                           (list :start '(2026 2 17 13 0) :summary "Meeting")
                           (list :start '(2026 2 24 13 0) :summary "Meeting")))
        (exdates '((2026 2 10 13 0) (2026 2 24 13 0))))
    (let ((result (calendar-sync--filter-exdates occurrences exdates)))
      (should (= 2 (length result)))
      ;; Feb 10 and Feb 24 should be removed
      (should-not (cl-find-if (lambda (occ)
                                (equal '(2026 2 10 13 0) (plist-get occ :start)))
                              result))
      (should-not (cl-find-if (lambda (occ)
                                (equal '(2026 2 24 13 0) (plist-get occ :start)))
                              result)))))

(ert-deftest test-calendar-sync--filter-exdates-normal-preserves-non-matches ()
  "Test that non-matching occurrences are preserved."
  (let ((occurrences (list (list :start '(2026 2 3 13 0) :summary "Meeting")
                           (list :start '(2026 2 10 13 0) :summary "Meeting")))
        (exdates '((2026 3 15 13 0))))  ; No match
    (let ((result (calendar-sync--filter-exdates occurrences exdates)))
      (should (= 2 (length result)))
      ;; Both should remain
      (should (cl-find-if (lambda (occ)
                            (equal '(2026 2 3 13 0) (plist-get occ :start)))
                          result))
      (should (cl-find-if (lambda (occ)
                            (equal '(2026 2 10 13 0) (plist-get occ :start)))
                          result)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--filter-exdates-boundary-empty-exdates-returns-all ()
  "Test that empty exdates list returns all occurrences."
  (let ((occurrences (list (list :start '(2026 2 3 13 0) :summary "Meeting")
                           (list :start '(2026 2 10 13 0) :summary "Meeting")))
        (exdates '()))
    (let ((result (calendar-sync--filter-exdates occurrences exdates)))
      (should (= 2 (length result))))))

(ert-deftest test-calendar-sync--filter-exdates-boundary-empty-occurrences-returns-empty ()
  "Test that empty occurrences list returns empty."
  (let ((occurrences '())
        (exdates '((2026 2 10 13 0))))
    (let ((result (calendar-sync--filter-exdates occurrences exdates)))
      (should (= 0 (length result))))))

(ert-deftest test-calendar-sync--filter-exdates-boundary-all-excluded-returns-empty ()
  "Test that when all occurrences are excluded, returns empty."
  (let ((occurrences (list (list :start '(2026 2 3 13 0) :summary "Meeting")
                           (list :start '(2026 2 10 13 0) :summary "Meeting")))
        (exdates '((2026 2 3 13 0) (2026 2 10 13 0))))
    (let ((result (calendar-sync--filter-exdates occurrences exdates)))
      (should (= 0 (length result))))))

(ert-deftest test-calendar-sync--filter-exdates-boundary-date-only-matches-any-time ()
  "Test that date-only EXDATE (nil hour/minute) matches any time on that day."
  (let ((occurrences (list (list :start '(2026 2 3 9 0) :summary "Morning")
                           (list :start '(2026 2 3 13 0) :summary "Afternoon")
                           (list :start '(2026 2 10 13 0) :summary "Next Week")))
        (exdates '((2026 2 3 nil nil))))  ; Date-only exclusion
    (let ((result (calendar-sync--filter-exdates occurrences exdates)))
      ;; Both Feb 3 occurrences should be removed
      (should (= 1 (length result)))
      (should (equal '(2026 2 10 13 0) (plist-get (car result) :start))))))

;;; Error Cases

(ert-deftest test-calendar-sync--filter-exdates-error-nil-occurrences-handles-gracefully ()
  "Test that nil occurrences handles gracefully."
  (let ((result (calendar-sync--filter-exdates nil '((2026 2 10 13 0)))))
    (should (listp result))
    (should (= 0 (length result)))))

(ert-deftest test-calendar-sync--filter-exdates-error-nil-exdates-returns-occurrences ()
  "Test that nil exdates returns original occurrences."
  (let ((occurrences (list (list :start '(2026 2 3 13 0) :summary "Meeting"))))
    (let ((result (calendar-sync--filter-exdates occurrences nil)))
      (should (= 1 (length result))))))

(provide 'test-calendar-sync--filter-exdates)
;;; test-calendar-sync--filter-exdates.el ends here
