;;; test-calendar-sync--filter-declined.el --- Tests for filter-declined  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for calendar-sync--filter-declined function.
;; Drops events whose :status is "declined" when calendar-sync-skip-declined
;; is non-nil; otherwise returns the input list unchanged.
;; Covers Normal, Boundary, and Error cases.

;;; Code:

(require 'ert)
(require 'calendar-sync)

;;; Test Data

(defun test-filter-declined--make-events ()
  "Return a small mixed-status event list."
  (list (list :uid "a" :summary "Accepted meeting"  :status "accepted")
        (list :uid "b" :summary "Declined meeting"  :status "declined")
        (list :uid "c" :summary "Tentative meeting" :status "tentative")
        (list :uid "d" :summary "Unknown status"    :status nil)
        (list :uid "e" :summary "Needs action"      :status "needs-action")))

;;; Normal Cases

(ert-deftest test-calendar-sync--filter-declined-normal-drops-declined ()
  "Normal: declined events are removed when the toggle is on."
  (let ((calendar-sync-skip-declined t)
        (events (test-filter-declined--make-events)))
    (let ((result (calendar-sync--filter-declined events)))
      (should (= 4 (length result)))
      (should-not (cl-find "declined" result
                           :key (lambda (e) (plist-get e :status))
                           :test #'equal)))))

(ert-deftest test-calendar-sync--filter-declined-normal-keeps-accepted ()
  "Normal: accepted events are preserved."
  (let ((calendar-sync-skip-declined t)
        (events (test-filter-declined--make-events)))
    (let ((result (calendar-sync--filter-declined events)))
      (should (cl-find "accepted" result
                       :key (lambda (e) (plist-get e :status))
                       :test #'equal)))))

(ert-deftest test-calendar-sync--filter-declined-normal-keeps-tentative ()
  "Normal: tentative events are preserved (only declined is dropped)."
  (let ((calendar-sync-skip-declined t)
        (events (test-filter-declined--make-events)))
    (let ((result (calendar-sync--filter-declined events)))
      (should (cl-find "tentative" result
                       :key (lambda (e) (plist-get e :status))
                       :test #'equal)))))

(ert-deftest test-calendar-sync--filter-declined-normal-keeps-needs-action ()
  "Normal: needs-action events are preserved."
  (let ((calendar-sync-skip-declined t)
        (events (test-filter-declined--make-events)))
    (let ((result (calendar-sync--filter-declined events)))
      (should (cl-find "needs-action" result
                       :key (lambda (e) (plist-get e :status))
                       :test #'equal)))))

(ert-deftest test-calendar-sync--filter-declined-normal-keeps-nil-status ()
  "Normal: events with no PARTSTAT (nil :status) are preserved.
Events without an attendee block (one-person events, ICS feeds that
omit attendee data) come through with :status nil and must not be
filtered."
  (let ((calendar-sync-skip-declined t)
        (events (test-filter-declined--make-events)))
    (let ((result (calendar-sync--filter-declined events)))
      (should (cl-find nil result
                       :key (lambda (e) (plist-get e :status))
                       :test #'equal)))))

(ert-deftest test-calendar-sync--filter-declined-normal-toggle-off-keeps-all ()
  "Normal: when the toggle is nil, declined events pass through."
  (let ((calendar-sync-skip-declined nil)
        (events (test-filter-declined--make-events)))
    (let ((result (calendar-sync--filter-declined events)))
      (should (= 5 (length result)))
      (should (cl-find "declined" result
                       :key (lambda (e) (plist-get e :status))
                       :test #'equal)))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--filter-declined-boundary-empty-list ()
  "Boundary: empty event list returns empty list under either toggle setting."
  (let ((calendar-sync-skip-declined t))
    (should (null (calendar-sync--filter-declined '()))))
  (let ((calendar-sync-skip-declined nil))
    (should (null (calendar-sync--filter-declined '())))))

(ert-deftest test-calendar-sync--filter-declined-boundary-all-declined ()
  "Boundary: list of only declined events returns empty list."
  (let ((calendar-sync-skip-declined t)
        (events (list (list :uid "a" :status "declined")
                      (list :uid "b" :status "declined")
                      (list :uid "c" :status "declined"))))
    (should (null (calendar-sync--filter-declined events)))))

(ert-deftest test-calendar-sync--filter-declined-boundary-none-declined ()
  "Boundary: list with zero declined events is returned unchanged in length."
  (let ((calendar-sync-skip-declined t)
        (events (list (list :uid "a" :status "accepted")
                      (list :uid "b" :status "tentative")
                      (list :uid "c" :status nil))))
    (should (= 3 (length (calendar-sync--filter-declined events))))))

(ert-deftest test-calendar-sync--filter-declined-boundary-case-mismatch-not-dropped ()
  "Boundary: a non-lowercase \"Declined\" is not dropped.
`calendar-sync--find-user-status' downcases PARTSTAT before storing,
so any :status reaching the filter is already lowercase. Anything
else (uppercase, mixed-case) is an upstream protocol mismatch and we
keep the event rather than filter it on a fuzzy match."
  (let ((calendar-sync-skip-declined t)
        (events (list (list :uid "a" :status "Declined")
                      (list :uid "b" :status "DECLINED"))))
    (should (= 2 (length (calendar-sync--filter-declined events))))))

;;; Error Cases

(ert-deftest test-calendar-sync--filter-declined-error-nil-input ()
  "Error: nil input returns nil (no crash)."
  (let ((calendar-sync-skip-declined t))
    (should (null (calendar-sync--filter-declined nil)))))

(provide 'test-calendar-sync--filter-declined)
;;; test-calendar-sync--filter-declined.el ends here
