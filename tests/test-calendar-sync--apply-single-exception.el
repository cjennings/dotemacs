;;; test-calendar-sync--apply-single-exception.el --- Tests for exception application -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for calendar-sync--apply-single-exception.
;; Applies exception overrides to an occurrence plist, returning a modified copy.

;;; Code:

(require 'ert)
(require 'testutil-calendar-sync)
(require 'calendar-sync)

;;; Normal Cases

(ert-deftest test-calendar-sync--apply-single-exception-normal-updates-time ()
  "Exception start/end override the occurrence times."
  (let ((occ (list :start '(2026 3 15 14 0) :end '(2026 3 15 15 0)
                   :summary "Standup" :uid "abc"))
        (exc (list :start '(2026 3 15 16 0) :end '(2026 3 15 17 0))))
    (let ((result (calendar-sync--apply-single-exception occ exc)))
      (should (equal '(2026 3 15 16 0) (plist-get result :start)))
      (should (equal '(2026 3 15 17 0) (plist-get result :end))))))

(ert-deftest test-calendar-sync--apply-single-exception-normal-updates-summary ()
  "Exception with summary overrides the occurrence summary."
  (let ((occ (list :start '(2026 3 15 14 0) :summary "Original"))
        (exc (list :start '(2026 3 15 14 0) :summary "Modified")))
    (let ((result (calendar-sync--apply-single-exception occ exc)))
      (should (equal "Modified" (plist-get result :summary))))))

(ert-deftest test-calendar-sync--apply-single-exception-normal-preserves-unset-fields ()
  "Fields not present in exception are preserved from occurrence."
  (let ((occ (list :start '(2026 3 15 14 0) :summary "Keep" :uid "abc"
                   :location "Room 1"))
        (exc (list :start '(2026 3 15 16 0))))
    (let ((result (calendar-sync--apply-single-exception occ exc)))
      (should (equal "Keep" (plist-get result :summary)))
      (should (equal "abc" (plist-get result :uid)))
      (should (equal "Room 1" (plist-get result :location))))))

;;; Boundary Cases

(ert-deftest test-calendar-sync--apply-single-exception-boundary-nil-end ()
  "Exception without :end preserves occurrence :end."
  (let ((occ (list :start '(2026 3 15 14 0) :end '(2026 3 15 15 0)))
        (exc (list :start '(2026 3 15 16 0))))
    (let ((result (calendar-sync--apply-single-exception occ exc)))
      (should (equal '(2026 3 15 15 0) (plist-get result :end))))))

(ert-deftest test-calendar-sync--apply-single-exception-boundary-does-not-mutate ()
  "Original occurrence plist is not mutated."
  (let ((occ (list :start '(2026 3 15 14 0) :summary "Original"))
        (exc (list :start '(2026 3 15 16 0) :summary "Changed")))
    (calendar-sync--apply-single-exception occ exc)
    (should (equal "Original" (plist-get occ :summary)))))

;;; Error Cases

(ert-deftest test-calendar-sync--apply-single-exception-error-empty-exception ()
  "Exception with no fields still returns a valid plist with occurrence data."
  (let ((occ (list :start '(2026 3 15 14 0) :summary "Keep"))
        (exc (list :start nil)))
    (let ((result (calendar-sync--apply-single-exception occ exc)))
      (should (equal "Keep" (plist-get result :summary))))))

(provide 'test-calendar-sync--apply-single-exception)
;;; test-calendar-sync--apply-single-exception.el ends here
