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

;;; Normal Cases — remaining overridable fields

(ert-deftest test-calendar-sync--apply-single-exception-overrides-description ()
  "Normal: an exception :description overrides the occurrence's."
  (let ((occ (list :start '(2026 3 15 14 0) :description "old"))
        (exc (list :start '(2026 3 15 14 0) :description "new")))
    (should (equal "new"
                   (plist-get (calendar-sync--apply-single-exception occ exc)
                              :description)))))

(ert-deftest test-calendar-sync--apply-single-exception-overrides-location ()
  "Normal: an exception :location overrides the occurrence's."
  (let ((occ (list :start '(2026 3 15 14 0) :location "Room A"))
        (exc (list :start '(2026 3 15 14 0) :location "Room B")))
    (should (equal "Room B"
                   (plist-get (calendar-sync--apply-single-exception occ exc)
                              :location)))))

(ert-deftest test-calendar-sync--apply-single-exception-overrides-attendees ()
  "Normal: an exception :attendees overrides the occurrence's."
  (let ((occ (list :start '(2026 3 15 14 0) :attendees '("a")))
        (exc (list :start '(2026 3 15 14 0) :attendees '("b" "c"))))
    (should (equal '("b" "c")
                   (plist-get (calendar-sync--apply-single-exception occ exc)
                              :attendees)))))

(ert-deftest test-calendar-sync--apply-single-exception-overrides-organizer ()
  "Normal: an exception :organizer overrides the occurrence's."
  (let ((occ (list :start '(2026 3 15 14 0) :organizer "old@x"))
        (exc (list :start '(2026 3 15 14 0) :organizer "new@x")))
    (should (equal "new@x"
                   (plist-get (calendar-sync--apply-single-exception occ exc)
                              :organizer)))))

(ert-deftest test-calendar-sync--apply-single-exception-overrides-url ()
  "Normal: an exception :url overrides the occurrence's."
  (let ((occ (list :start '(2026 3 15 14 0) :url "http://old"))
        (exc (list :start '(2026 3 15 14 0) :url "http://new")))
    (should (equal "http://new"
                   (plist-get (calendar-sync--apply-single-exception occ exc)
                              :url)))))

(provide 'test-calendar-sync--apply-single-exception)
;;; test-calendar-sync--apply-single-exception.el ends here
