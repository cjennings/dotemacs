;;; test-local-repository--car-member.el --- Tests for car-member -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for `car-member' in local-repository.el — the predicate
;; localrepo-initialize uses to check whether an archive id is already
;; registered in package-archives / package-archive-priorities.

;;; Code:

(require 'ert)
(require 'local-repository)

;;; Normal Cases

(ert-deftest test-local-repository-car-member-found ()
  "Normal: VALUE present as a car returns the matching tail (non-nil)."
  (should (equal (car-member 'b '((a . 1) (b . 2) (c . 3)))
                 '(b c))))

(ert-deftest test-local-repository-car-member-not-found ()
  "Normal: VALUE absent from every car returns nil."
  (should-not (car-member 'z '((a . 1) (b . 2)))))

(ert-deftest test-local-repository-car-member-string-car ()
  "Normal: car comparison uses `equal', so string keys match by value."
  (should (car-member "localrepo"
                      '(("gnu" . "url1") ("localrepo" . "url2")))))

;;; Boundary Cases

(ert-deftest test-local-repository-car-member-empty-list ()
  "Boundary: an empty list never matches."
  (should-not (car-member 'a nil)))

(ert-deftest test-local-repository-car-member-single-match ()
  "Boundary: a single-element list whose car matches returns non-nil."
  (should (car-member 'only '((only . 1)))))

(ert-deftest test-local-repository-car-member-single-no-match ()
  "Boundary: a single-element list whose car differs returns nil."
  (should-not (car-member 'x '((only . 1)))))

(ert-deftest test-local-repository-car-member-nil-value-with-nil-car ()
  "Boundary: a nil VALUE matches a cons whose car is nil."
  (should (car-member nil '((nil . 1) (a . 2)))))

(ert-deftest test-local-repository-car-member-nil-value-no-nil-car ()
  "Boundary: a nil VALUE with no nil car returns nil."
  (should-not (car-member nil '((a . 1) (b . 2)))))

;;; Error Cases

(ert-deftest test-local-repository-car-member-non-cons-element ()
  "Error: a non-cons element makes `car' signal wrong-type-argument."
  (should-error (car-member 'x '(1 2)) :type 'wrong-type-argument))

(provide 'test-local-repository--car-member)
;;; test-local-repository--car-member.el ends here
