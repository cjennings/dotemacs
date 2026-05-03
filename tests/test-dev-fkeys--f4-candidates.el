;;; test-dev-fkeys--f4-candidates.el --- Tests for cj/--f4-candidates -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the candidate-menu builder. Returns an alist of
;; (LABEL . ACTION) for the F4 completing-read prompt. The first entry
;; is the default (selected on RET).

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-f4-candidates-compiled-has-four-entries ()
  "Normal: compiled project gets all four candidates."
  (let ((cands (cj/--f4-candidates 'compiled)))
    (should (= (length cands) 4))))

(ert-deftest test-dev-fkeys-f4-candidates-compiled-default-is-compile-and-run ()
  "Normal: first candidate for compiled is Compile + Run (the default)."
  (let ((cands (cj/--f4-candidates 'compiled)))
    (should (equal (caar cands) "Compile + Run"))
    (should (eq (cdar cands) 'compile-and-run))))

(ert-deftest test-dev-fkeys-f4-candidates-compiled-includes-all-actions ()
  "Normal: compiled menu maps each label to its action symbol."
  (let ((cands (cj/--f4-candidates 'compiled)))
    (should (eq (cdr (assoc "Compile + Run" cands))   'compile-and-run))
    (should (eq (cdr (assoc "Compile" cands))         'compile-only))
    (should (eq (cdr (assoc "Run" cands))             'run-only))
    (should (eq (cdr (assoc "Clean + Rebuild" cands)) 'clean-rebuild))))

(ert-deftest test-dev-fkeys-f4-candidates-interpreted-is-run-only ()
  "Normal: interpreted project gets a single Run candidate."
  (let ((cands (cj/--f4-candidates 'interpreted)))
    (should (= (length cands) 1))
    (should (equal (caar cands) "Run"))
    (should (eq (cdar cands) 'run-only))))

(ert-deftest test-dev-fkeys-f4-candidates-unknown-is-compile-plain ()
  "Normal: unknown project gets a single Compile candidate that calls plain compile."
  (let ((cands (cj/--f4-candidates 'unknown)))
    (should (= (length cands) 1))
    (should (equal (caar cands) "Compile"))
    (should (eq (cdar cands) 'compile-plain))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f4-candidates-bogus-symbol-falls-back-to-unknown ()
  "Boundary: an unrecognized project-type symbol falls through to the unknown branch."
  (let ((cands (cj/--f4-candidates 'fictional-type)))
    (should (= (length cands) 1))
    (should (eq (cdar cands) 'compile-plain))))

;;; Error Cases

(ert-deftest test-dev-fkeys-f4-candidates-nil-falls-back-to-unknown ()
  "Error: nil project-type falls through to the unknown branch."
  (let ((cands (cj/--f4-candidates nil)))
    (should (= (length cands) 1))
    (should (eq (cdar cands) 'compile-plain))))

(provide 'test-dev-fkeys--f4-candidates)
;;; test-dev-fkeys--f4-candidates.el ends here
