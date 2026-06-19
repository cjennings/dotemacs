;;; test-reconcile--dirty-p.el --- Tests for cj/reconcile--dirty-p -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for `cj/reconcile--dirty-p' in reconcile-open-repos.el.  It runs
;; git status --porcelain via `cj/reconcile--git' and reports clean (nil),
;; dirty (non-nil), or 'status-failed when git itself errors.  The git call
;; is stubbed at the `cj/reconcile--git' boundary (it returns a plist).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'reconcile-open-repos)

(defmacro test-reconcile-dirty--with-git (plist &rest body)
  "Run BODY with `cj/reconcile--git' stubbed to return PLIST."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'cj/reconcile--git)
              (lambda (&rest _) ,plist)))
     ,@body))

;;; Normal Cases

(ert-deftest test-reconcile-dirty-p-clean-returns-nil ()
  "Normal: exit 0 with empty porcelain output means clean (nil)."
  (test-reconcile-dirty--with-git '(:exit 0 :output "")
    (should-not (cj/reconcile--dirty-p "/repo"))))

(ert-deftest test-reconcile-dirty-p-dirty-returns-non-nil ()
  "Normal: exit 0 with porcelain content means dirty (non-nil)."
  (test-reconcile-dirty--with-git '(:exit 0 :output " M file.el\n")
    (should (cj/reconcile--dirty-p "/repo"))))

;;; Boundary Cases

(ert-deftest test-reconcile-dirty-p-whitespace-only-is-clean ()
  "Boundary: whitespace-only output trims to empty and counts as clean."
  (test-reconcile-dirty--with-git '(:exit 0 :output "   \n")
    (should-not (cj/reconcile--dirty-p "/repo"))))

;;; Error Cases

(ert-deftest test-reconcile-dirty-p-git-failure-returns-status-failed ()
  "Error: a non-zero git exit returns the symbol 'status-failed."
  (test-reconcile-dirty--with-git '(:exit 128 :output "fatal: not a repo")
    (should (eq (cj/reconcile--dirty-p "/repo") 'status-failed))))

(provide 'test-reconcile--dirty-p)
;;; test-reconcile--dirty-p.el ends here
