;;; test-dev-fkeys--projectile-reset-cmds.el --- Tests for cj/projectile-reset-cmds -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the manual escape-hatch command that clears projectile's
;; per-project compile / test / run cache for the current project. Use
;; case: projectile's auto-derived default was wrong to begin with and
;; you want to reset to projectile's default-derived cmd at the next
;; prompt.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defvar projectile-compile-cmd-map nil)
(defvar projectile-test-cmd-map nil)
(defvar projectile-run-cmd-map nil)

;;; Normal Cases

(ert-deftest test-dev-fkeys-projectile-reset-cmds-clears-all-three-maps ()
  "Normal: clears compile, test, and run cache entries for the current root.
Other projects' entries are left alone."
  (let ((projectile-compile-cmd-map (make-hash-table :test 'equal))
        (projectile-test-cmd-map    (make-hash-table :test 'equal))
        (projectile-run-cmd-map     (make-hash-table :test 'equal)))
    (puthash "/p/" "make"      projectile-compile-cmd-map)
    (puthash "/p/" "make test" projectile-test-cmd-map)
    (puthash "/p/" "./run.sh"  projectile-run-cmd-map)
    (puthash "/other/" "untouched" projectile-compile-cmd-map)
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (cj/projectile-reset-cmds))
    (should-not (gethash "/p/" projectile-compile-cmd-map))
    (should-not (gethash "/p/" projectile-test-cmd-map))
    (should-not (gethash "/p/" projectile-run-cmd-map))
    (should (string= (gethash "/other/" projectile-compile-cmd-map) "untouched"))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-projectile-reset-cmds-no-cached-entry-is-noop ()
  "Boundary: project root has no cached entries → command runs cleanly,
no error, maps stay empty."
  (let ((projectile-compile-cmd-map (make-hash-table :test 'equal))
        (projectile-test-cmd-map    (make-hash-table :test 'equal))
        (projectile-run-cmd-map     (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (cj/projectile-reset-cmds))
    (should (zerop (hash-table-count projectile-compile-cmd-map)))
    (should (zerop (hash-table-count projectile-test-cmd-map)))
    (should (zerop (hash-table-count projectile-run-cmd-map)))))

;;; Error Cases

(ert-deftest test-dev-fkeys-projectile-reset-cmds-no-project-signals-user-error ()
  "Error: no project detected → user-error rather than silent no-op."
  (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () nil)))
    (should-error (cj/projectile-reset-cmds) :type 'user-error)))

(provide 'test-dev-fkeys--projectile-reset-cmds)
;;; test-dev-fkeys--projectile-reset-cmds.el ends here
