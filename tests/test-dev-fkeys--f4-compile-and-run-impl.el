;;; test-dev-fkeys--f4-compile-and-run-impl.el --- Tests for cj/--f4-compile-and-run-impl -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the "Compile + Run" action handler. After kicking off the
;; compile, attaches a one-shot `compilation-finish-functions' hook that
;; runs the project on success.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-compile-and-run-impl-invokes-projectile-compile ()
  "Normal: handler calls `projectile-compile-project'.

Components integrated:
- `cj/--f4-compile-and-run-impl' (unit under test)
- `projectile-compile-project' (MOCKED via cl-letf)
- `compilation-finish-functions' (real, scoped via let)"
  (let ((compile-calls 0)
        (compilation-finish-functions nil))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (_arg) (cl-incf compile-calls))))
      (cj/--f4-compile-and-run-impl)
      (should (= compile-calls 1)))))

(ert-deftest test-dev-fkeys-compile-and-run-impl-installs-finish-hook ()
  "Normal: handler installs exactly one hook in `compilation-finish-functions'."
  (let ((compilation-finish-functions nil))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (_arg) nil)))
      (cj/--f4-compile-and-run-impl)
      (should (= (length compilation-finish-functions) 1)))))

(ert-deftest test-dev-fkeys-compile-and-run-impl-hook-runs-projectile-run-on-success ()
  "Normal: when the compile finishes successfully, the installed hook calls
`projectile-run-project'.

Components integrated:
- `cj/--f4-compile-and-run-impl' (unit under test)
- `projectile-compile-project' (MOCKED — no-op)
- `projectile-run-project' (MOCKED — counts calls)
- `compilation-finish-functions' (real)
- `run-hook-with-args' (real — simulates compile.el firing the hook)"
  (let ((run-calls 0)
        (compilation-finish-functions nil))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (_arg) nil))
              ((symbol-function 'projectile-run-project)
               (lambda (_arg) (cl-incf run-calls))))
      (cj/--f4-compile-and-run-impl)
      (run-hook-with-args 'compilation-finish-functions nil "finished\n")
      (should (= run-calls 1)))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-compile-and-run-impl-hook-skips-projectile-run-on-failure ()
  "Boundary: when the compile fails, projectile-run-project must not run.
The hook still self-removes (covered in the make-once-hook tests)."
  (let ((run-calls 0)
        (compilation-finish-functions nil))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (_arg) nil))
              ((symbol-function 'projectile-run-project)
               (lambda (_arg) (cl-incf run-calls))))
      (cj/--f4-compile-and-run-impl)
      (run-hook-with-args 'compilation-finish-functions nil "exited abnormally\n")
      (should (= run-calls 0)))))

(provide 'test-dev-fkeys--f4-compile-and-run-impl)
;;; test-dev-fkeys--f4-compile-and-run-impl.el ends here
