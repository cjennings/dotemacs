;;; test-dev-fkeys--f4-clean-rebuild-impl.el --- Tests for cj/--f4-clean-rebuild-impl -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the "Clean + Rebuild" action handler. Runs the heuristic clean
;; command via `compile' from the project root, then chains
;; `projectile-compile-project' on success via the one-shot finish hook.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defmacro test-dev-fkeys-cr--with-project (markers &rest body)
  "Create a temp project dir with each filename in MARKERS as an empty file.
Bind the dir path to ROOT in BODY. Cleans up on exit."
  (declare (indent 1))
  `(let ((root (make-temp-file "test-dev-fkeys-cr-" t)))
     (unwind-protect
         (progn
           (dolist (marker ,markers)
             (write-region "" nil (expand-file-name marker root)))
           ,@body)
       (delete-directory root t))))

;;; Normal Cases

(ert-deftest test-dev-fkeys-clean-rebuild-impl-runs-derived-clean-cmd ()
  "Normal: handler invokes `compile' with the heuristic clean command for the
project marker present at ROOT.

Components integrated:
- `cj/--f4-clean-rebuild-impl' (unit under test)
- `cj/--f4-derive-clean-cmd' (real)
- `compile' (MOCKED — captures the command string)
- `projectile-compile-project' (MOCKED — no-op)
- `compilation-finish-functions' (real, scoped via let)"
  (test-dev-fkeys-cr--with-project '("Makefile")
    (let ((compile-calls nil)
          (compilation-finish-functions nil))
      (cl-letf (((symbol-function 'compile)
                 (lambda (cmd) (push cmd compile-calls)))
                ((symbol-function 'projectile-compile-project)
                 (lambda (_arg) nil)))
        (cj/--f4-clean-rebuild-impl root)
        (should (equal compile-calls '("make clean")))))))

(ert-deftest test-dev-fkeys-clean-rebuild-impl-installs-finish-hook ()
  "Normal: handler installs exactly one hook in `compilation-finish-functions'."
  (test-dev-fkeys-cr--with-project '("go.mod")
    (let ((compilation-finish-functions nil))
      (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil))
                ((symbol-function 'projectile-compile-project)
                 (lambda (_arg) nil)))
        (cj/--f4-clean-rebuild-impl root)
        (should (= (length compilation-finish-functions) 1))))))

(ert-deftest test-dev-fkeys-clean-rebuild-impl-hook-runs-projectile-compile-on-success ()
  "Normal: when the clean step finishes successfully, the installed hook
calls `projectile-compile-project' to do the rebuild."
  (test-dev-fkeys-cr--with-project '("Cargo.toml")
    (let ((compile-calls 0)
          (compilation-finish-functions nil))
      (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil))
                ((symbol-function 'projectile-compile-project)
                 (lambda (_arg) (cl-incf compile-calls))))
        (cj/--f4-clean-rebuild-impl root)
        (run-hook-with-args 'compilation-finish-functions nil "finished\n")
        (should (= compile-calls 1))))))

(ert-deftest test-dev-fkeys-clean-rebuild-impl-runs-clean-from-project-root ()
  "Normal: the clean compile runs with default-directory bound to ROOT."
  (test-dev-fkeys-cr--with-project '("Eask")
    (let ((seen-dir nil)
          (compilation-finish-functions nil))
      (cl-letf (((symbol-function 'compile)
                 (lambda (_cmd) (setq seen-dir default-directory)))
                ((symbol-function 'projectile-compile-project)
                 (lambda (_arg) nil)))
        (cj/--f4-clean-rebuild-impl root)
        (should (string= (file-name-as-directory seen-dir)
                         (file-name-as-directory root)))))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-clean-rebuild-impl-hook-skips-rebuild-on-failure ()
  "Boundary: when the clean step fails, projectile-compile-project does not run."
  (test-dev-fkeys-cr--with-project '("Makefile")
    (let ((compile-calls 0)
          (compilation-finish-functions nil))
      (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil))
                ((symbol-function 'projectile-compile-project)
                 (lambda (_arg) (cl-incf compile-calls))))
        (cj/--f4-clean-rebuild-impl root)
        (run-hook-with-args 'compilation-finish-functions nil "exited abnormally\n")
        (should (= compile-calls 0))))))

;;; Error Cases

(ert-deftest test-dev-fkeys-clean-rebuild-impl-no-clean-cmd-signals-user-error ()
  "Error: a project root with no recognized markers signals a user-error
rather than silently running nothing."
  (test-dev-fkeys-cr--with-project '("README.md")
    (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil))
              ((symbol-function 'projectile-compile-project)
               (lambda (_arg) nil)))
      (should-error (cj/--f4-clean-rebuild-impl root) :type 'user-error))))

(ert-deftest test-dev-fkeys-clean-rebuild-impl-nil-root-signals-user-error ()
  "Error: a nil root signals a user-error (no project detected)."
  (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil))
            ((symbol-function 'projectile-compile-project)
             (lambda (_arg) nil)))
    (should-error (cj/--f4-clean-rebuild-impl nil) :type 'user-error)))

(provide 'test-dev-fkeys--f4-clean-rebuild-impl)
;;; test-dev-fkeys--f4-clean-rebuild-impl.el ends here
