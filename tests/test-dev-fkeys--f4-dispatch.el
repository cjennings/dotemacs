;;; test-dev-fkeys--f4-dispatch.el --- Tests for cj/--f4-dispatch -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the dispatch router. Each known action symbol routes to the
;; corresponding command. Unknown actions raise user-error. The action
;; handlers themselves are tested in their own files; this file only
;; verifies the routing layer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-dispatch-compile-only-calls-projectile-compile ()
  "Normal: 'compile-only routes to projectile-compile-project."
  (let ((calls 0))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (_arg) (cl-incf calls))))
      (cj/--f4-dispatch 'compile-only)
      (should (= calls 1)))))

(ert-deftest test-dev-fkeys-dispatch-compile-only-propagates-prefix-arg ()
  "Normal: 'compile-only forwards `current-prefix-arg' to
projectile-compile-project so `C-u F4 → Compile' forces a re-prompt."
  (let ((seen-arg 'unset)
        (current-prefix-arg t))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (arg) (setq seen-arg arg))))
      (cj/--f4-dispatch 'compile-only)
      (should (eq seen-arg t)))))

(ert-deftest test-dev-fkeys-dispatch-run-only-propagates-prefix-arg ()
  "Normal: 'run-only forwards `current-prefix-arg' to
projectile-run-project."
  (let ((seen-arg 'unset)
        (current-prefix-arg t))
    (cl-letf (((symbol-function 'projectile-run-project)
               (lambda (arg) (setq seen-arg arg))))
      (cj/--f4-dispatch 'run-only)
      (should (eq seen-arg t)))))

(ert-deftest test-dev-fkeys-dispatch-run-only-calls-projectile-run ()
  "Normal: 'run-only routes to projectile-run-project."
  (let ((calls 0))
    (cl-letf (((symbol-function 'projectile-run-project)
               (lambda (_arg) (cl-incf calls))))
      (cj/--f4-dispatch 'run-only)
      (should (= calls 1)))))

(ert-deftest test-dev-fkeys-dispatch-compile-and-run-routes-to-impl ()
  "Normal: 'compile-and-run routes to cj/--f4-compile-and-run-impl."
  (let ((calls 0))
    (cl-letf (((symbol-function 'cj/--f4-compile-and-run-impl)
               (lambda () (cl-incf calls))))
      (cj/--f4-dispatch 'compile-and-run)
      (should (= calls 1)))))

(ert-deftest test-dev-fkeys-dispatch-clean-rebuild-routes-to-impl-with-root ()
  "Normal: 'clean-rebuild routes to cj/--f4-clean-rebuild-impl with the project root.

Components integrated:
- `cj/--f4-dispatch' (unit under test)
- `cj/--f4-project-root' (MOCKED — returns a fake path)
- `cj/--f4-clean-rebuild-impl' (MOCKED — captures the ROOT it received)"
  (let (received-root)
    (cl-letf (((symbol-function 'cj/--f4-project-root)
               (lambda () "/fake/project/"))
              ((symbol-function 'cj/--f4-clean-rebuild-impl)
               (lambda (root) (setq received-root root))))
      (cj/--f4-dispatch 'clean-rebuild)
      (should (string= received-root "/fake/project/")))))

(ert-deftest test-dev-fkeys-dispatch-compile-plain-uses-call-interactively-on-compile ()
  "Normal: 'compile-plain invokes `compile' interactively (so the user is
prompted for a command). We check that `call-interactively' fires with
the symbol `compile'."
  (let (received-fn)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn &rest _) (setq received-fn fn))))
      (cj/--f4-dispatch 'compile-plain)
      (should (eq received-fn #'compile)))))

;;; Error Cases

(ert-deftest test-dev-fkeys-dispatch-unknown-action-signals-user-error ()
  "Error: dispatch on an unknown symbol raises user-error."
  (should-error (cj/--f4-dispatch 'fictional-action) :type 'user-error))

(ert-deftest test-dev-fkeys-dispatch-nil-action-signals-user-error ()
  "Error: nil action raises user-error (defensive against bad menu data)."
  (should-error (cj/--f4-dispatch nil) :type 'user-error))

(provide 'test-dev-fkeys--f4-dispatch)
;;; test-dev-fkeys--f4-dispatch.el ends here
