;;; test-dev-fkeys--f4-compile-and-run-impl.el --- Tests for cj/--f4-compile-and-run-impl -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the "Compile + Run" action handler. After kicking off the
;; compile, attaches a one-shot finish hook buffer-locally in the
;; compilation buffer projectile returns, so the global
;; `compilation-finish-functions' is never touched.  A quit at
;; projectile's compile prompt therefore can never leave an armed hook
;; that a later unrelated compile would fire.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defmacro test-dev-fkeys-car--with-buffer (buf &rest body)
  "Run BODY with BUF bound to a temp buffer standing in for a compilation buffer."
  (declare (indent 1))
  `(let ((,buf (generate-new-buffer " *test-compilation*")))
     (unwind-protect
         (progn ,@body)
       (kill-buffer ,buf))))

;;; Normal Cases

(ert-deftest test-dev-fkeys-compile-and-run-impl-invokes-projectile-compile ()
  "Normal: handler calls `projectile-compile-project'.

Components integrated:
- `cj/--f4-compile-and-run-impl' (unit under test)
- `projectile-compile-project' (MOCKED via cl-letf)"
  (let ((compile-calls 0))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (_arg) (cl-incf compile-calls) nil)))
      (cj/--f4-compile-and-run-impl)
      (should (= compile-calls 1)))))

(ert-deftest test-dev-fkeys-compile-and-run-impl-installs-hook-in-compilation-buffer ()
  "Normal: the one-shot hook lands buffer-locally in the compilation buffer;
the global `compilation-finish-functions' stays untouched."
  (test-dev-fkeys-car--with-buffer buf
    (let ((compilation-finish-functions nil))
      (cl-letf (((symbol-function 'projectile-compile-project)
                 (lambda (_arg) buf)))
        (cj/--f4-compile-and-run-impl)
        (should (null compilation-finish-functions))
        (should (= 1 (length (remq t (buffer-local-value
                                      'compilation-finish-functions buf)))))))))

(ert-deftest test-dev-fkeys-compile-and-run-impl-hook-runs-projectile-run-on-success ()
  "Normal: when the compile finishes successfully, the buffer-local hook
calls `projectile-run-project'.

Components integrated:
- `cj/--f4-compile-and-run-impl' (unit under test)
- `projectile-compile-project' (MOCKED — returns the compilation buffer)
- `projectile-run-project' (MOCKED — counts calls)
- `compilation-finish-functions' (real, buffer-local)
- `run-hook-with-args' (real — simulates compile.el firing the hook)"
  (test-dev-fkeys-car--with-buffer buf
    (let ((run-calls 0)
          (compilation-finish-functions nil))
      (cl-letf (((symbol-function 'projectile-compile-project)
                 (lambda (_arg) buf))
                ((symbol-function 'projectile-run-project)
                 (lambda (_arg) (cl-incf run-calls))))
        (cj/--f4-compile-and-run-impl)
        (with-current-buffer buf
          (run-hook-with-args 'compilation-finish-functions buf "finished\n"))
        (should (= run-calls 1))))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-compile-and-run-impl-hook-skips-projectile-run-on-failure ()
  "Boundary: when the compile fails, projectile-run-project must not run.
The hook still self-removes (covered in the make-once-hook tests)."
  (test-dev-fkeys-car--with-buffer buf
    (let ((run-calls 0)
          (compilation-finish-functions nil))
      (cl-letf (((symbol-function 'projectile-compile-project)
                 (lambda (_arg) buf))
                ((symbol-function 'projectile-run-project)
                 (lambda (_arg) (cl-incf run-calls))))
        (cj/--f4-compile-and-run-impl)
        (with-current-buffer buf
          (run-hook-with-args 'compilation-finish-functions
                              buf "exited abnormally\n"))
        (should (= run-calls 0))))))

(ert-deftest test-dev-fkeys-compile-and-run-impl-quit-leaves-no-global-hook ()
  "Boundary: a quit at projectile's prompt leaves no armed hook anywhere.
This is the regression the buffer-local install exists to prevent: the
old shape armed a global hook before the prompt, so C-g left it live and
the next unrelated compile fired the chained run."
  (let ((compilation-finish-functions nil))
    (cl-letf (((symbol-function 'projectile-compile-project)
               (lambda (_arg) (signal 'quit nil))))
      (condition-case nil
          (cj/--f4-compile-and-run-impl)
        (quit nil))
      (should (null compilation-finish-functions)))))

(provide 'test-dev-fkeys--f4-compile-and-run-impl)
;;; test-dev-fkeys--f4-compile-and-run-impl.el ends here
