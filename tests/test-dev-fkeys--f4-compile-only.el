;;; test-dev-fkeys--f4-compile-only.el --- Smoke tests for cj/f4-compile-only -*- lexical-binding: t -*-

;;; Commentary:
;; Smoke tests for the C-F4 fast path. On a compiled project, calls
;; `projectile-compile-project'. On an interpreted project, messages
;; "not a compiled language". With no project detected, falls back to
;; plain `compile'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defmacro test-dev-fkeys-co--with-project (markers &rest body)
  "Set up a temp project with MARKERS, bind ROOT, run BODY, clean up."
  (declare (indent 1))
  `(let ((root (make-temp-file "test-dev-fkeys-co-" t)))
     (unwind-protect
         (progn
           (dolist (marker ,markers)
             (write-region "" nil (expand-file-name marker root)))
           ,@body)
       (delete-directory root t))))

;;; Normal Cases

(ert-deftest test-dev-fkeys-f4-compile-only-compiled-project-runs-projectile-compile ()
  "Normal: on a compiled project, calls projectile-compile-project."
  (test-dev-fkeys-co--with-project '("go.mod")
    (let ((calls 0))
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'projectile-compile-project)
                 (lambda (_arg) (cl-incf calls))))
        (cj/f4-compile-only)
        (should (= calls 1))))))

(ert-deftest test-dev-fkeys-f4-compile-only-propagates-prefix-arg ()
  "Normal: on a compiled project, `current-prefix-arg' is forwarded to
projectile-compile-project so `C-u C-F4' forces a re-prompt."
  (test-dev-fkeys-co--with-project '("go.mod")
    (let ((seen-arg 'unset)
          (current-prefix-arg t))
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'projectile-compile-project)
                 (lambda (arg) (setq seen-arg arg))))
        (cj/f4-compile-only)
        (should (eq seen-arg t))))))

(ert-deftest test-dev-fkeys-f4-compile-only-interpreted-project-skips-compile ()
  "Normal: on an interpreted project, projectile-compile-project does not run."
  (test-dev-fkeys-co--with-project '("pyproject.toml")
    (let ((calls 0))
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'projectile-compile-project)
                 (lambda (_arg) (cl-incf calls))))
        (cj/f4-compile-only)
        (should (= calls 0))))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f4-compile-only-unknown-project-falls-back-to-compile ()
  "Boundary: outside any project, falls back to interactive `compile'."
  (let (received-fn)
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () nil))
              ((symbol-function 'call-interactively)
               (lambda (fn &rest _) (setq received-fn fn))))
      (cj/f4-compile-only)
      (should (eq received-fn #'compile)))))

(provide 'test-dev-fkeys--f4-compile-only)
;;; test-dev-fkeys--f4-compile-only.el ends here
