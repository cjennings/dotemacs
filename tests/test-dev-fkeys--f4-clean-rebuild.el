;;; test-dev-fkeys--f4-clean-rebuild.el --- Smoke tests for cj/f4-clean-rebuild -*- lexical-binding: t -*-

;;; Commentary:
;; Smoke tests for the M-F4 fast path. On a compiled project, runs the
;; clean-rebuild handler. Interpreted and unknown projects get a no-op
;; message — no rebuild attempt, no error.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defmacro test-dev-fkeys-cr-w--with-project (markers &rest body)
  "Set up a temp project with MARKERS, bind ROOT, run BODY, clean up."
  (declare (indent 1))
  `(let ((root (make-temp-file "test-dev-fkeys-cr-w-" t)))
     (unwind-protect
         (progn
           (dolist (marker ,markers)
             (write-region "" nil (expand-file-name marker root)))
           ,@body)
       (delete-directory root t))))

;;; Normal Cases

(ert-deftest test-dev-fkeys-f4-clean-rebuild-compiled-project-runs-impl-with-root ()
  "Normal: on a compiled project, calls cj/--f4-clean-rebuild-impl with the
project root."
  (test-dev-fkeys-cr-w--with-project '("Makefile")
    (let (received-root)
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'cj/--f4-clean-rebuild-impl)
                 (lambda (r) (setq received-root r))))
        (cj/f4-clean-rebuild)
        (should (string= received-root root))))))

(ert-deftest test-dev-fkeys-f4-clean-rebuild-interpreted-project-skips-impl ()
  "Normal: on an interpreted project, the impl handler is not invoked."
  (test-dev-fkeys-cr-w--with-project '("pyproject.toml")
    (let ((calls 0))
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'cj/--f4-clean-rebuild-impl)
                 (lambda (_r) (cl-incf calls))))
        (cj/f4-clean-rebuild)
        (should (= calls 0))))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f4-clean-rebuild-unknown-project-skips-impl ()
  "Boundary: outside any project, the impl handler is not invoked."
  (let ((calls 0))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () nil))
              ((symbol-function 'cj/--f4-clean-rebuild-impl)
               (lambda (_r) (cl-incf calls))))
      (cj/f4-clean-rebuild)
      (should (= calls 0)))))

(provide 'test-dev-fkeys--f4-clean-rebuild)
;;; test-dev-fkeys--f4-clean-rebuild.el ends here
