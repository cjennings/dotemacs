;;; test-dev-fkeys--f4-compile-and-run.el --- Smoke tests for cj/f4-compile-and-run -*- lexical-binding: t -*-

;;; Commentary:
;; Smoke tests for the interactive F4 wrapper. The wrapper:
;;
;; 1. Resolves the project root.
;; 2. Detects project type from markers.
;; 3. Builds the candidate list.
;; 4. Prompts via completing-read.
;; 5. Looks up the chosen label's action.
;; 6. Dispatches.
;;
;; Per the elisp-testing rule on Interactive vs Internal split, the heavy
;; lifting is in `cj/--f4-dispatch' and the helpers — those are tested
;; directly in their own files. This file just confirms the wrapper wires
;; the pieces together: prompt fires, the chosen label routes to the right
;; action symbol.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defmacro test-dev-fkeys-f4--with-project (markers &rest body)
  "Set up a temp project with MARKERS, bind ROOT, run BODY, clean up."
  (declare (indent 1))
  `(let ((root (make-temp-file "test-dev-fkeys-f4-" t)))
     (unwind-protect
         (progn
           (dolist (marker ,markers)
             (write-region "" nil (expand-file-name marker root)))
           ,@body)
       (delete-directory root t))))

;;; Normal Cases

(ert-deftest test-dev-fkeys-f4-compile-and-run-prompts-with-completing-read ()
  "Normal: the wrapper invokes completing-read with the project's candidate labels.

Components integrated:
- `cj/f4-compile-and-run' (unit under test)
- `cj/--f4-project-root' (MOCKED — returns the temp project root)
- `cj/--detect-project-type' (real)
- `cj/--f4-candidates' (real)
- `completing-read' (MOCKED — captures candidates and returns the default)
- `cj/--f4-dispatch' (MOCKED — captures the action it received)"
  (test-dev-fkeys-f4--with-project '("go.mod")
    (let (seen-candidates seen-action)
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq seen-candidates collection)
                   (car collection)))
                ((symbol-function 'cj/--f4-dispatch)
                 (lambda (action) (setq seen-action action))))
        (cj/f4-compile-and-run)
        ;; Compiled-project candidates are the four labels.
        (should (member "Compile + Run" seen-candidates))
        (should (member "Compile" seen-candidates))
        (should (member "Run" seen-candidates))
        (should (member "Clean + Rebuild" seen-candidates))
        ;; Default (first label) is "Compile + Run", which routes to
        ;; the compile-and-run action.
        (should (eq seen-action 'compile-and-run))))))

(ert-deftest test-dev-fkeys-f4-compile-and-run-routes-chosen-label-to-action ()
  "Normal: a non-default label selection routes to the right action."
  (test-dev-fkeys-f4--with-project '("Cargo.toml")
    (let (seen-action)
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "Run"))
                ((symbol-function 'cj/--f4-dispatch)
                 (lambda (action) (setq seen-action action))))
        (cj/f4-compile-and-run)
        (should (eq seen-action 'run-only))))))

(ert-deftest test-dev-fkeys-f4-compile-and-run-interpreted-project-shows-run-only ()
  "Normal: an interpreted project's menu has only the Run candidate."
  (test-dev-fkeys-f4--with-project '("pyproject.toml")
    (let (seen-candidates)
      (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () root))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq seen-candidates collection)
                   (car collection)))
                ((symbol-function 'cj/--f4-dispatch) (lambda (_action) nil)))
        (cj/f4-compile-and-run)
        (should (equal seen-candidates '("Run")))))))

(provide 'test-dev-fkeys--f4-compile-and-run)
;;; test-dev-fkeys--f4-compile-and-run.el ends here
