;;; test-dev-fkeys--f6-test-runner.el --- Smoke tests for cj/f6-test-runner -*- lexical-binding: t -*-

;;; Commentary:
;; Smoke tests for the F6 top-level menu. The wrapper:
;;
;; 1. Prompts via completing-read with two candidates.
;; 2. \"All tests\" invokes `projectile-test-project'.
;; 3. \"Current file's tests\" invokes `cj/--f6-current-file-tests-impl'
;;    with the buffer's file and the project root.
;;
;; The orchestrator and helpers are tested in their own files; this file
;; just confirms the wiring.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-f6-test-runner-prompts-with-completing-read ()
  "Normal: F6 prompts with the two-entry candidate list."
  (let (seen-candidates)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq seen-candidates collection)
                 (car collection)))
              ((symbol-function 'projectile-test-project)
               (lambda (_arg) nil))
              ((symbol-function 'cj/--f4-project-root) (lambda () "/p/"))
              ((symbol-function 'cj/--f6-current-file-tests-impl)
               (lambda (_f _r) nil)))
      (cj/f6-test-runner)
      (should (member "All tests" seen-candidates))
      (should (member "Current file's tests" seen-candidates)))))

(ert-deftest test-dev-fkeys-f6-test-runner-all-tests-routes-to-projectile ()
  "Normal: choosing 'All tests' invokes projectile-test-project."
  (let ((calls 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "All tests"))
              ((symbol-function 'projectile-test-project)
               (lambda (_arg) (cl-incf calls)))
              ((symbol-function 'cj/--f4-project-root) (lambda () "/p/"))
              ((symbol-function 'cj/--f6-current-file-tests-impl)
               (lambda (_f _r) nil)))
      (cj/f6-test-runner)
      (should (= calls 1)))))

(ert-deftest test-dev-fkeys-f6-test-runner-all-tests-propagates-prefix-arg ()
  "Normal: choosing 'All tests' forwards `current-prefix-arg' to
projectile-test-project so `C-u F6 → All tests' forces a re-prompt."
  (let ((seen-arg 'unset)
        (current-prefix-arg t))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "All tests"))
              ((symbol-function 'projectile-test-project)
               (lambda (arg) (setq seen-arg arg)))
              ((symbol-function 'cj/--f4-project-root) (lambda () "/p/"))
              ((symbol-function 'cj/--f6-current-file-tests-impl)
               (lambda (_f _r) nil)))
      (cj/f6-test-runner)
      (should (eq seen-arg t)))))

(ert-deftest test-dev-fkeys-f6-test-runner-current-file-routes-to-impl ()
  "Normal: choosing 'Current file's tests' invokes the orchestrator with
the buffer file and projectile root.

Components integrated:
- `cj/f6-test-runner' (unit under test)
- `completing-read' (MOCKED — picks the second label)
- `cj/--f4-project-root' (MOCKED — fixed root)
- `cj/--f6-current-file-tests-impl' (MOCKED — captures args)
- `buffer-file-name' (MOCKED via cl-letf)"
  (let (seen-file seen-root)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Current file's tests"))
              ((symbol-function 'projectile-test-project) (lambda (_arg) nil))
              ((symbol-function 'cj/--f4-project-root) (lambda () "/p/"))
              ((symbol-function 'buffer-file-name) (lambda () "/p/foo.el"))
              ((symbol-function 'cj/--f6-current-file-tests-impl)
               (lambda (file root)
                 (setq seen-file file seen-root root))))
      (cj/f6-test-runner)
      (should (string= seen-file "/p/foo.el"))
      (should (string= seen-root "/p/")))))

(provide 'test-dev-fkeys--f6-test-runner)
;;; test-dev-fkeys--f6-test-runner.el ends here
