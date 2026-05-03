;;; test-dev-fkeys--projectile-around-revert.el --- Tests for cj/--projectile-around-revert -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the around-advice that wires the capture / finish-hook pair
;; to projectile cmd runners. The advice:
;;
;; 1. Captures the prior cached cmd via `cj/--projectile-capture-cmd'.
;; 2. Adds `cj/--projectile-revert-on-fail' to `compilation-finish-functions'.
;; 3. Calls ORIG-FN with ARGS so projectile's normal flow proceeds.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(defvar projectile-compile-cmd-map nil)

;;; Normal Cases

(ert-deftest test-dev-fkeys-projectile-around-revert-invokes-orig-fn ()
  "Normal: advice calls the wrapped function with its args."
  (let ((calls nil)
        (cj/--projectile-revert-state nil)
        (compilation-finish-functions nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (cj/--projectile-around-revert
       'projectile-compile-cmd-map
       (lambda (&rest args) (push args calls))
       'arg1 'arg2))
    (should (equal calls '((arg1 arg2))))))

(ert-deftest test-dev-fkeys-projectile-around-revert-captures-prior ()
  "Normal: advice captures the prior cmd into the revert state."
  (let ((cj/--projectile-revert-state nil)
        (compilation-finish-functions nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal)))
    (puthash "/p/" "make build" projectile-compile-cmd-map)
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (cj/--projectile-around-revert
       'projectile-compile-cmd-map
       (lambda (&rest _) nil)))
    (should (equal (plist-get cj/--projectile-revert-state :prior)
                   "make build"))))

(ert-deftest test-dev-fkeys-projectile-around-revert-installs-finish-hook ()
  "Normal: advice adds the revert-on-fail hook to compilation-finish-functions."
  (let ((cj/--projectile-revert-state nil)
        (compilation-finish-functions nil)
        (projectile-compile-cmd-map (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () "/p/")))
      (cj/--projectile-around-revert
       'projectile-compile-cmd-map
       (lambda (&rest _) nil)))
    (should (member #'cj/--projectile-revert-on-fail
                    compilation-finish-functions))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-projectile-around-revert-no-project-still-runs-orig-fn ()
  "Boundary: no project root → capture is a no-op, orig-fn still runs.
The state stays nil so the finish hook will be a no-op too."
  (let ((calls 0)
        (cj/--projectile-revert-state nil)
        (compilation-finish-functions nil))
    (cl-letf (((symbol-function 'cj/--f4-project-root) (lambda () nil)))
      (cj/--projectile-around-revert
       'projectile-compile-cmd-map
       (lambda (&rest _) (cl-incf calls))))
    (should (= calls 1))
    (should (null cj/--projectile-revert-state))))

(provide 'test-dev-fkeys--projectile-around-revert)
;;; test-dev-fkeys--projectile-around-revert.el ends here
