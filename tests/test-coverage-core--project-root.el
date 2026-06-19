;;; test-coverage-core--project-root.el --- Tests for cj/--coverage-project-root -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for `cj/--coverage-project-root' in coverage-core.el — returns the
;; projectile project root when available, else `default-directory'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

;;; Normal Cases

(ert-deftest test-coverage-project-root-uses-projectile-when-available ()
  "Normal: with projectile available and in a project, returns its root."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () "/home/u/proj/")))
    (should (equal (cj/--coverage-project-root) "/home/u/proj/"))))

;;; Boundary Cases

(ert-deftest test-coverage-project-root-falls-back-when-projectile-absent ()
  "Boundary: with no projectile function, falls back to default-directory."
  (cl-letf (((symbol-function 'projectile-project-root) nil))
    (let ((default-directory "/fallback/dir/"))
      (should (equal (cj/--coverage-project-root) "/fallback/dir/")))))

(ert-deftest test-coverage-project-root-falls-back-when-not-in-project ()
  "Boundary: projectile present but returns nil (not in a project) falls back."
  (cl-letf (((symbol-function 'projectile-project-root) (lambda () nil)))
    (let ((default-directory "/fallback/dir/"))
      (should (equal (cj/--coverage-project-root) "/fallback/dir/")))))

(provide 'test-coverage-core--project-root)
;;; test-coverage-core--project-root.el ends here
