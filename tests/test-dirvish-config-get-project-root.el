;;; test-dirvish-config-get-project-root.el --- Tests for cj/get-project-root -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/get-project-root' picks projectile when available, falls back to
;; project.el, and returns nil when neither finds a project.  These tests
;; stub the two underlying APIs (`projectile-project-root',
;; `project-current', `project-root') so the function can be exercised
;; without an actual project on disk.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

;; Make sure both APIs are at least fbound so the `fboundp' guards in
;; `cj/get-project-root' fire correctly under cl-letf.
(unless (fboundp 'projectile-project-root)
  (defalias 'projectile-project-root #'ignore))
(unless (fboundp 'project-current)
  (defalias 'project-current #'ignore))
(unless (fboundp 'project-root)
  (defalias 'project-root #'ignore))

(ert-deftest test-cj-get-project-root-projectile-wins ()
  "Normal: when projectile returns a root, that root is returned."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda (&rest _) "/home/me/code/proj/"))
            ((symbol-function 'project-current)
             (lambda (&rest _) (error "project-current should not be reached"))))
    (should (equal (cj/get-project-root) "/home/me/code/proj/"))))

(ert-deftest test-cj-get-project-root-falls-back-to-project-el ()
  "Normal: when projectile returns nil, project.el's root wins."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda (&rest _) nil))
            ((symbol-function 'project-current)
             (lambda (&rest _) '(vc Git "/home/me/repo/")))
            ((symbol-function 'project-root)
             (lambda (_) "/home/me/repo/")))
    (should (equal (cj/get-project-root) "/home/me/repo/"))))

(ert-deftest test-cj-get-project-root-projectile-errors-then-fallback ()
  "Boundary: projectile signaling an error is suppressed; fallback runs."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda (&rest _) (error "projectile not initialized")))
            ((symbol-function 'project-current)
             (lambda (&rest _) '(vc Git "/home/me/repo/")))
            ((symbol-function 'project-root)
             (lambda (_) "/home/me/repo/")))
    (should (equal (cj/get-project-root) "/home/me/repo/"))))

(ert-deftest test-cj-get-project-root-no-project-returns-nil ()
  "Boundary: neither projectile nor project.el find anything."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda (&rest _) nil))
            ((symbol-function 'project-current)
             (lambda (&rest _) nil)))
    (should-not (cj/get-project-root))))

(provide 'test-dirvish-config-get-project-root)
;;; test-dirvish-config-get-project-root.el ends here
