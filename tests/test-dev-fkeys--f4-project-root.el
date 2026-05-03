;;; test-dev-fkeys--f4-project-root.el --- Tests for cj/--f4-project-root -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the projectile-root wrapper. Projectile's
;; `projectile-project-root' raises an error in some configurations when
;; called outside a known project; this wrapper degrades to nil so the F4
;; dispatcher can route to the 'unknown branch instead of crashing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-f4-project-root-returns-projectile-value ()
  "Normal: returns whatever `projectile-project-root' returns when fbound."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () "/some/project/")))
    (should (string= (cj/--f4-project-root) "/some/project/"))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f4-project-root-returns-nil-when-projectile-unbound ()
  "Boundary: returns nil when projectile is not loaded.

Components integrated:
- `cj/--f4-project-root' (unit under test)
- `projectile-project-root' (MOCKED — fmakunbound to simulate absence)"
  (let ((had-projectile (fboundp 'projectile-project-root))
        (saved (and (fboundp 'projectile-project-root)
                    (symbol-function 'projectile-project-root))))
    (unwind-protect
        (progn
          (when had-projectile (fmakunbound 'projectile-project-root))
          (should (null (cj/--f4-project-root))))
      (when had-projectile
        (fset 'projectile-project-root saved)))))

;;; Error Cases

(ert-deftest test-dev-fkeys-f4-project-root-returns-nil-on-projectile-error ()
  "Error: returns nil when `projectile-project-root' signals an error."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () (error "Outside a known project"))))
    (should (null (cj/--f4-project-root)))))

(provide 'test-dev-fkeys--f4-project-root)
;;; test-dev-fkeys--f4-project-root.el ends here
