;;; test-dev-fkeys--projectile-advice-install.el --- Tests for Projectile advice setup -*- lexical-binding: t -*-

;;; Commentary:
;; Smoke tests for the load-order contract between dev-fkeys.el and Projectile.
;; Requiring dev-fkeys should not force Projectile to load, but the command-cache
;; revert advice should be installed once Projectile is available.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

(ert-deftest test-dev-fkeys-projectile-advice-register-defers-until-projectile-loads ()
  "When Projectile is not loaded, registration should use `eval-after-load'."
  (let (registered-feature registered-form install-called)
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest _) (and (not (eq feature 'projectile))
                                      (featurep feature))))
              ((symbol-function 'eval-after-load)
               (lambda (feature form)
                 (setq registered-feature feature
                       registered-form form)))
              ((symbol-function 'cj/--projectile-install-revert-advice)
               (lambda () (setq install-called t))))
      (cj/--projectile-register-revert-advice))
    (should (eq registered-feature 'projectile))
    (should (equal registered-form '(cj/--projectile-install-revert-advice)))
    (should-not install-called)))

(ert-deftest test-dev-fkeys-projectile-advice-register-installs-when-projectile-loaded ()
  "When Projectile is already loaded, registration should install immediately."
  (let (install-called eval-after-load-called)
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest _) (eq feature 'projectile)))
              ((symbol-function 'eval-after-load)
               (lambda (&rest _args) (setq eval-after-load-called t)))
              ((symbol-function 'cj/--projectile-install-revert-advice)
               (lambda () (setq install-called t))))
      (cj/--projectile-register-revert-advice))
    (should install-called)
    (should-not eval-after-load-called)))

(ert-deftest test-dev-fkeys-projectile-advice-install-skips-unbound-projectile-functions ()
  "The installer should not advise Projectile functions that are not bound."
  (let (advised)
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (and (not (memq symbol '(projectile-compile-project
                                           projectile-test-project
                                           projectile-run-project)))
                      (fboundp symbol))))
              ((symbol-function 'advice-add)
               (lambda (symbol &rest _args)
                 (push symbol advised))))
      (cj/--projectile-install-revert-advice))
    (should-not advised)))

(ert-deftest test-dev-fkeys-projectile-advice-install-advises-bound-projectile-functions ()
  "The installer should advise each available Projectile command runner."
  (let (advised)
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (or (memq symbol '(projectile-compile-project
                                     projectile-test-project
                                     projectile-run-project))
                     (fboundp symbol))))
              ((symbol-function 'advice-member-p)
               (lambda (&rest _args) nil))
              ((symbol-function 'advice-add)
               (lambda (symbol _where function &rest _args)
                 (push (list symbol function) advised))))
      (cj/--projectile-install-revert-advice))
    (should (member '(projectile-compile-project
                      cj/--projectile-compile-around-revert)
                    advised))
    (should (member '(projectile-test-project
                      cj/--projectile-test-around-revert)
                    advised))
    (should (member '(projectile-run-project
                      cj/--projectile-run-around-revert)
                    advised))))

(provide 'test-dev-fkeys--projectile-advice-install)
;;; test-dev-fkeys--projectile-advice-install.el ends here
