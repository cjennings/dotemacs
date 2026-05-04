;;; test-system-defaults-vc-follow-symlinks.el --- Tests for VC symlink default -*- lexical-binding: t; -*-

;;; Commentary:

;; system-defaults.el has startup side effects, so load it with unrelated
;; external interactions stubbed and assert the setting this file owns.

;;; Code:

(require 'cl-lib)
(require 'autorevert)
(require 'bookmark)
(require 'ert)
(require 'server)
(require 'vc-hooks)

(defvar org-dir nil
  "Test binding for system-defaults bookmark path setup.")

(defvar user-home-dir nil
  "Test binding for system-defaults default directory setup.")

(defvar use-package-always-ensure nil
  "Test binding for use-package package installation policy.")

(defconst test-system-defaults--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for system-defaults tests.")

(defmacro test-system-defaults--with-load-environment (&rest body)
  "Run BODY with side effects from system-defaults.el stubbed."
  `(let ((user-emacs-directory (file-name-as-directory
                                (make-temp-file "system-defaults-emacs-" t)))
         (user-home-dir (file-name-as-directory
                         (make-temp-file "system-defaults-home-" t)))
         (org-dir (file-name-as-directory
                   (make-temp-file "system-defaults-org-" t)))
         (use-package-always-ensure nil))
     (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) t))
               ((symbol-function 'server-start) #'ignore)
               ((symbol-function 'set-locale-environment) #'ignore)
               ((symbol-function 'prefer-coding-system) #'ignore)
               ((symbol-function 'set-default-coding-systems) #'ignore)
               ((symbol-function 'set-terminal-coding-system) #'ignore)
               ((symbol-function 'set-keyboard-coding-system) #'ignore)
               ((symbol-function 'set-selection-coding-system) #'ignore)
               ((symbol-function 'set-charset-priority) #'ignore)
               ((symbol-function 'global-auto-revert-mode) #'ignore)
               ((symbol-function 'recentf-mode) #'ignore))
       (unless (fboundp 'use-package)
         (defmacro use-package (&rest _args) nil))
       ,@body)))

(ert-deftest test-system-defaults-vc-follow-symlinks-normal-sets-t ()
  "Normal: system-defaults follows version-controlled symlinks without asking."
  (test-system-defaults--with-load-environment
   (let ((vc-follow-symlinks nil))
     (load (expand-file-name "modules/system-defaults.el"
                             test-system-defaults--repo-root)
           nil t)
     (should (eq vc-follow-symlinks t)))))

(provide 'test-system-defaults-vc-follow-symlinks)
;;; test-system-defaults-vc-follow-symlinks.el ends here
