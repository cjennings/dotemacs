;;; testutil-system-defaults.el --- Shared load harness for system-defaults tests -*- lexical-binding: t; -*-

;;; Commentary:

;; system-defaults.el has startup side effects (server start, coding-system
;; setup, recentf, file creation under `user-emacs-directory').  Tests that
;; assert one of its settings load the module fresh in a sandbox with those
;; external interactions stubbed, then inspect the variable they own.  This
;; file holds that harness so each system-defaults test file shares one copy.

;;; Code:

(require 'cl-lib)
(require 'autorevert)
(require 'bookmark)
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
  "Repository root, derived from this file's location under tests/.")

(defmacro test-system-defaults--with-load-environment (&rest body)
  "Run BODY with the side effects of loading system-defaults.el stubbed.
`user-emacs-directory', `user-home-dir', and `org-dir' are redirected to
throwaway temp directories so file creation lands in the sandbox."
  (declare (indent 0))
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

(defun test-system-defaults--load ()
  "Load system-defaults.el fresh from the repo for a smoke assertion.
Call inside `test-system-defaults--with-load-environment'."
  (load (expand-file-name "modules/system-defaults.el"
                          test-system-defaults--repo-root)
        nil t))

(provide 'testutil-system-defaults)
;;; testutil-system-defaults.el ends here
