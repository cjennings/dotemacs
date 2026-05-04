;;; test-auth-config-debug.el --- Tests for auth-source debug defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensures credential lookup debug logging is opt-in.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'auth-source)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun test-auth-config-debug--load ()
  "Load auth-config while stubbing external process calls."
  (setq cj/auth-source-debug-enabled nil)
  (setq auth-source-debug :before-load)
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _args) 0)))
    (load (expand-file-name "modules/auth-config.el" user-emacs-directory)
          nil t)))

(ert-deftest test-auth-config-debug-defaults-to-disabled ()
  "Loading auth-config should leave auth-source debug logging disabled."
  (test-auth-config-debug--load)
  (should-not cj/auth-source-debug-enabled)
  (should-not auth-source-debug))

(ert-deftest test-auth-config-debug-toggle-updates-auth-source ()
  "The troubleshooting toggle should update both public debug variables."
  (test-auth-config-debug--load)
  (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
    (cj/set-auth-source-debug t)
    (should cj/auth-source-debug-enabled)
    (should auth-source-debug)
    (cj/set-auth-source-debug nil)
    (should-not cj/auth-source-debug-enabled)
    (should-not auth-source-debug)))

(provide 'test-auth-config-debug)
;;; test-auth-config-debug.el ends here
