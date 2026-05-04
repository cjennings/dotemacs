;;; test-auth-config-toggle-auth-source-debug.el --- Tests for debug toggle -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies the debug toggle helper flips both module and auth-source state.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'auth-source)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun test-auth-config-toggle--load ()
  "Load auth-config with external process calls stubbed."
  (setq cj/auth-source-debug-enabled nil)
  (setq auth-source-debug nil)
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _args) 0)))
    (load (expand-file-name "modules/auth-config.el" user-emacs-directory)
          nil t)))

(ert-deftest test-auth-config-toggle-normal-flips-disabled-to-enabled ()
  "Normal: toggle from disabled flips both vars to enabled."
  (test-auth-config-toggle--load)
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (cj/toggle-auth-source-debug)
    (should cj/auth-source-debug-enabled)
    (should auth-source-debug)))

(ert-deftest test-auth-config-toggle-boundary-two-toggles-return-to-disabled ()
  "Boundary: two toggles in a row leave both vars disabled."
  (test-auth-config-toggle--load)
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (cj/toggle-auth-source-debug)
    (cj/toggle-auth-source-debug)
    (should-not cj/auth-source-debug-enabled)
    (should-not auth-source-debug)))

(provide 'test-auth-config-toggle-auth-source-debug)
;;; test-auth-config-toggle-auth-source-debug.el ends here
