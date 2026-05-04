;;; test-auth-config-kill-gpg-agent.el --- Tests for cj/kill-gpg-agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies cj/kill-gpg-agent reports success or warning based on shell exit code.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'auth-source)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun test-auth-config-kill--load ()
  "Load auth-config with external process calls stubbed."
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _args) 0)))
    (load (expand-file-name "modules/auth-config.el" user-emacs-directory)
          nil t)))

(ert-deftest test-auth-config-kill-gpg-agent-normal-shell-success ()
  "Normal: shell-command exit 0 yields success message."
  (test-auth-config-kill--load)
  (let ((last-message nil))
    (cl-letf (((symbol-function 'shell-command) (lambda (&rest _) 0))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq last-message fmt))))
      (cj/kill-gpg-agent)
      (should (string-match-p "gpg-agent killed" last-message)))))

(ert-deftest test-auth-config-kill-gpg-agent-error-shell-fails ()
  "Error: shell-command non-zero yields warning message."
  (test-auth-config-kill--load)
  (let ((last-message nil))
    (cl-letf (((symbol-function 'shell-command) (lambda (&rest _) 1))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq last-message fmt))))
      (cj/kill-gpg-agent)
      (should (string-match-p "Failed to kill gpg-agent" last-message)))))

(provide 'test-auth-config-kill-gpg-agent)
;;; test-auth-config-kill-gpg-agent.el ends here
