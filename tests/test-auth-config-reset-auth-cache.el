;;; test-auth-config-reset-auth-cache.el --- Tests for cj/reset-auth-cache -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies cache-reset orchestrates auth-source, EPA, and gpg-agent
;; clears according to the prefix-arg flag.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'auth-source)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun test-auth-config-reset--load ()
  "Load auth-config with external process calls stubbed."
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _args) 0)))
    (load (expand-file-name "modules/auth-config.el" user-emacs-directory)
          nil t)))

(ert-deftest test-auth-config-reset-auth-cache-normal-no-prefix-skips-shell ()
  "Normal: no prefix arg clears Emacs caches but does not invoke shell-command."
  (test-auth-config-reset--load)
  (let ((auth-clears 0)
        (epa-clears 0)
        (shell-calls 0))
    (cl-letf (((symbol-function 'auth-source-forget-all-cached)
               (lambda () (cl-incf auth-clears)))
              ((symbol-function 'epa-file-clear-cache)
               (lambda () (cl-incf epa-clears)))
              ((symbol-function 'shell-command)
               (lambda (&rest _) (cl-incf shell-calls) 0))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (cj/reset-auth-cache nil)
      (should (= auth-clears 1))
      (should (= epa-clears 1))
      (should (zerop shell-calls)))))

(ert-deftest test-auth-config-reset-auth-cache-boundary-with-prefix-shell-success ()
  "Boundary: with prefix and shell-command returning 0, all three caches clear."
  (test-auth-config-reset--load)
  (let ((auth-clears 0)
        (epa-clears 0)
        (shell-calls 0)
        (last-message nil))
    (cl-letf (((symbol-function 'auth-source-forget-all-cached)
               (lambda () (cl-incf auth-clears)))
              ((symbol-function 'epa-file-clear-cache)
               (lambda () (cl-incf epa-clears)))
              ((symbol-function 'shell-command)
               (lambda (&rest _) (cl-incf shell-calls) 0))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq last-message fmt))))
      (cj/reset-auth-cache t)
      (should (= auth-clears 1))
      (should (= epa-clears 1))
      (should (= shell-calls 1))
      (should (string-match-p "Emacs and gpg-agent caches cleared" last-message)))))

(ert-deftest test-auth-config-reset-auth-cache-error-with-prefix-shell-fails ()
  "Error: with prefix and shell-command non-zero, warning is shown, Emacs caches still cleared."
  (test-auth-config-reset--load)
  (let ((auth-clears 0)
        (epa-clears 0)
        (last-message nil))
    (cl-letf (((symbol-function 'auth-source-forget-all-cached)
               (lambda () (cl-incf auth-clears)))
              ((symbol-function 'epa-file-clear-cache)
               (lambda () (cl-incf epa-clears)))
              ((symbol-function 'shell-command)
               (lambda (&rest _) 1))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq last-message fmt))))
      (cj/reset-auth-cache t)
      (should (= auth-clears 1))
      (should (= epa-clears 1))
      (should (string-match-p "Failed to clear gpg-agent cache" last-message)))))

(provide 'test-auth-config-reset-auth-cache)
;;; test-auth-config-reset-auth-cache.el ends here
