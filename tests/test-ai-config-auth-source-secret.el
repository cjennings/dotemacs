;;; test-ai-config-auth-source-secret.el --- Tests for the required-secret wrapper -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/auth-source-secret' is the required-secret layer over the shared
;; `cj/auth-source-secret-value' primitive: it returns the secret, or errors
;; when none is found.  These tests stub the primitive to exercise both paths.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-config)

(ert-deftest test-ai-config-auth-source-secret-returns-value ()
  "Normal: returns the value the primitive resolves."
  (cl-letf (((symbol-function 'cj/auth-source-secret-value) (lambda (&rest _) "sk-x")))
    (should (equal "sk-x" (cj/auth-source-secret "api.example.com" "apikey")))))

(ert-deftest test-ai-config-auth-source-secret-errors-on-miss ()
  "Error: signals when the primitive finds no secret."
  (cl-letf (((symbol-function 'cj/auth-source-secret-value) (lambda (&rest _) nil)))
    (should-error (cj/auth-source-secret "api.example.com" "apikey"))))

(provide 'test-ai-config-auth-source-secret)
;;; test-ai-config-auth-source-secret.el ends here
