;;; test-init-keymap-registration.el --- Keymap registration API contract -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 3 of the load-graph project introduces a small registration API in
;; keybindings.el so feature modules stop mutating `cj/custom-keymap' directly.
;; These tests pin the contract: a registered prefix map or command resolves
;; from `cj/custom-keymap', the which-key label is optional, and an invalid key
;; signals.  Each test rebinds `cj/custom-keymap' to a fresh keymap so the real
;; global prefix is never polluted.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)

(ert-deftest test-init-keymap-registration-prefix-map-resolves ()
  "Normal: a prefix map registered under KEY resolves from cj/custom-keymap."
  (let ((cj/custom-keymap (make-sparse-keymap))
        (sub (make-sparse-keymap)))
    (cj/register-prefix-map "a" sub "alpha")
    (should (eq sub (keymap-lookup cj/custom-keymap "a")))))

(ert-deftest test-init-keymap-registration-command-resolves ()
  "Normal: a command registered under KEY resolves from cj/custom-keymap."
  (let ((cj/custom-keymap (make-sparse-keymap)))
    (cj/register-command "b" #'ignore "beta")
    (should (eq #'ignore (keymap-lookup cj/custom-keymap "b")))))

(ert-deftest test-init-keymap-registration-label-optional ()
  "Boundary: registration without a LABEL still binds the key."
  (let ((cj/custom-keymap (make-sparse-keymap))
        (sub (make-sparse-keymap)))
    (cj/register-prefix-map "c" sub)
    (should (eq sub (keymap-lookup cj/custom-keymap "c")))))

(ert-deftest test-init-keymap-registration-invalid-key-signals ()
  "Error: a non-string key is rejected by the underlying keymap-set."
  (let ((cj/custom-keymap (make-sparse-keymap)))
    (should-error (cj/register-prefix-map 42 (make-sparse-keymap)))))

(provide 'test-init-keymap-registration)
;;; test-init-keymap-registration.el ends here
