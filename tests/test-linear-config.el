;;; test-linear-config.el --- Tests for linear-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the lazy API-key loader and the keybinding wiring.  linear-emacs
;; itself is never loaded here (it's a deferred :load-path package), so
;; `linear-emacs-api-key' is declared special below to make the dynamic
;; let-bindings reach `cj/linear--ensure-api-key'.  `cj/auth-source-secret-value'
;; is stubbed — no authinfo.gpg / GPG access in the tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'linear-config)

;; linear-config declares this with a bare (defvar linear-emacs-api-key), which
;; is only file-local; declare it special here so the let-bindings are dynamic.
(defvar linear-emacs-api-key nil)

(ert-deftest test-linear-ensure-api-key-loads-when-unset ()
  "Normal: an unset key is loaded from auth-source."
  (let ((linear-emacs-api-key nil))
    (cl-letf (((symbol-function 'cj/auth-source-secret-value)
               (lambda (&rest _) "lin_api_test")))
      (cj/linear--ensure-api-key)
      (should (equal linear-emacs-api-key "lin_api_test")))))

(ert-deftest test-linear-ensure-api-key-keeps-existing ()
  "Boundary: an already-set key is neither overwritten nor re-fetched."
  (let ((linear-emacs-api-key "already-set") (fetched nil))
    (cl-letf (((symbol-function 'cj/auth-source-secret-value)
               (lambda (&rest _) (setq fetched t) "other")))
      (cj/linear--ensure-api-key)
      (should (equal linear-emacs-api-key "already-set"))
      (should-not fetched))))

(ert-deftest test-linear-ensure-api-key-nil-when-absent ()
  "Boundary: a missing authinfo entry leaves the key nil without error."
  (let ((linear-emacs-api-key nil))
    (cl-letf (((symbol-function 'cj/auth-source-secret-value) (lambda (&rest _) nil)))
      (cj/linear--ensure-api-key)
      (should-not linear-emacs-api-key))))

(ert-deftest test-linear-keymap-bound-under-prefix ()
  "Smoke: C-; L holds the linear keymap and the entry commands are bound."
  (should (keymapp cj/linear-keymap))
  (should (eq (keymap-lookup (current-global-map) "C-; L") cj/linear-keymap))
  (should (eq (keymap-lookup cj/linear-keymap "l") #'linear-emacs-list-issues))
  (should (eq (keymap-lookup cj/linear-keymap "n") #'linear-emacs-new-issue)))

(provide 'test-linear-config)
;;; test-linear-config.el ends here
