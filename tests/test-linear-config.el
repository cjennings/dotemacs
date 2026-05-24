;;; test-linear-config.el --- Tests for linear-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the lazy API-key loader and the keybinding wiring.  pearl
;; itself is never loaded here (it's a deferred :load-path package), so
;; `pearl-api-key' is declared special below to make the dynamic
;; let-bindings reach `cj/linear--ensure-api-key'.  `cj/auth-source-secret-value'
;; is stubbed — no authinfo.gpg / GPG access in the tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'linear-config)

;; linear-config declares this with a bare (defvar pearl-api-key), which
;; is only file-local; declare it special here so the let-bindings are dynamic.
(defvar pearl-api-key nil)

(ert-deftest test-linear-ensure-api-key-loads-when-unset ()
  "Normal: an unset key is loaded from auth-source."
  (let ((pearl-api-key nil))
    (cl-letf (((symbol-function 'cj/auth-source-secret-value)
               (lambda (&rest _) "lin_api_test")))
      (cj/linear--ensure-api-key)
      (should (equal pearl-api-key "lin_api_test")))))

(ert-deftest test-linear-ensure-api-key-keeps-existing ()
  "Boundary: an already-set key is neither overwritten nor re-fetched."
  (let ((pearl-api-key "already-set") (fetched nil))
    (cl-letf (((symbol-function 'cj/auth-source-secret-value)
               (lambda (&rest _) (setq fetched t) "other")))
      (cj/linear--ensure-api-key)
      (should (equal pearl-api-key "already-set"))
      (should-not fetched))))

(ert-deftest test-linear-ensure-api-key-nil-when-absent ()
  "Boundary: a missing authinfo entry leaves the key nil without error."
  (let ((pearl-api-key nil))
    (cl-letf (((symbol-function 'cj/auth-source-secret-value) (lambda (&rest _) nil)))
      (cj/linear--ensure-api-key)
      (should-not pearl-api-key))))

(ert-deftest test-linear-install-key-advice-loads-before-check-setup ()
  "Error-regression: `pearl-check-setup' loads the key before reading it.
The lazy loader originally only advised the GraphQL request entry point, so
`check-setup' — which reads `pearl-api-key' directly without making a
request — falsely reported \"not set\" on a fresh session."
  (let ((pearl-api-key nil)
        (key-at-read :unread))
    (cl-letf (((symbol-function 'cj/auth-source-secret-value)
               (lambda (&rest _) "lin_api_test"))
              ((symbol-function 'pearl--graphql-request-async)
               (lambda (&rest _) nil))
              ((symbol-function 'pearl-check-setup)
               (lambda () (setq key-at-read pearl-api-key))))
      (cj/linear--install-key-advice)
      (unwind-protect
          (progn
            (pearl-check-setup)
            (should (equal key-at-read "lin_api_test")))
        (advice-remove 'pearl--graphql-request-async
                       #'cj/linear--ensure-key-before)
        (advice-remove 'pearl-check-setup
                       #'cj/linear--ensure-key-before)))))

(ert-deftest test-linear-keymap-bound-under-prefix ()
  "Smoke: C-; L holds the linear keymap and the entry commands are bound."
  (should (keymapp cj/linear-keymap))
  (should (eq (keymap-lookup (current-global-map) "C-; L") cj/linear-keymap))
  (should (eq (keymap-lookup cj/linear-keymap "l") #'pearl-list-issues))
  (should (eq (keymap-lookup cj/linear-keymap "n") #'pearl-new-issue))
  ;; commands added in the package rework
  (should (eq (keymap-lookup cj/linear-keymap "f") #'pearl-list-issues-filtered))
  (should (eq (keymap-lookup cj/linear-keymap "v") #'pearl-run-view))
  (should (eq (keymap-lookup cj/linear-keymap "o") #'pearl-open-current-issue)))

(ert-deftest test-linear-edit-submap-bound ()
  "Smoke: C-; L e holds the edit-issue sub-keymap with field commands."
  (should (keymapp cj/linear-edit-keymap))
  (should (eq (keymap-lookup cj/linear-keymap "e") cj/linear-edit-keymap))
  (should (eq (keymap-lookup cj/linear-edit-keymap "a") #'pearl-set-assignee))
  (should (eq (keymap-lookup cj/linear-edit-keymap "s") #'pearl-set-state)))

(provide 'test-linear-config)
;;; test-linear-config.el ends here
