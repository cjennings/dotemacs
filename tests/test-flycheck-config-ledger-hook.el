;;; test-flycheck-config-ledger-hook.el --- flycheck reaches ledger buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; `flycheck-ledger' registers a `ledger' checker, but a checker only runs where
;; `flycheck-mode' is on.  Until 2026-07-10 flycheck-config enabled the mode in
;; `sh-mode' and `emacs-lisp-mode' only, and no `global-flycheck-mode' existed, so
;; an unbalanced transaction in a ledger file produced no warning at all.
;;
;; These tests pin the hook, not the checker.  Whether the `ledger' checker itself
;; works is flycheck-ledger's problem; whether it ever gets a chance to run is ours.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'flycheck-config)

(ert-deftest test-flycheck-config-enables-flycheck-in-ledger-buffers ()
  "Normal: opening a ledger buffer turns `flycheck-mode' on.
Without this, `flycheck-ledger' is loaded, its checker is registered, and
nothing ever lints a financial file."
  (should (memq #'flycheck-mode (default-value 'ledger-mode-hook))))

(ert-deftest test-flycheck-config-keeps-its-existing-mode-hooks ()
  "Boundary: adding ledger doesn't displace the modes flycheck already covered."
  (should (memq #'flycheck-mode (default-value 'sh-mode-hook)))
  (should (memq #'flycheck-mode (default-value 'emacs-lisp-mode-hook))))

(provide 'test-flycheck-config-ledger-hook)
;;; test-flycheck-config-ledger-hook.el ends here
