;;; test-ledger-config.el --- Characterization tests for ledger-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Captures the behavior ledger-config.el has today, before any guardrail work
;; changes it.  See docs/design/2026-07-10-ledger-config-audit.org for the audit
;; these tests pin.
;;
;; The clean-on-save helpers are defined in the `use-package' `:preface', which
;; use-package emits unconditionally, so they exist under `make test' even though
;; ledger-mode itself never loads there (no `package-initialize' in the test run).
;;
;; `ledger-mode-clean-buffer' is stubbed: it is the boundary this config delegates
;; to, and it rewrites the whole buffer.  What these tests pin is whether our hook
;; calls it, not what it does.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ledger-config)

(ert-deftest test-ledger-config-clean-before-save-cleans-when-enabled ()
  "Normal: with `cj/ledger-clean-on-save' set, the save hook cleans the buffer."
  (let ((called 0)
        (cj/ledger-clean-on-save t))
    (cl-letf (((symbol-function 'ledger-mode-clean-buffer)
               (lambda (&rest _) (setq called (1+ called)))))
      (cj/ledger--clean-before-save))
    (should (= 1 called))))

(ert-deftest test-ledger-config-clean-before-save-skips-when-disabled ()
  "Boundary: with `cj/ledger-clean-on-save' nil, the save hook does nothing."
  (let ((called 0)
        (cj/ledger-clean-on-save nil))
    (cl-letf (((symbol-function 'ledger-mode-clean-buffer)
               (lambda (&rest _) (setq called (1+ called)))))
      (cj/ledger--clean-before-save))
    (should (= 0 called))))

(ert-deftest test-ledger-config-clean-before-save-demotes-errors ()
  "Error: a failing clean does not signal, so the file still saves.
This is the current contract.  It also means a clean that fails partway
leaves the buffer in whatever state it reached, because nothing rolls back."
  (let ((cj/ledger-clean-on-save t)
        (inhibit-message t))
    (cl-letf (((symbol-function 'ledger-mode-clean-buffer)
               (lambda (&rest _) (error "boom"))))
      (should (progn (cj/ledger--clean-before-save) t)))))

(ert-deftest test-ledger-config-enable-clean-on-save-is-buffer-local ()
  "Normal: the hook installs buffer-locally, not globally."
  (let ((global-before (default-value 'before-save-hook)))
    (with-temp-buffer
      (cj/ledger--enable-clean-on-save)
      (should (memq #'cj/ledger--clean-before-save before-save-hook))
      (should-not (memq #'cj/ledger--clean-before-save
                        (default-value 'before-save-hook))))
    (should (equal global-before (default-value 'before-save-hook)))))

(ert-deftest test-ledger-config-clean-on-save-defaults-on ()
  "Normal: clean-on-save ships enabled.
Pinned because the audit questions whether a whole-buffer sort belongs on
every save of a financial file.  If that default flips, this test should
fail and be updated deliberately."
  (should (eq t (default-value 'cj/ledger-clean-on-save))))

(provide 'test-ledger-config)
;;; test-ledger-config.el ends here
