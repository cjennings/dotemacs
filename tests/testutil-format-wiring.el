;;; testutil-format-wiring.el --- Shared helpers for formatter wiring tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers used by the per-language formatter-wiring tests.  Two pieces:
;;
;; - `format-test--ensure-packages-init' bootstraps `package' once per
;;   batch run so use-package forms in the prog-* modules can find
;;   their packages in elpa.  The Makefile's test target invokes Emacs
;;   with --no-site-file --no-site-lisp, which skips the implicit
;;   `package-initialize' that an interactive session would do at
;;   startup.
;;
;; - `format-test--skip-unless-executable' is a thin wrapper around
;;   `ert-skip' for the "formatter not installed on this machine" path.
;;   Tests that need the underlying binary on PATH call this first.

;;; Code:

(require 'ert)
(require 'package)

(defvar format-test--packages-initialized nil
  "Non-nil once `package-initialize' has run in this batch process.")

(defun format-test--ensure-packages-init ()
  "Initialise `package' the first time this is called in a batch run."
  (unless format-test--packages-initialized
    (package-initialize)
    (setq format-test--packages-initialized t)))

(defun format-test--skip-unless-executable (program)
  "Skip the current ERT test unless PROGRAM is on PATH."
  (unless (executable-find program)
    (ert-skip (format "%s not on PATH; skipping installation-dependent assertion"
                      program))))

(provide 'testutil-format-wiring)
;;; testutil-format-wiring.el ends here
