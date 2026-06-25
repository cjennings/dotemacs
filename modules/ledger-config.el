;;; ledger-config.el --- Ledger Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Editing support for ledger-format plain-text accounting files: ledger-mode,
;; flycheck linting, company completion, clean-on-save, and a small report set.
;; The reports and reconcile shell out to the `ledger' CLI; a load-time check
;; warns when it is missing rather than letting a report fail cryptically.

;;; Code:

;; ------------------------------- Declarations --------------------------------

(declare-function ledger-mode-clean-buffer "ledger-mode")
(declare-function cj/executable-find-or-warn "system-lib")
(defvar ledger-mode-map)
(defvar company-backends)

(defcustom cj/ledger-clean-on-save t
  "When non-nil, tidy a ledger buffer with `ledger-mode-clean-buffer' before save."
  :type 'boolean
  :group 'ledger)

;; -------------------------------- Ledger Mode --------------------------------
;; edit files in ledger format

(use-package ledger-mode
  :mode ("\\.dat\\'"
         "\\.ledger\\'"
         "\\.journal\\'")
  :preface
  (defun cj/ledger--clean-before-save ()
    "Tidy the ledger buffer before save when `cj/ledger-clean-on-save' is set.
Errors are demoted so a malformed buffer still saves."
    (when cj/ledger-clean-on-save
      (with-demoted-errors "Error cleaning ledger buffer: %S"
        (ledger-mode-clean-buffer))))
  (defun cj/ledger--enable-clean-on-save ()
    "Install the clean-on-save hook buffer-locally so it fires on every save path."
    (add-hook 'before-save-hook #'cj/ledger--clean-before-save nil t))
  :hook (ledger-mode . cj/ledger--enable-clean-on-save)
  :custom
  (ledger-clear-whole-transactions t)
  (ledger-reconcile-default-commodity "$")
  (ledger-report-use-header-line nil)
  (ledger-highlight-xact-under-point t)
  (ledger-reports
   '(("bal"            "%(binary) --strict -f %(ledger-file) bal")
     ("bal this month" "%(binary) --strict -f %(ledger-file) bal -p %(month) -S amount")
     ("bal this year"  "%(binary) --strict -f %(ledger-file) bal -p 'this year'")
     ("net worth"      "%(binary) --strict -f %(ledger-file) bal Assets Liabilities")
     ("account"        "%(binary) --strict -f %(ledger-file) reg %(account)")))
  :config
  (cj/executable-find-or-warn "ledger" 'ledger-mode))

;; ------------------------------ Flycheck Ledger ------------------------------
;; syntax and unbalanced-transaction linting

(use-package flycheck-ledger
  :after ledger-mode)

;; ------------------------------- Company Ledger ------------------------------
;; account/payee autocompletion for ledger

(use-package company-ledger
  :after (company ledger-mode)
  :config
  (add-to-list 'company-backends 'company-ledger))

(provide 'ledger-config)
;;; ledger-config.el ends here.
