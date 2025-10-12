;;; ledger-config.el --- Ledger Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; -------------------------------- Ledger Mode --------------------------------
;; edit files in ledger format

(use-package ledger-mode
  :mode ("\\.dat\\'"
         "\\.ledger\\'"
         "\\.journal\\'")
  :preface
  (defun cj/ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (save-excursion
      (when (buffer-modified-p)
        (with-demoted-errors (ledger-mode-clean-buffer))
        (save-buffer))))
  :bind
  (:map ledger-mode-map
        ("C-x C-s" . cj/ledger-save))
  :custom
  (ledger-clear-whole-transactions t)
  (ledger-reconcile-default-commodity "$")
  (ledger-report-use-header-line nil)
  (ledger-reports
   '(("bal"            "%(binary) --strict -f %(ledger-file) bal")
     ("bal this month" "%(binary) --strict -f %(ledger-file) bal -p %(month) -S amount")
     ("bal this year"  "%(binary) --strict -f %(ledger-file) bal -p 'this year'")
     ("net worth"      "%(binary) --strict -f %(ledger-file) bal Assets Liabilities")
     ("account"        "%(binary) --strict -f %(ledger-file) reg %(account)"))))

;; ------------------------------ Flycheck Ledger ------------------------------
;; syntax and unbalanced transaction linting

(use-package flycheck-ledger
  :after ledger-mode)

;; ------------------------------- Company Ledger ------------------------------
;; autocompletion for ledger

(use-package company-ledger
  :after (company ledger-mode)
  :config
  (add-to-list 'company-backends 'company-ledger))

(provide 'ledger-config)
;;; ledger-config.el ends here.
