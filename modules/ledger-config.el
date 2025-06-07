;;; ledger-config.el --- Ledger Configuration -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Use C-c ? in ledger mode for discoverability.

;; Unlike Mu4e and Elfeed (with Elfeed dashboard), there's no landing page for
;; Ledger. You're thrown immediately into your financial data file in edit mode.
;; Ok, but might be frightening for someone learning how to use Ledger. The
;; added hydra is intended to show what actions are available, as well as their
;; key bindings.


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

;; === Ledger Hydra ===
;; menu available while in ledger-mode for common ledger commands

(with-eval-after-load 'ledger-mode
  (defhydra hydra-ledger (:color teal :timeout 10 :hint nil)
    "Ledger common commands menu"
    ("r" ledger-report "report" :column "report")
    ("a" ledger-add-transaction "add transaction" :column "edit")
    ("R" ledger-sort-region  "sort region" :column "edit")
    ("A" ledger-sort-buffer  "sort all" :column "edit"))

  (defun ledger-mode-hook-hydra-setup ()
    "Create ledger hydra/menu keybinding when entering ledger mode."
    (local-set-key (kbd "C-c ?") 'hydra-ledger/body))
  (add-hook 'ledger-mode-hook 'ledger-mode-hook-hydra-setup))

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
