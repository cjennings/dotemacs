;;; prog-lsp --- Setup for LSP Mode -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; good reference as to what to enable/disable in lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

;;; Code:


;;;;; ---------------------------- LSP Mode ---------------------------

(use-package lsp-mode
  :hook
  ((c-mode c++-mode go-mode js-mode js-jsx-mode typescript-mode python-mode web-mode) . lsp-deferred)
  :commands (lsp)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action))
  :bind-keymap ("C-c L" . lsp-command-map)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5))

;;;;; ----------------------------- LSP UI ----------------------------

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions nil) ;; turn off code actions in sidebar
  (setq lsp-ui-sideline-delay 0.05))

(provide 'prog-lsp)
;;; prog-lsp.el ends here
