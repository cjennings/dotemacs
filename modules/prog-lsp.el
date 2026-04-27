;;; prog-lsp --- Setup for LSP Mode -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; good reference as to what to enable/disable in lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

;;; Code:

;; Forward declarations for byte-compile and let-binding under lexical scope.
;; Real definitions are lsp-mode's defcustoms.
(defvar lsp-file-watch-ignored-directories)
(defvar lsp-enable-remote)

;;;;; --------------------- File-Watch Ignore Patterns ---------------------
;; lsp-mode prompts when a workspace exceeds `lsp-file-watch-threshold' (1000)
;; directories.  Real source repos cross that line easily once node_modules,
;; build outputs, and language caches are counted.  These patterns extend the
;; lsp-mode defaults (.git, .svn, .idea, ...) instead of replacing them, so the
;; built-in VC/IDE excludes still apply.  Buffer-local overrides via
;; `.dir-locals.el' don't work — lsp-mode reads the global value at workspace
;; init, not the buffer-local one.  Hence: global defaults here.

(defvar cj/lsp-file-watch-ignored-extras
  '("[/\\\\]node_modules\\'"
    "[/\\\\]\\.ruff_cache\\'"
    "[/\\\\]dist\\'"
    "[/\\\\]coverage\\'"
    "[/\\\\]test-results\\'"
    "[/\\\\]playwright-report\\'"
    "[/\\\\]tf[/\\\\]\\.terraform\\'"
    "[/\\\\]__pycache__\\'"
    "[/\\\\]\\.venv\\'"
    "[/\\\\]venv\\'"
    "[/\\\\]\\.pytest_cache\\'"
    "[/\\\\]\\.mypy_cache\\'"
    "[/\\\\]target\\'")
  "Build/cache directory patterns to add to `lsp-file-watch-ignored-directories'.
Each entry is an Emacs regex matching a path ending in the named directory.")

(defun cj/lsp--add-file-watch-ignored-extras ()
  "Append `cj/lsp-file-watch-ignored-extras' to lsp-mode's ignore list.
Idempotent — `add-to-list' skips patterns already present."
  (dolist (pattern cj/lsp-file-watch-ignored-extras)
    (add-to-list 'lsp-file-watch-ignored-directories pattern)))

;;;;; ---------------------------- LSP Mode ---------------------------

(use-package lsp-mode
  :hook
  ((c-mode c++-mode go-mode js-mode js-jsx-mode typescript-mode python-mode web-mode) . lsp-deferred)
  :commands (lsp)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action))
  :bind-keymap ("C-c L" . lsp-command-map)
  :init
  (setq lsp-enable-remote nil) ;; Don't start LSP on TRAMP files (slow, prompts for project root)
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
  (setq lsp-idle-delay 0.5)
  (cj/lsp--add-file-watch-ignored-extras))

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
