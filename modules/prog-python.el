;;; prog-python --- Python Specific Setup and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Python programming environment with LSP, tree-sitter, and formatting.
;;
;; Installation:
;;   pip install python-lsp-server[all] flake8
;;
;; LSP will provide:
;;   - Intelligent code completion
;;   - Jump to definition (M-.)
;;   - Find references
;;   - On-the-fly error checking (flake8)
;;   - Documentation on hover

;;; Code:

(defvar python-ts-mode-map)

;; Forward declarations for LSP
(declare-function lsp-deferred "lsp-mode")
(defvar lsp-pylsp-server-command)
(defvar lsp-pylsp-plugins-flake8-enabled)
(defvar lsp-pylsp-plugins-pylint-enabled)
(defvar lsp-pylsp-plugins-pycodestyle-enabled)
(defvar lsp-pylsp-plugins-autopep8-enabled)
(defvar lsp-pylsp-plugins-yapf-enabled)
(defvar lsp-pylsp-plugins-pydocstyle-enabled)
(defvar lsp-pylsp-plugins-rope-completion-enabled)

;; Forward declarations for external packages
(declare-function company-mode "company")
(defvar poetry-tracking-strategy)

(defvar pylsp-path "pylsp"
  "Path to Python LSP server (pylsp or pyright).
Install with: pip install python-lsp-server[all]
Or for pyright: pip install pyright")

;; -------------------------------- Python Setup -------------------------------
;; preferences for Python programming

(defun cj/python-setup ()
  "My default code preferences for Python coding."
  (company-mode)                      ;; completion framework
  (flyspell-prog-mode)                ;; spell check comments
  (superword-mode)                    ;; see-this-as-one-word
  (setq-local fill-column 80)         ;; wrap at 80 columns
  (setq-local tab-width 4)            ;; set the tab width to 4 spaces
  (setq-local standard-indent 4)      ;; indent 4 spaces
  (setq-local indent-tabs-mode nil)   ;; disable tab characters
  (electric-pair-mode t)              ;; match delimiters automatically

  ;; Enable LSP if available
  (when (and (fboundp 'lsp-deferred)
             (executable-find pylsp-path))
    (lsp-deferred)))

;; ----------------------------------- Python ----------------------------------
;; configuration for python-ts-mode (treesit-based Python editing)

(use-package python
  :ensure nil ;; built-in
  :hook
  (python-ts-mode . cj/python-setup)
  :custom
  (python-shell-interpreter "python3")
  :config
  ;; remove the "guess indent" python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Remove python-mode from auto-mode-alist to prefer python-ts-mode
  (setq auto-mode-alist
        (rassq-delete-all 'python-mode auto-mode-alist)))

;; ------------------------------- LSP for Python ------------------------------
;; Python-specific LSP configuration
;; Core LSP setup is in prog-general.el

(use-package lsp-mode
  :hook (python-ts-mode . lsp-deferred)
  :config
  ;; Use pylsp (python-lsp-server) - more lightweight than pyright
  (setq lsp-pylsp-server-command pylsp-path)

  ;; Configure pylsp plugins
  (setq lsp-pylsp-plugins-flake8-enabled t)
  (setq lsp-pylsp-plugins-pylint-enabled nil)  ;; too slow
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil)  ;; use flake8 instead
  (setq lsp-pylsp-plugins-autopep8-enabled nil)  ;; use blacken instead
  (setq lsp-pylsp-plugins-yapf-enabled nil)
  (setq lsp-pylsp-plugins-pydocstyle-enabled t)
  (setq lsp-pylsp-plugins-rope-completion-enabled t))

;; ----------------------------------- Poetry ----------------------------------
;; virtual environments and dependencies

(use-package poetry
  :defer t
  :after (python)
  :hook (python-ts-mode . poetry-tracking-mode)
  :config
  ;; Checks for the correct virtualenv. Better strategy IMO because the default
  ;; one is quite slow.
  (setq poetry-tracking-strategy 'switch-buffer))

;; ---------------------------------- Blacken ----------------------------------
;; formatting on save

(use-package blacken
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook (python-ts-mode . blacken-mode))

;; ---------------------------------- Numpydoc ---------------------------------
;; automatically insert NumPy style docstrings in Python function definitions

(use-package numpydoc
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-ts-mode-map
			  ("C-c C-n" . numpydoc-generate)))

;; ------------------------------------ TOML -----------------------------------
;; editing support and documentation for TOML files

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package eldoc-toml
  :hook (toml-mode . eldoc-toml-mode))


(provide 'prog-python)
;;; prog-python.el ends here
