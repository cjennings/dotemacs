;;; prog-webdev.el --- Web Development Packages and Settings -*- lexical-binding: t; coding: utf-8; -*-
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; TypeScript, JavaScript, and HTML development with tree-sitter, LSP, and
;; prettier formatting.
;;
;; Installation:
;;   sudo pacman -S typescript-language-server typescript prettier
;;
;; Features:
;;   - Tree-sitter: Syntax highlighting for TS, TSX, JS (via treesit-auto)
;;   - LSP: Completion, go-to-definition, hover docs, rename, references
;;   - Formatting: prettier via C-; f
;;   - Web-mode: Mixed HTML/CSS/JS templates with context-aware completion
;;
;; Workflow:
;;   1. Open .ts/.tsx/.js file → tree-sitter mode + LSP auto-starts
;;   2. C-; f → Format with prettier
;;   3. Open .html file → web-mode with LSP support

;;; Code:

(defvar typescript-ts-mode-map)
(defvar tsx-ts-mode-map)
(defvar js-ts-mode-map)

;; Forward declarations for LSP
(declare-function lsp-deferred "lsp-mode")

;; Forward declarations for external packages
(declare-function company-mode "company")

(defvar ts-language-server-path "typescript-language-server"
  "Path to typescript-language-server executable.
Install with: sudo pacman -S typescript-language-server")

(defvar prettier-path "prettier"
  "Path to prettier executable.
Install with: sudo pacman -S prettier")

;; ------------------------------ Web Dev Setup --------------------------------
;; shared setup for TypeScript, JavaScript, and TSX modes

(defun cj/webdev-setup ()
  "Set up common preferences for web development buffers."
  (company-mode)
  (flyspell-prog-mode)
  (superword-mode)
  (setq-local fill-column 100)
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq-local indent-tabs-mode nil)
  (electric-pair-mode t)

  ;; Enable LSP if available
  (when (and (fboundp 'lsp-deferred)
             (executable-find ts-language-server-path))
    (lsp-deferred)))

(defun cj/webdev-format-buffer ()
  "Format the current buffer with prettier.
Detects the file type automatically from the filename."
  (interactive)
  (if (executable-find prettier-path)
      (let ((point (point)))
        (shell-command-on-region (point-min) (point-max)
                                (format "prettier --stdin-filepath %s"
                                        (shell-quote-argument
                                         (or buffer-file-name "file.ts")))
                                nil t)
        (goto-char (min point (point-max))))
    (user-error "prettier not found; install with: sudo pacman -S prettier")))

(defun cj/webdev-keybindings ()
  "Set up keybindings for web development buffers."
  (local-set-key (kbd "C-; f") #'cj/webdev-format-buffer))

;; ----------------------------- TypeScript / JS -------------------------------
;; tree-sitter modes (built-in, Emacs 29+)
;; NOTE: No :mode directives — treesit-auto (in prog-general.el) handles
;; the auto-mode-alist mappings and auto-installs grammars on first use.

(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :hook
  ((typescript-ts-mode . cj/webdev-setup)
   (typescript-ts-mode . cj/webdev-keybindings)))

(use-package tsx-ts-mode
  :ensure nil
  :defer t
  :hook
  ((tsx-ts-mode . cj/webdev-setup)
   (tsx-ts-mode . cj/webdev-keybindings)))

(use-package js-ts-mode
  :ensure nil
  :defer t
  :hook
  ((js-ts-mode . cj/webdev-setup)
   (js-ts-mode . cj/webdev-keybindings)))

;; ----------------------------------- LSP -------------------------------------
;; TypeScript/JavaScript LSP configuration
;; Core LSP setup is in prog-general.el

(use-package lsp-mode
  :hook ((typescript-ts-mode tsx-ts-mode js-ts-mode) . lsp-deferred))

;; --------------------------------- CSS Eldoc ---------------------------------
;; CSS info in the echo area

(use-package css-eldoc
  :defer t)

;; ---------------------------------- Web Mode ---------------------------------
;; major mode for editing web templates (HTML with embedded JS/CSS)

(use-package web-mode
  :defer t
  :custom
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-engines-alist '(("django" . "\\.html\\'")))
  :mode ("\\.html?$" . web-mode)
  :hook (web-mode . cj/webdev-keybindings))

(provide 'prog-webdev)
;;; prog-webdev.el ends here.
