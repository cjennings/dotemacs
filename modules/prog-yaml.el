;;; prog-yaml --- YAML Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; YAML editing with tree-sitter highlighting and one-key formatting.
;;
;; Features:
;;   - Tree-sitter: Syntax highlighting and structural navigation
;;   - Formatting: Normalize indentation and style via C-; f
;;
;; Workflow:
;;   1. Open .yml/.yaml file → yaml-ts-mode with tree-sitter highlighting
;;   2. C-; f → Format buffer with prettier

;;; Code:

;; -------------------------------- YAML Mode ----------------------------------
;; tree-sitter mode for YAML files (built-in, Emacs 29+)
;; NOTE: No :mode directive — treesit-auto (in prog-general.el) handles
;; the auto-mode-alist mapping and auto-installs the grammar on first use.

(use-package yaml-ts-mode
  :ensure nil
  :defer t)

;; -------------------------------- Formatting ---------------------------------
;; normalize indentation and style, bound to standard format key

(defun cj/yaml-format-buffer ()
  "Format the current YAML buffer with prettier.
Preserves point position as closely as possible."
  (interactive)
  (if (executable-find "prettier")
      (let ((point (point)))
        (shell-command-on-region (point-min) (point-max)
                                "prettier --parser yaml" nil t)
        (goto-char (min point (point-max))))
    (user-error "prettier not found; install with: npm install -g prettier")))

(defun cj/yaml-setup ()
  "Set up YAML buffer keybindings and linting."
  (local-set-key (kbd "C-; f") #'cj/yaml-format-buffer)
  (flycheck-mode 1))

(add-hook 'yaml-ts-mode-hook #'cj/yaml-setup)

(provide 'prog-yaml)
;;; prog-yaml.el ends here
