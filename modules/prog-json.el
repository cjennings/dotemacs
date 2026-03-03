;;; prog-json.el --- JSON Editing, Formatting, and jq Integration -*- lexical-binding: t; coding: utf-8; -*-
;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; JSON editing with tree-sitter highlighting, one-key formatting, and
;; interactive jq queries against the current buffer.
;;
;; Features:
;;   - Tree-sitter: Better syntax highlighting and structural navigation
;;   - Formatting: Pretty-print with sorted keys via C-; f
;;   - jq: Interactive jq REPL against current JSON buffer
;;
;; Workflow:
;;   1. Open .json file → json-ts-mode with tree-sitter highlighting
;;   2. C-; f → Format/pretty-print the buffer
;;   3. C-c C-q → Open jq interactive buffer to query/transform JSON

;;; Code:

(defvar json-ts-mode-map)

;; -------------------------------- JSON Mode ----------------------------------
;; tree-sitter mode for JSON files (built-in, Emacs 29+)
;; NOTE: No :mode directive here — treesit-auto (in prog-general.el) handles
;; the auto-mode-alist mapping and auto-installs the grammar on first use.

(use-package json-ts-mode
  :ensure nil
  :defer t)

;; -------------------------------- Formatting ---------------------------------
;; pretty-print with sorted keys, bound to standard format key

(defun cj/json-format-buffer ()
  "Format the current JSON buffer with sorted keys.
Uses jq if available for reliable formatting, otherwise falls
back to the built-in `json-pretty-print-buffer-ordered'."
  (interactive)
  (if (executable-find "jq")
      (let ((point (point)))
        (shell-command-on-region (point-min) (point-max) "jq --sort-keys ." nil t)
        (goto-char (min point (point-max))))
    (json-pretty-print-buffer-ordered)))

(defun cj/json-setup ()
  "Set up JSON buffer keybindings."
  (local-set-key (kbd "C-; f") #'cj/json-format-buffer))

(add-hook 'json-ts-mode-hook #'cj/json-setup)

;; --------------------------------- jq Mode -----------------------------------
;; interactive jq queries against JSON buffers

(use-package jq-mode
  :defer t
  :bind (:map json-ts-mode-map
              ("C-c C-q" . jq-interactively)))

(provide 'prog-json)
;;; prog-json.el ends here.
