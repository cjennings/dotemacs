;;; prog-yaml --- YAML Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none necessary; currently eager but should load by YAML major
;;   mode (Phase 6 deferral candidate).
;; Top-level side effects: one add-hook, package configuration via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
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

(defun cj/--yaml-format-region (program &rest args)
  "Replace the buffer with PROGRAM ARGS run over its contents, via argv.
Runs PROGRAM (with ARGS) on the whole buffer through
`call-process-region' — no shell, so no quoting or word-splitting.
The buffer is replaced only when PROGRAM exits zero; on a non-zero
exit the buffer is left untouched and an error is signalled with
the program's stderr text.  Point is preserved as closely as the
reformatted size allows.  Returns t on success."
  (let* ((point (point))
         (src (current-buffer))
         (out (generate-new-buffer " *yaml-format-out*"))
         (status (apply #'call-process-region
                        (point-min) (point-max) program
                        nil out nil args)))
    (unwind-protect
        (if (and (integerp status) (zerop status))
            (progn
              (with-current-buffer src
                (replace-buffer-contents out)
                (goto-char (min point (point-max))))
              t)
          (user-error "%s failed: %s" program
                      (string-trim (with-current-buffer out (buffer-string)))))
      (kill-buffer out))))

(defun cj/yaml-format-buffer ()
  "Format the current YAML buffer with prettier.
Preserves point position as closely as possible."
  (interactive)
  (if (executable-find "prettier")
      (cj/--yaml-format-region "prettier" "--parser" "yaml")
    (user-error "prettier not found; install with: npm install -g prettier")))

(defun cj/yaml-setup ()
  "Set up YAML buffer keybindings and linting."
  (local-set-key (kbd "C-; f") #'cj/yaml-format-buffer)
  (flycheck-mode 1))

(add-hook 'yaml-ts-mode-hook #'cj/yaml-setup)

(provide 'prog-yaml)
;;; prog-yaml.el ends here
