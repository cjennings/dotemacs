;;; prog-go --- Golang Specific Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Configuration for Go programming using go-ts-mode (tree-sitter based).
;; Requires go-mode package for gofmt and related commands.
;;
;; Installation:
;;   go install golang.org/x/tools/cmd/goimports@latest
;;   go install golang.org/x/tools/gopls@latest
;;   go install honnef.co/go/tools/cmd/staticcheck@latest
;;
;; LSP will provide:
;;   - Intelligent code completion
;;   - Jump to definition (M-.)
;;   - Find references
;;   - On-the-fly error checking
;;   - Code lenses (run tests, show package stats)

;;; Code:

(defvar go-bin-path (expand-file-name "~/go/bin")
  "Path to Go binaries directory.
This is where tools like goimports and staticcheck are installed.")

(defvar gopls-path "gopls"
  "Path to gopls (Go language server).
Install with: go install golang.org/x/tools/gopls@latest")

(defvar dlv-path "dlv"
  "Path to Delve debugger.
Install with: go install github.com/go-delve/delve/cmd/dlv@latest")

(defvar go-ts-mode-map)
(defvar go-mod-ts-mode-map)

;; Forward declarations for LSP
(declare-function lsp-deferred "lsp-mode")
(defvar lsp-go-gopls-server-path)
(defvar lsp-go-analyses)
(defvar lsp-go-codelenses)

;; Forward declarations for external packages
(declare-function company-mode "company")
(declare-function cj/disabled "system-defaults")
(defvar gofmt-command)

;; ---------------------------------- Go Setup ---------------------------------
;; golang preferences following Go community standards

(defun cj/go-setup ()
  "Default code preferences for Golang following Go conventions."
  (company-mode)
  (setq-local tab-width 4)              ;; Go standard tab width
  (setq-local standard-indent 4)        ;; indent 4 spaces per level
  (setq-local indent-tabs-mode t)       ;; use real tabs (Go convention)
  (electric-pair-mode t)                ;; match delimiters automatically

  ;; Enable LSP if available
  (when (and (fboundp 'lsp-deferred)
             (executable-find gopls-path))
    (lsp-deferred)))

(defun cj/go-staticcheck ()
  "Run staticcheck on the current Go package."
  (interactive)
  (let* ((staticcheck-bin (expand-file-name "staticcheck" go-bin-path))
         (default-directory (if buffer-file-name
                               (file-name-directory buffer-file-name)
                             default-directory)))
    (if (file-executable-p staticcheck-bin)
        (compile (format "%s ./..." staticcheck-bin))
      (message "staticcheck not found at %s. Install with: go install honnef.co/go/tools/cmd/staticcheck@latest"
               staticcheck-bin))))

(defun cj/go-debug ()
  "Start Delve debugger for the current Go package."
  (interactive)
  (let* ((dlv-bin (expand-file-name dlv-path go-bin-path))
         (default-directory (if buffer-file-name
                               (file-name-directory buffer-file-name)
                             default-directory)))
    (if (or (executable-find dlv-path)
            (file-executable-p dlv-bin))
        (gud-gdb (format "%s debug" (or (executable-find dlv-path) dlv-bin)))
      (message "Delve not found. Install with: go install github.com/go-delve/delve/cmd/dlv@latest"))))

(defun cj/go-mode-keybindings ()
  "Set up keybindings for Go programming.
Overrides default prog-mode keybindings with Go-specific commands."
  ;; S-f5: Run staticcheck (static analysis)
  (local-set-key (kbd "S-<f5>") #'cj/go-staticcheck)

  ;; S-f6: Debug with Delve
  (local-set-key (kbd "S-<f6>") #'cj/go-debug))

;; ---------------------------------- Go Mode ----------------------------------
;; go-ts-mode configuration (treesit-based Go editing)

(use-package go-mode
  :hook ((go-ts-mode . cj/go-setup)
         (go-ts-mode . cj/go-mode-keybindings))
  :bind (:map go-ts-mode-map
			  ("C-; f"  . gofmt))  ;; Override global formatter with gofmt/goimports
  :mode (("\\.go\\'" . go-ts-mode)      ;; .go files use go-ts-mode
         ("go\\.mod\\'" . go-mod-ts-mode)) ;; go.mod uses go-mod-ts-mode
  :config
  (add-to-list 'exec-path go-bin-path)
  ;; Use goimports for formatting (adds/removes imports automatically)
  (setq gofmt-command "goimports"))

;; -------------------------------- LSP for Go ---------------------------------
;; Go-specific LSP configuration using gopls
;; Core LSP setup is in prog-general.el

(use-package lsp-mode
  :hook ((go-ts-mode go-mod-ts-mode) . lsp-deferred)
  :config
  (setq lsp-go-gopls-server-path gopls-path)

  ;; Configure gopls
  (setq lsp-go-analyses '((shadow . t)
                         (simplifycompositelit . :json-false)))
  (setq lsp-go-codelenses '((gc_details . t)
                           (generate . t)
                           (regenerate_cgo . t)
                           (test . t)
                           (tidy . t)
                           (upgrade_dependency . t)
                           (vendor . t))))

(provide 'prog-go)
;;; prog-go.el ends here
