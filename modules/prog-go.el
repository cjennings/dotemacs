;;; prog-go --- Golang Specific Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Configuration for Go programming using go-ts-mode (tree-sitter based).
;; Requires go-mode package for gofmt and related commands.
;;
;; Tools installation:
;;   go install golang.org/x/tools/cmd/goimports@latest
;;   go install honnef.co/go/tools/cmd/staticcheck@latest

;;; Code:

(defvar go-bin-path (expand-file-name "~/go/bin")
  "Path to Go binaries directory.
This is where tools like goimports and staticcheck are installed.")

(defvar go-ts-mode-map)
(defvar go-mod-ts-mode-map)

;; ---------------------------------- Go Setup ---------------------------------
;; golang preferences following Go community standards

(defun cj/go-setup ()
  "Default code preferences for Golang following Go conventions."
  (company-mode)
  (setq-local tab-width 4)              ;; Go standard tab width
  (setq-local standard-indent 4)        ;; indent 4 spaces per level
  (setq-local indent-tabs-mode t)       ;; use real tabs (Go convention)
  (electric-pair-mode t))               ;; match delimiters automatically

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

;; ---------------------------------- Go Mode ----------------------------------
;; go-ts-mode configuration (treesit-based Go editing)

(use-package go-mode
  :hook (go-ts-mode . cj/go-setup)
  :bind (:map go-ts-mode-map
			  ("<f6>"   . gofmt)
			  ("C-c 6"  . gofmt)
			  ("<f4>"   . cj/go-staticcheck)
			  ("C-c 4"  . cj/go-staticcheck))
  :mode (("\\.go\\'" . go-ts-mode)      ;; .go files use go-ts-mode
         ("go\\.mod\\'" . go-mod-ts-mode)) ;; go.mod uses go-mod-ts-mode
  :config
  (add-to-list 'exec-path go-bin-path)
  ;; Use goimports for formatting (adds/removes imports automatically)
  (setq gofmt-command "goimports"))

(provide 'prog-go)
;;; prog-go.el ends here
