;;; prog-go --- Golang Specific Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ---------------------------------- Go Setup ---------------------------------
;; golang preferences

(defun cj/go-setup ()
  "My default code preferences for Golang."
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-hl)
  (tree-sitter-hl-mode)
  (hs-minor-mode)
  (company-mode)
  (setq-default tab-width 4)            ;; set the tab width to 4 spaces
  (setq-default standard-indent 4)      ;; indent 4 spaces
  (setq-default indent-tabs-mode nil)   ;; disable tab characters
  (electric-pair-mode t))               ;; match delimiters automatically
(add-hook 'go-mode-hook 'cj/go-setup)

;; ---------------------------------- Go Mode ----------------------------------
;; go mode configuration

(use-package go-mode
  :bind (:map go-mode-map
			  ("<f6>"   . gofmt)
			  ("C-c 6"  . gofmt)
			  ("<f4>"   . golint)
			  ("C-c 4"  . golint))
  :config
  (add-to-list 'exec-path "~/go/bin")
  ;; allow adding/removing fmt lines; install with:
  ;; go install golang.org/x/tools/cmd/goimports@latest
  (setq gofmt-command "goimports"))

(provide 'prog-go)
;;; prog-go.el ends here
