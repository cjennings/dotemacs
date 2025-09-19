;;; latex-config --- Setup for LaTeX and Related Software -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; WORKFLOW:
;;
;; Opening any tex file will put you into LaTeX mode.
;;
;; C-c C-m to enter macros
;; C-c C-e to enter environment
;;
;; C-c C-c to compile a tex document using latexmk
;; C-c C-v to view the resulting pdf
;;
;;; Code:

;; ----------------------------- Auctex And Related ----------------------------

(use-package tex
  :ensure auctex
  :defer t
  :hook
  (TeX-mode-hook . (lambda () (setq TeX-command-default "latexmk"))) ; use latexmk by default
  (LaTeX-mode    . (lambda () (TeX-fold-mode 1)))                    ; automatically activate TeX-fold-mode.
  (LaTeX-mode    . flyspell-mode)                               ; turn on flyspell-mode by default
  (LaTeX-mode    . TeX-PDF-mode)
  (LaTeX-mode . (lambda () (push (list 'output-pdf "Zathura") TeX-view-program-selection)))
  :mode
  ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)       ; auto save style info when saving buffer
  (setq TeX-parse-self t)      ; parse file after loading if it has no style hook
  (setq TeX-save-query nil)    ; don't ask to save files before starting TeX
  (setq TeX-PDF-mode t)        ; compile to PDF mode, rather than DVI
  (setq-default TeX-master t)) ; Assume the file is the master file itself

(use-package auctex-latexmk
  :defer t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :after tex
  :init (company-auctex-init))

;; ----------------------------- Graphviz Dot Mode -----------------------------

(use-package graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

(provide 'latex-config)
;;; latex-config.el ends here
