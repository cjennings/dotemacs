;;; latex-config.el --- Setup for LaTeX and Related Software -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none; LaTeX/AUCTeX editing, a hook/mode-loaded deferral candidate.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none.
;; Direct test load: yes.
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

;; ----------------------------- PDF Viewer Selection --------------------------
;;
;; Pick whichever PDF viewer is available rather than hard-coding Zathura.
;; The selected viewer is pushed onto `TeX-view-program-selection' for
;; output-pdf so `C-c C-v' opens the compiled PDF.

(defconst cj/--latex-pdf-viewer-candidates
  ;; (PROGRAM . TeX-VIEWER-NAME) in preference order.  TeX-view-program-list
  ;; has built-in entries for "Zathura", "Evince", "Okular", "Skim",
  ;; "PDF Tools", and platform openers; we match against those names.
  '(("zathura"     . "Zathura")
    ("evince"      . "Evince")
    ("okular"      . "Okular")
    ("SumatraPDF"  . "Sumatra PDF")
    ("xdg-open"    . "xdg-open"))
  "Ordered (EXECUTABLE . TEX-VIEWER-NAME) pairs for PDF preview selection.")

(defvar TeX-view-program-selection)
(declare-function pdf-view-mode "pdf-view")

(defun cj/--latex-select-pdf-viewer ()
  "Select the first available external PDF viewer for `output-pdf'.
Falls back to PDF Tools when no external viewer is on PATH.  Any existing
`output-pdf' entry is dropped first, then the chosen viewer is consed onto
the head so it wins over any default.  Idempotent: re-running leaves a
single entry."
  (let* ((found (cl-find-if (lambda (entry)
                              (executable-find (car entry)))
                            cj/--latex-pdf-viewer-candidates))
         (viewer-name (if found (cdr found) "PDF Tools")))
    (setq TeX-view-program-selection
          (cons (list 'output-pdf viewer-name)
                (assq-delete-all 'output-pdf TeX-view-program-selection)))))

;; ----------------------------- Auctex And Related ----------------------------

(use-package tex
  :ensure auctex
  :defer t
  :hook
  ;; Name the mode, not the hook: use-package appends "-hook" to any symbol not
  ;; ending in "-mode", so `TeX-mode' becomes `TeX-mode-hook' while the literal
  ;; `TeX-mode-hook' would expand to the unbound `TeX-mode-hook-hook'.
  (TeX-mode . (lambda () (setq TeX-command-default "latexmk"))) ; use latexmk by default
  (LaTeX-mode    . (lambda () (TeX-fold-mode 1)))                    ; automatically activate TeX-fold-mode.
  (LaTeX-mode    . flyspell-mode)                               ; turn on flyspell-mode by default
  (LaTeX-mode    . TeX-PDF-mode)
  (LaTeX-mode . cj/--latex-select-pdf-viewer)
  :mode
  ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)       ; auto save style info when saving buffer
  (setq TeX-parse-self t)      ; parse file after loading if it has no style hook
  (setq TeX-save-query nil)    ; don't ask to save files before starting TeX
  (setq TeX-PDF-mode t)        ; compile to PDF mode, rather than DVI
  (setq-default TeX-master t)) ; Assume the file is the master file itself

(use-package auctex-latexmk
  ;; Load with AUCTeX, not deferred: `:defer t' has no autoload trigger here, so
  ;; `auctex-latexmk-setup' never runs and "latexmk" never joins TeX-command-list.
  :after tex
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
