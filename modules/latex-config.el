;;; latex-config --- Setup for LaTeX and Related Software -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;;;; ----------------------------- Auctex ----------------------------

;; (use-package tex
;;   :ensure auctex
;;   :hook
;;   (LaTeX-mode    . (lambda () (TeX-fold-mode 1)))                    ; automatically activate TeX-fold-mode.
;;   (TeX-mode-hook . (lambda () (setq TeX-command-default "latexmk"))) ; use latexmk by default
;;   (LaTeX-mode    . flyspell-mode)                               ; turn on flyspell-mode by default
;;   ;; (LaTeX-mode    . TeX-PDF-mode)
;;   ;; (LaTeX-mode . (lambda () (push (list 'output-pdf "Zathura") TeX-view-program-selection)))
;;   :mode
;;   ("\\.tex\\'" . latex-mode)
;;   :config
;;   (setq TeX-auto-save t)       ; auto save style info when saving buffer
;;   (setq TeX-parse-self t)      ; parse file after loading if it has no style hook
;;   (setq TeX-save-query nil)    ; don't ask to save files before starting TeX
;;   (setq TeX-PDF-mode t)        ; compile to PDF mode, rather than DVI
;;   (setq-default TeX-master t)) ; Assume the file is the master file itself

;; ;; use pdftools as viewer
;; ;; https://emacs.stackexchange.com/questions/21755/use-pdfview-as-default-auctex-pdf-viewer#21764
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;; ;; to have the buffer refresh after compilation,
;; ;; very important so that PDFView refreshes itself after compilation
;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)


;; https://github.com/tom-tan/auctex-latexmk
;; You should also add the following line to your .latexmkrc file:
;; # .latexmkrc starts
;; $pdf_mode = 1;
;; # .latexmkrc ends

;; AUCTEX-LATEXMK
;;
;; (use-package auctex-latexmk
;;   :config
;;   (auctex-latexmk-setup)
;;   (setq auctex-latexmk-inherit-TeX-PDF-mode t))


(provide 'latex-config)
;;; latex-config.el ends here
