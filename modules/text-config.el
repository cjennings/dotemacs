;;; text-config --- Text Settings and Functionality -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Configuration for text editing features including:
;; - Basic text mode settings (visual line mode, indentation, spacing)
;; - Text manipulation (move-text, expand-region, change-inner)
;; - Selection behavior (delete-selection-mode)
;; - Editing tools (edit-indirect, olivetti, accent)
;; - Visual enhancements (prettify-symbols, visual-fill-column)

;;; Code:

;; ------------------------------- Text Settings -------------------------------

;; Global text settings
(setq-default indent-tabs-mode nil)   ;; indentation should not insert tabs
(setq require-final-newline nil)      ;; don't require newlines at the end of files
(setq sentence-end-double-space nil)  ;; in the 21st century, sentences end w/ a single space

(defun cj/text-mode-settings ()
  "Personal settings for `text-mode'."
  (turn-on-visual-line-mode))        ;; wrap text in text modes
(add-hook 'text-mode-hook 'cj/text-mode-settings)

;; --------------------------------- Move Text ---------------------------------
;; move the current line or selected region up or down in the buffer

(use-package move-text
  :defer 0.5
  :bind
  (("C-<up>" . move-text-up)
   ("C-<down>" . move-text-down)))

;; ------------------------------- Expand Region -------------------------------
;; increase the region by semantic units

(use-package expand-region
  :defer 0.5
  :bind
  (("M-=" . er/expand-region)
   ("C->" . er/expand-region)
   ("M--" . er/contract-region)
   ("C-<" . er/contract-region)))

;; ---------------------------- Change Inner / Outer ---------------------------
;; change inner and outer, just like in vim.

(use-package change-inner
  :defer 0.5
  :bind (("C-c i" . change-inner)
         ("C-c o" . change-outer)))

;; ------------------------------ Delete Selection -----------------------------
;; delete the region on character insertion

(use-package delsel
  :ensure nil ;; built-in
  :defer 0.5
  :config
  (delete-selection-mode t))

;; ------------------------------- Edit Indirect -------------------------------
;; edit selection in new buffer, C-c to finish; replaces with modifications

(use-package edit-indirect
  :defer 1
  :bind ("M-I" . edit-indirect-region))

;; ------------------------------ Prettify Symbols -----------------------------
;; replacing the word l-a-m-b-d-a with a symbol, just because

(setq-default prettify-symbols-alist
			  (let ((mapping (lambda (pair)
							   (let ((k (car pair))
									 (v (cdr pair)))
								 (list (cons (downcase k) v)
									   (cons (upcase k) v))))))
				(apply #'append
					   (mapcar mapping
							   '(
								 ("#+begin_src" . "λ")
								 ("#+begin_src" . "λ")
								 ("#+end_src" . "λ")
                                 ("#+begin_quote" . "")
								 ("#+end_quote" . "")
								 ("lambda" . "λ"))))))


(add-hook 'prog-mode-hook 'turn-on-prettify-symbols-mode)
(add-hook 'org-mode-hook 'turn-on-prettify-symbols-mode)

;; ---------------------------------- Olivetti ---------------------------------
;; center text in the middle of the screen.

(use-package olivetti
  :defer 1
  :config
  (setq-default olivetti-body-width 100))

;; --------------------------- Accent (Diacriticals) ---------------------------
;; an easy way to enter diacritical marks

(use-package accent
  :defer 1
  :bind ("C-`" . accent-company))

;; ----------------------------- Visual Fill Column ----------------------------
;; text wrapping

(use-package visual-fill-column
  :defer 0.5
  :config
  (setq-default visual-fill-column-center-text nil)
  (setq-default visual-fill-column-width 100)
  :hook
  (visual-line-mode . visual-fill-column-mode))

(provide 'text-config)
;;; text-config.el ends here
