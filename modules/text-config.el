;;; text-config --- Text Settings and Functionality -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ------------------------------- Text Settings -------------------------------

(defun cj/text-mode-settings ()
  "Personal settings for \='text-mode\='."
  (turn-on-visual-line-mode)            ;; wrap text in text modes (additional modes set elsewhere)
  (setq-default indent-tabs-mode nil)   ;; indentation should not insert tabs
  (setq sentence-end-double-space nil)) ;; in the 21st century, sentences may end w/ a single space
(add-hook 'text-mode-hook 'cj/text-mode-settings)

;; don't require newlines at EOF
(setq require-final-newline nil)                        ;; don't require newlines at the end of files
(custom-set-variables '(require-final-newline nil))     ;; some major modes read newline setting from custom

;; --------------------------------- Move Text ---------------------------------
;; move the current line or selected region up or down in the buffer

(use-package move-text
  :defer .5
  :bind
  ("C-<up>" . move-text-up)
  ("C-<down>" . move-text-down)
  :config
  (move-text-default-bindings))

;; ------------------------------- Expand Region -------------------------------
;; increase the selected region by semantic units

(use-package expand-region
  :defer .5
  :bind
  ("M-=" . er/expand-region)
  ("C->" . er/expand-region)
  ("M--" . er/contract-region)
  ("C-<" . er/contract-region))

;; ---------------------------- Change Inner / Outer ---------------------------
;; change inner and outer, just like in vim.

(use-package change-inner
  :defer .5
  :bind (("C-c i" . change-inner)
		 ("C-c o" . change-outer)))

;; ------------------------------ Delete Selection -----------------------------
;; delete the region on character insertion

(use-package delsel
  :ensure nil ;; built-in
  :defer .5
  :custom (delete-selection-mode t))

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
								 ("#+begin_quote" . " ")
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

;; --------------------------- Acccent (Diacriticals) --------------------------
;; an easy way to enter diacritical marks

(use-package accent
  :defer 1
  :bind ("C-c C-a" . accent-company))

;; ----------------------------- Visual Fill Column ----------------------------
;; text wrapping

(use-package visual-fill-column
  :defer .5
  :demand t)

(provide 'text-config)
;;; text-config.el ends heref
