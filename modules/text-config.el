;;; text-config --- Text Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
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
(setopt indent-tabs-mode nil)         ;; indentation should not insert tabs
(setopt require-final-newline nil)    ;; don't require newlines at the end of files
(setopt sentence-end-double-space nil) ;; in the 21st century, sentences end w/ a single space

(defun cj/text-mode-settings ()
  "Personal settings for `text-mode'."
  (turn-on-visual-line-mode))        ;; wrap text in text modes
(add-hook 'text-mode-hook #'cj/text-mode-settings)

;; --------------------------------- Move Text ---------------------------------
;; move the current line or selected region up or down in the buffer

(use-package move-text
  :bind
  (("C-<up>" . move-text-up)
   ("C-<down>" . move-text-down)))

;; ------------------------------- Expand Region -------------------------------
;; increase the region by semantic units

(use-package expand-region
  :bind
  (("M-=" . er/expand-region)
   ("C->" . er/expand-region)
   ("M--" . er/contract-region)
   ("C-<" . er/contract-region)))

;; ---------------------------- Change Inner / Outer ---------------------------
;; change inner and outer, just like in vim.

(use-package change-inner
  :bind (("C-c i" . change-inner)
         ("C-c o" . change-outer)))

;; ------------------------------ Delete Selection -----------------------------
;; delete the region on character insertion

(use-package delsel
  :ensure nil ;; built-in
  :config
  (delete-selection-mode t))

;; ------------------------------- Edit Indirect -------------------------------
;; edit selection in new buffer, C-c to finish; replaces with modifications

(use-package edit-indirect
  :bind ("M-I" . edit-indirect-region))

;; ------------------------------ Prettify Symbols -----------------------------
;; replacing the word l-a-m-b-d-a with a symbol, just because

(defun cj/case-insensitive-symbol-pair (pair)
  "Convert a symbol PAIR to both lowercase and uppercase variants.
PAIR is a cons cell of (string . symbol)."
  (let ((k (car pair))
        (v (cdr pair)))
    (list (cons (downcase k) v)
          (cons (upcase k) v))))

(setopt prettify-symbols-alist
        (apply #'append
               (mapcar #'cj/case-insensitive-symbol-pair
                       '(("#+begin_src" . "λ")
                         ("#+end_src" . "λ")
                         ("lambda" . "λ")))))

(add-hook 'prog-mode-hook #'turn-on-prettify-symbols-mode)
(add-hook 'org-mode-hook #'turn-on-prettify-symbols-mode)

;; ---------------------------------- Olivetti ---------------------------------
;; center text in the middle of the screen.

(use-package olivetti
  :commands olivetti-mode
  :config
  (setopt olivetti-body-width 100))  ;; 100 characters wide (comfortable reading width)

;; --------------------------- Accent (Diacriticals) ---------------------------
;; an easy way to enter diacritical marks

(use-package accent
  :commands accent-company
  :bind ("C-`" . accent-company))

(provide 'text-config)
;;; text-config.el ends here
