;;; prog-python --- Python Specific Setup and Functionality -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ------------------------- General Settings ------------------------

(add-hook 'python-mode-hook (lambda () (setq indent-tabs-mode nil))) ;; use spaces, not tabs

;; ----------------------------------- Python ----------------------------------
;; remove the guess indent python message

(use-package python
  :config
  (setq python-indent-guess-indent-offset-verbose nil))

;; --------------------------- Python Mode ---------------------------

(use-package python-mode
  :ensure nil ;; built-in
  :hook
  ((python-mode . flyspell-prog-mode)
   (python-mode . superword-mode)
   (python-mode . company-mode)
   (python-mode . electric-pair-mode))   ;; auto-complete braces and pairs
  :custom
  (python-shell-interpreter "python3")
  (setq python-indent-offset 4)) ;; 4 spaces default indent

;; ----------------------------------- Poetry ----------------------------------
;; virtual environments and dependencies

;; (use-package poetry
;;   :defer t
;;   :config
;;   ;; Checks for the correct virtualenv. Better strategy IMO because the default
;;   ;; one is quite slow.
;;   (setq poetry-tracking-strategy 'switch-buffer)
;;   :hook (python-mode . poetry-tracking-mode))

;; ---------------------------------- Blacken ----------------------------------
;; formatting on save

(use-package blacken
  :defer t
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook (python-mode . blacken-mode))

;; ---------------------------------- Numpydoc ---------------------------------
;; automatically insert NumPy style docstrings in Python function definitions

(use-package numpydoc
  :defer t
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
			  ("C-c C-n" . numpydoc-generate)))

;; ------------------------------------ Toml -----------------------------------

(use-package toml-mode
  :defer .5)

(use-package eldoc-toml
  :defer .5)


(provide 'prog-python)
;;; prog-python.el ends here
