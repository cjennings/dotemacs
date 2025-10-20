;;; prog-python --- Python Specific Setup and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

(defvar python-ts-mode-map)

;; -------------------------------- Python Setup -------------------------------
;; preferences for Python programming

(defun cj/python-setup ()
  "My default code preferences for Python coding."
  (hs-minor-mode)                     ;; folding
  (company-mode)                      ;; completion framework
  (flyspell-prog-mode)                ;; spell check comments
  (superword-mode)                    ;; see-this-as-one-word
  (setq-default fill-column 80)       ;; wrap at 80 columns
  (setq-default tab-width 4)          ;; set the tab width to 4 spaces
  (setq-default standard-indent 4)    ;; indent 4 spaces
  (setq-default indent-tabs-mode nil) ;; disable tab characters
  (electric-pair-mode t))             ;; match delimiters automatically

;; ----------------------------------- Python ----------------------------------
;; configuration for python-ts-mode (treesit-based Python editing)

(use-package python
  :ensure nil ;; built-in
  :hook
  (python-ts-mode . cj/python-setup)
  :custom
  (python-shell-interpreter "python3")
  :config
  ;; remove the "guess indent" python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Remove python-mode from auto-mode-alist to prefer python-ts-mode
  (setq auto-mode-alist
        (rassq-delete-all 'python-mode auto-mode-alist)))

;; ----------------------------------- Poetry ----------------------------------
;; virtual environments and dependencies

(use-package poetry
  :defer t
  :after (python)
  :hook (python-ts-mode . poetry-tracking-mode)
  :config
  ;; Checks for the correct virtualenv. Better strategy IMO because the default
  ;; one is quite slow.
  (setq poetry-tracking-strategy 'switch-buffer))

;; ---------------------------------- Blacken ----------------------------------
;; formatting on save

(use-package blacken
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook (python-ts-mode . blacken-mode))

;; ---------------------------------- Numpydoc ---------------------------------
;; automatically insert NumPy style docstrings in Python function definitions

(use-package numpydoc
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-ts-mode-map
			  ("C-c C-n" . numpydoc-generate)))

;; ------------------------------------ TOML -----------------------------------
;; editing support and documentation for TOML files

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package eldoc-toml
  :hook (toml-mode . eldoc-toml-mode))


(provide 'prog-python)
;;; prog-python.el ends here
