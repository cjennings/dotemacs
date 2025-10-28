;;; selection-framework.el --- Completion and Selection Framework -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; This module configures the completion and selection framework using:
;; - Vertico: Vertical completion UI
;; - Marginalia: Rich annotations in minibuffer
;; - Consult: Practical commands based on completing-read
;; - Orderless: Advanced completion style
;; - Embark: Contextual actions
;; - Company: In-buffer completion
;;
;; The configuration provides a modern, fast, and feature-rich completion
;; experience that enhances Emacs' built-in completing-read functionality.
;;
;;; Code:

;; ---------------------------------- Vertico ----------------------------------
;; Vertical completion UI

(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)                ; Cycle through candidates
  (vertico-count 10)               ; Number of candidates to display
  (vertico-resize nil)             ; Don't resize the minibuffer
  (vertico-sort-function #'vertico-sort-history-alpha) ; History first, then alphabetical
  :bind (:map vertico-map
			  ("C-j"   . vertico-next)
			  ("C-k"   . vertico-previous)
			  ("C-l"   . vertico-insert)  ; Insert current candidate
			  ("RET"   . vertico-exit)
			  ("C-RET" . vertico-exit-input)
			  ("M-RET" . minibuffer-force-complete-and-exit)
			  ("TAB"   . minibuffer-complete))
  :init
  (vertico-mode))

(use-package marginalia
  :demand t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :demand t
  :hook (marginalia-mode nerd-icons-completion-marginalia-setup)
  :after marginalia
  :init
  (nerd-icons-completion-mode))

;; ---------------------------------- Consult ----------------------------------
;; Practical commands based on completing-read

(use-package consult
  :demand t
  :bind (;; C-c bindings (mode-specific-map)
		 ("C-c h" . consult-history)
		 ;; C-x bindings (ctl-x-map)
		 ("C-x M-:" . consult-complex-command)
		 ("C-x b" . consult-buffer)
		 ("C-x 4 b" . consult-buffer-other-window)
		 ("C-x 5 b" . consult-buffer-other-frame)
		 ("C-x r b" . consult-bookmark)
		 ("C-x p b" . consult-project-buffer)
		 ;; M-g bindings (goto-map)
		 ("M-g e" . consult-compile-error)
		 ("M-g f" . consult-flymake)
		 ("M-g g" . consult-goto-line)
		 ("M-g M-g" . consult-goto-line)
		 ("M-g o" . consult-outline)
		 ("M-g m" . consult-mark)
		 ("M-g k" . consult-global-mark)
		 ("M-g i" . consult-imenu)
		 ("M-g I" . consult-imenu-multi)
		 ;; M-s bindings (search-map)
		 ("M-s d" . consult-find)
		 ("M-s D" . consult-locate)
		 ("M-s g" . consult-grep)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 ("M-s k" . consult-keep-lines)
		 ("M-s u" . consult-focus-lines)
		 ;; Isearch integration
		 ("M-s e" . consult-isearch-history)
		 :map isearch-mode-map
		 ("M-e" . consult-isearch-history)
		 ("M-s e" . consult-isearch-history)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 ;; Minibuffer history
		 :map minibuffer-local-map
		 ("M-s" . consult-history)
		 ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for =consult-register', =consult-register-load',
  ;; =consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
		register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref)

  :config
  ;; Configure preview. Default is 'any.
  (setq consult-preview-key 'any)

  ;; Configure narrowing key
  (setq consult-narrow-key "<")

  ;; Reset to defaults to avoid issues
  (setq consult-point-placement 'match-beginning)

  ;; Use Consult for completion-at-point
  (setq completion-in-region-function #'consult-completion-in-region))

;; Override default search with consult-line
(keymap-global-set "C-s" #'consult-line)

;; Consult integration with Embark
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
		 :map vertico-map
		 ("C-x C-d" . consult-dir)
		 ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs))

;; --------------------------------- Orderless ---------------------------------
;; Advanced completion style - provides space-separated, out-of-order matching

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion orderless basic))
								   (multi-category (styles orderless basic))))
  (orderless-matching-styles '(orderless-literal
							   orderless-regexp
							   orderless-initialism
							   orderless-prefixes)))

;; ---------------------------------- Embark -----------------------------------
;; Contextual actions - provides right-click like functionality

(use-package embark
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick an action to run
   ("C->" . embark-act-all)     ;; pick an action to run on all candidates
   ("C-," . embark-dwim)        ;; do what I mean
   ("C-h B" . embark-bindings)) ;; alternative for =describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

;; --------------------------- Consult Integration ----------------------------
;; Additional integrations for specific features

;; Yasnippet integration
(use-package consult-yasnippet
  :after yasnippet
  :bind ("C-c s i" . consult-yasnippet))

;; Flycheck integration
(use-package consult-flycheck
  :after flycheck
  :bind (:map flycheck-mode-map
			  ("C-c ! c" . consult-flycheck)))

;; ---------------------------------- Company ----------------------------------
;; In-buffer completion for text and code

(use-package company
  :demand t
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
		("<tab>" . company-complete-selection)
		("C-n" . company-select-next)
		("C-p" . company-select-previous))
  :custom
  (company-backends '(company-capf company-files company-keywords))
  (company-idle-delay 2)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 10)
  (company-selection-wrap-around t)
  (company-require-match nil)
  :config
  ;; Disable company in mail-related modes
  (setq company-global-modes
		'(not message-mode
			  mu4e-compose-mode
			  org-msg-edit-mode)))


(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode))

;; Icons for company
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; --------------------------------- Prescient ---------------------------------

(use-package prescient
  :demand t
  :config
  (prescient-persist-mode))

(use-package vertico-prescient
  :demand t
  :config
  (vertico-prescient-mode))

(use-package company-prescient
  :demand t
  :config
  (company-prescient-mode))

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c h" "consult history"
    "C-c s i" "insert snippet"
    "M-g" "goto menu"
    "M-s" "search menu"))

(provide 'selection-framework)
;;; selection-framework.el ends here
