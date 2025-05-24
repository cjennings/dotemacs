;;; selection-framework --- Search/Narrowing and Related Functionality -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:


;; ---------------------------------- Company ----------------------------------
;; Company is a modular in-buffer tool-tip-style completion front-end framework.

(use-package company
  :defer .5
  :hook
  (
   ;; (text-mode . company-mode) ;; also disables in org mode
   (prog-mode . company-mode)
   (lisp-interaction-mode . company-mode))
  :custom
  ;; search other buffers =with the same mode= for completion
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; don't downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; provide proper casing even if I don't.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  ( company-idle-delay 2)
  ;; use vscode icons in the margin
  (company-format-margin-function #'company-vscode-light-icons-margin)
  ;; no company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode)))

(with-eval-after-load 'company
  (define-key company-active-map
			  (kbd "TAB")
			  #'company-complete-common-or-cycle)
  (define-key company-active-map
			  (kbd "<backtab>")
			  (lambda ()
				(interactive)
				(company-complete-common-or-cycle -1))))

;; ---------------------------------- Counsel ----------------------------------
;; part of the counsel/ivy/swiper trio. ivy-enhanced versions of Emacs commands.

(use-package counsel
  :defer .5
  :bind
  ("C-c U" . counsel-unicode-char))

;; ------------------------------------ Ivy ------------------------------------
;; A generic completion mechanism for Emacs. https://github.com/abo-abo/swiper#ivy

(use-package ivy
  :defer .5
  :bind (("C-c u" . ivy-resume))
  (:map ivy-occur-grep-mode-map
		("n" . ivy-occur-next-line)
		("p" . ivy-occur-previous-line)
		("b" . backward-char)
		("f" . forward-char)
		("v" . ivy-occur-press)
		("RET" . ivy-occur-press))
  :config
  (setq ivy-action-wrap t)              ;; wrap next and previous actions
  (setq ivy-count-format "%d/%d ")      ;; show index as well as count
  (setq ivy-extra-directories nil)      ;; don't show ./ and ../ in lists
  (setq ivy-height 13)                  ;; 13 lines high
  (setq ivy--regex-ignore-order t)      ;; ignore word order
  (setq ivy-use-selectable-prompt t)    ;; prompt becomes selectable
  (setq ivy-use-virtual-buffers t)      ;; ivy-switch-buffer shows recently killed buffers
  (setq ivy-virtual-abbreviate 'full)   ;; show the full virtual file path
  (setq ivy-wrap t)                     ;; wrap list when finished to start and vice-versa
  (ivy-mode)

  ;; modify default search behaviour of ivy
  (setq ivy-re-builders-alist
		'((t . ivy--regex-plus))))

;; ALL THE ICONS IVY
;; Shows icons while using Ivy and Counsel
;; https://github.com/asok/all-the-icons-ivy
(use-package all-the-icons-ivy
  :defer .5
  :after ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; IVY-RICH
;; comes with rich transformers for commands from ivy and counsel.
;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :after ivy
  :defer .5
  :hook (counsel-mode . ivy-rich-mode)
  :config
  ;; For better performance
  ;; Better experience with icons
  (setq ivy-rich-parse-remote-buffer nil))


;; ALL THE ICONS IVY RICH
;; extracted from Centaur Emacs and leverages ivy-rich and all-the-icons.
;; https://github.com/seagle0128/all-the-icons-ivy-rich
(use-package all-the-icons-ivy-rich
  :defer .5
  :after (all-the-icons ivy-rich)
  :config
  (all-the-icons-ivy-rich-mode 1)
  (setq all-the-icons-ivy-rich-icon-size 0.8))

;; ----------------------------------- Swiper ----------------------------------
;; Swiper displays an overview of all matches, leveraging Ivy.

(use-package swiper
  :defer .5
  :bind
  (("C-s" . swiper)
   ("M-s" . swiper-isearch-thing-at-point))
  :config
  (setq swiper-action-recenter t)       ;; recenter after selection
  (setq swiper-goto-start-of-match t))  ;; jump to the beginning of match after selection

;; --------------------------------- Marginalia --------------------------------
;; Enables richer annotations in the selection framework

(use-package marginalia
  :defer .5
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; --------------------------------- Prescient ---------------------------------
;; Sorts and filters candidates that appear when you use a package like Ivy or Company.

(use-package prescient
  :defer .5
  :config
  (setq prescient-sort-full-matches-first t))

;; IVY PRESCIENT: Prescient integration with Ivy
(use-package ivy-prescient
  :defer .5
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1))

(provide 'selection-framework)
;;; selection-framework.el ends here
