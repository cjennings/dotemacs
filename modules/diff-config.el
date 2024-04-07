;;; ediff-config.el --- diff Configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; highly useful setup for configuring ediff
;; https://oremacs.com/2015/01/17/setting-up-ediff/

;;; Code:

;; -------------------------------- Csetq Macro --------------------------------
;; a macro that allows a setq for custom variables. uses the variable's set
;; property without writing customizations to emacs init.

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
				'set-default)
            ',variable ,value))

;; ------------------------------- Ediff Settings ------------------------------
;; ediff configuration. note the dired-ediff-files function in dirvish.

;; lose the control panel
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)

;; only split horizontally
(csetq ediff-split-window-function 'split-window-horizontally)

;; ignore whitespace in diffs
(csetq ediff-diff-options "-w")

;; only highlight the current diff:
(setq-default ediff-highlight-all-diffs 'nil)

;; use j and k for next and previous diffs
(defun cj/ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))
(add-hook 'ediff-mode-hook 'cj/ediff-hook)

;; restore the window setup after quitting
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; ----------------------------------- Ztree -----------------------------------
;; diff two directories

(use-package ztree
  :defer .5
  :bind
  ("C-c D" . ztree-diff))

(provide 'diff-config)
;;; diff-config.el ends here.
