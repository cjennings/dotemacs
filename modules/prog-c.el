;;; prog-c --- C Programming Settings and Functionality -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;;;; ------------------------------ C-Mode Settings ------------------------------

(defun cj/c-mode-settings ()
  "Settings for \\='c-mode\\='."
  (setq-default indent-tabs-mode nil)                 ;; spaces, not tabs
  (setq-default c-basic-offset 4)                     ;; 4 spaces offset
  (setq c-default-style "stroustrup")                 ;; k&r c, 2nd edition
  (setq c-basic-indent 4)                             ;; indent 4 spaces
  (setq compile-command "CFLAGS=\"-Wall -g \" make ") ;; default make command
  (setq display-line-numbers-type t)                  ;; disable relative line numbers in C
  (setq comment-auto-fill-only-comments t)            ;; only auto-fill inside comments
  (auto-fill-mode)                                   ;; auto-fill multiline comments
  (electric-pair-mode))                                ;; automatic parenthesis pairing
(add-hook 'c-mode-common-hook 'cj/c-mode-settings)

;;;; -------------------------- Keybindings --------------------------

(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key (kbd "S-<f2>") #'compile)
                                (local-set-key (kbd "S-<f3>") #'gdb)))



(provide 'prog-c)
;;; prog-c.el ends here
