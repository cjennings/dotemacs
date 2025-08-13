;;; help-config --- Help Functionality Configuration -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:


(setq help-window-select t) ;; Always select the help buffer in a separate window

(global-set-key (kbd "C-h P") 'list-packages) ;; bring up the package menu

;; ---------------------------------- Helpful ----------------------------------

(use-package helpful
  :defer .5
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command)
  ("C-h ." . helpful-at-point)
  ("C-h o" . helpful-symbol) ;; overrides 'describe-symbol' keybinding
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))


;; ------------------------------------ Man ------------------------------------

(use-package man
  :defer 1
  :ensure nil ;; built-in
  :bind ("C-h M" . man))


;; ------------------------------------ Info -----------------------------------

(use-package info
  :defer 1
  :ensure nil ;; built-in
  :bind
  (:map Info-mode-map
		("m" . bookmark-set) ;; note:overrides menu selection
		("M" . Info-menu)) ;; so menu selection goes here
  :preface
  (defun open-with-info-mode ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (kill-buffer (current-buffer))
      (info file-name)))
  :hook
  (info-mode . info-persist-history-mode)
  :init
  ;; add personal info files in emacs config assets directory
  ;; BUG: This causes an error on launch
  (push (concat user-emacs-directory "assets/info") Info-directory-list)
  (add-to-list 'auto-mode-alist '("\\.info\\'" . open-with-info-mode)))


(provide 'help-config)
;;; help-utils.el ends here
