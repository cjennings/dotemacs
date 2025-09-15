;;; help-config --- Help Functionality Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; This module enhances Emacs' built-in help system and documentation features.
;; It configures:
;;
;; 1. Helpful - A better help buffer that provides context, examples, and source code
;; 2. Man - Man page viewing integration
;; 3. Info - Enhanced Info mode with custom keybindings and directory configuration
;;
;; The configuration prioritizes discoverability and improves the experience of
;; reading documentation within Emacs. Custom keybindings maintain the C-h prefix
;; convention for help-related commands.

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
  ;; These variables are set by counsel which is loaded in selection-framework.el
  (with-eval-after-load 'counsel
	(setq counsel-describe-function-function #'helpful-callable)
	(setq counsel-describe-variable-function #'helpful-variable)))

;; ------------------------------------ Man ------------------------------------

(use-package man
  :defer 1
  :ensure nil ;; built-in
  :bind ("C-h M" . man))

;; ------------------------------------ Info -----------------------------------

(use-package info
  :ensure nil ;; built-in
  :bind
  (:map Info-mode-map
		("m" . bookmark-set) ;; Rebind 'm' from Info-menu to bookmark-set
		("M" . Info-menu))   ;; Move Info-menu to 'M' instead
  :preface
  (defun cj/open-with-info-mode ()
	"Open the current buffer's file in Info mode if it's a valid info file.
Preserves any unsaved changes and checks if the file exists."
	(interactive)
	(let ((file-name (buffer-file-name)))
	  (when file-name
		(if (and (file-exists-p file-name)
				 (string-match-p "\\.info\\'" file-name))
			(progn
			  (when (buffer-modified-p)
				(if (y-or-n-p "Buffer has unsaved changes. Save before opening in Info? ")
					(save-buffer)
				  (message "Operation canceled")
				  (cl-return-from cj/open-with-info-mode)))
			  (kill-buffer (current-buffer))
			  (info file-name))
		  (message "Not a valid info file: %s" file-name)))))
  :init
  ;; Add personal info files BEFORE Info mode initializes
  (let ((personal-info-dir (expand-file-name "assets/info" user-emacs-directory)))
	(when (file-directory-p personal-info-dir)
	  (setq Info-directory-list (list personal-info-dir))))
  ;; the above makes the directory the info list. the below adds it to the default list
  ;;      (add-to-list 'Info-default-directory-list personal-info-dir)))
  :hook
  (info-mode . info-persist-history-mode)
  :config
  ;; Make .info files open with our custom function
  (add-to-list 'auto-mode-alist '("\\.info\\'" . cj/open-with-info-mode)))

(provide 'help-config)
;;; help-config.el ends here.
