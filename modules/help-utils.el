;;; help-utils --- Help Additions and Preferences -*- lexical-binding: t; -*-
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

;; ---------------------------------- Devdocs ----------------------------------

(use-package devdocs
  :defer 1
  :config
  (global-set-key (kbd "C-h D s") 'devdocs-search)
  (global-set-key (kbd "C-h D b") 'devdocs-peruse)
  (global-set-key (kbd "C-h D l") 'devdocs-lookup)
  (global-set-key (kbd "C-h D i") 'devdocs-install)
  (global-set-key (kbd "C-h D d") 'devdocs-delete)
  (global-set-key (kbd "C-h D u") 'devdocs-update-all)
  (define-key devdocs-mode-map "b" 'devdocs-go-back)
  (define-key devdocs-mode-map "f" 'devdocs-go-forward))

;; ------------------------------------ TLDR -----------------------------------

(use-package tldr
  :defer 1
  :bind ("C-h T" . tldr))

;; -------------------------------- Wiki Summary -------------------------------

(use-package wiki-summary
  :defer 1
  :bind ("C-h W" . wiki-summary))

;; --------------------------- Browse Local Arch Wiki --------------------------
;; on Arch: yay (or whatever your AUR package manager is) -S  arch-wiki-docs
;; browse the arch wiki topics offline


(defun cj/local-arch-wiki-search ()
  (interactive)
  (let* ((dir "/usr/share/doc/arch-wiki/html/en")
		 (full-filenames (directory-files dir t "\\.html\\'"))
		 (basenames (mapcar 'file-name-base full-filenames))
		 (chosen (completing-read "Choose an ArchWiki Topic: " basenames)))
	(if (member chosen basenames)
		(let* ((idx (cl-position chosen basenames :test 'equal))
			   (fullname (nth idx full-filenames))
			   (url (concat "file://" fullname)))
		  (eww-browse-url url))
	  (message "File not found! Is arch-wiki-docs installed?"))))
(global-set-key (kbd "C-h A") 'cj/local-arch-wiki-search)

(provide 'help-utils)
;;; help-utils.el ends here
