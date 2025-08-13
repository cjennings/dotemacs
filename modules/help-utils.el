;;; help-utils --- Help Integrations and Searches -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

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
