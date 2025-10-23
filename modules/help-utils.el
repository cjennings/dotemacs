;;; help-utils --- Help Integrations and Searches -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; This module provides various utilities for accessing documentation and help resources
;; directly within Emacs.
;; DevDocs for programming documentation
;; TLDR command line cheat sheets
;; Wikipedia pages through the `wiki-summary` package.
;; ArchWiki pages are also browsable via EWW.
;;
;;; Keybindings:
;; - C-h D s: Search DevDocs for documentation.
;; - C-h D b: Peruse DevDocs browsing through documentation.
;; - C-h D l: Lookup specific documentation in DevDocs.
;; - C-h D i: Install documentation for a chosen library in DevDocs.
;; - C-h D d: Delete documentation from DevDocs.
;; - C-h D u: Update all installed DevDocs documentation.
;; - C-h T: Access TLDR (cheat sheets for command-line tools).
;; - C-h W: Summarize wiki pages with the wiki-summary package.
;; - C-h A: Search and browse local Arch Wiki topics in EWW.
;;
;;; Code:

;; ---------------------------------- Devdocs ----------------------------------

(use-package devdocs
  :commands (devdocs-search devdocs-peruse devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)
  :init
  (keymap-global-set "C-h D s" #'devdocs-search)
  (keymap-global-set "C-h D b" #'devdocs-peruse)
  (keymap-global-set "C-h D l" #'devdocs-lookup)
  (keymap-global-set "C-h D i" #'devdocs-install)
  (keymap-global-set "C-h D d" #'devdocs-delete)
  (keymap-global-set "C-h D u" #'devdocs-update-all)
  :config
  (define-key devdocs-mode-map "b" #'devdocs-go-back)
  (define-key devdocs-mode-map "f" #'devdocs-go-forward))

;; ------------------------------------ TLDR -----------------------------------

(use-package tldr
  :bind ("C-h T" . tldr))

;; -------------------------------- Wiki Summary -------------------------------

(use-package wiki-summary
  :bind ("C-h W" . wiki-summary))

;; --------------------------- Browse Local Arch Wiki --------------------------
;; on Arch: yay (or whatever your AUR package manager is) -S  arch-wiki-docs
;; browse the arch wiki topics offline

(defun cj/local-arch-wiki-search ()
  "Prompt for an ArchWiki topic and open its local HTML copy in EWW.

Looks for “*.html” files under \"/usr/share/doc/arch-wiki/html/en\",
lets you complete on their basenames, and displays the chosen file
with `eww-browse-url'. If no file is found, reminds you to install
arch-wiki-docs."
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
(keymap-global-set "C-h A" #'cj/local-arch-wiki-search)

(provide 'help-utils)
;;; help-utils.el ends here
