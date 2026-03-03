;;; org-export-config.el --- Org Export Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; This module configures Org mode's export capabilities, providing multiple
;; backend options for converting Org documents to various formats.
;;
;; Built-in backends configured:
;; - LaTeX/PDF: Academic documents and presentations
;; - HTML: Web publishing with HTML5 support
;; - Markdown: README files and web content
;; - ODT: Office documents for LibreOffice/MS Word
;; - Texinfo: GNU documentation and Info files
;;
;; Extended via Pandoc:
;; - Additional formats: DOCX, self-contained HTML5
;; - Custom PDF export with Zathura integration
;;
;; Key features:
;; - UTF-8 encoding enforced across all backends
;; - Subtree export as default scope
;;
;; Note: reveal.js presentations are handled by org-reveal-config.el (C-; p)
;;
;;; Code:

(require 'system-lib)

;; --------------------------------- Org Export --------------------------------

(use-package ox
  :defer .5
  :ensure nil
  :after org
  :config
  ;; load the built-in backends
  (dolist (feat '(ox-odt ox-latex ox-md ox-ascii))
	(require feat))

  ;; now tell Org exactly which backends to use
  (setq org-export-backends '(odt latex md ascii))

  ;; Other Settings
  (setq org-export-preserve-breaks t)        ;; keep line breaks in all Org export back-ends
  (setq org-export-coding-system 'utf-8)     ;; force utf-8 in org
  (setq org-export-headline-levels 6)        ;; export headlines 6 levels deep
  (setq org-export-with-section-numbers nil) ;; export without section numbers by default
  (setq org-export-with-tags nil)            ;; export without tags by default
  (setq org-export-with-tasks '("TODO"))     ;; export with tasks by default
  (setq org-export-with-tasks nil)           ;; export WITHOUT tasks by default
  (setq org-export-with-toc t)               ;; export WITH table of contents by default
  (setq org-export-initial-scope 'buffer)   ;; 'subtree is your other choice
  (setq org-export-with-author nil))         ;; export without author by default

(use-package ox-html
  :ensure nil ; Built into Org
  :defer t
  :after ox
  :config
  (setq org-html-postamble nil)
  (setq org-html-html5-fancy t)
  (setq org-html-head-include-default-style nil))


(use-package ox-texinfo
  :ensure nil  ; Built into Org
  :defer t
  :after ox
  :config
  (setq org-texinfo-coding-system 'utf-8)
  (setq org-texinfo-default-class "info")
  (add-to-list 'org-export-backends 'texinfo))

(use-package ox-pandoc
  :defer t
  :after ox
  :config
  ;; Set default options for pandoc
  (setq org-pandoc-options '((standalone . t)
							 (mathjax . t)))

  ;; Configure specific format options
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
  (setq org-pandoc-options-for-html5 '((html-q-tags . t)
									   (self-contained . t)))
  (setq org-pandoc-options-for-markdown '((atx-headers . t)))

  ;; Custom function to export to PDF and open with Zathura
  (defun my/org-pandoc-export-to-pdf-and-open ()
	"Export to PDF via pandoc and open with Zathura."
	(interactive)
	(let ((pdf-file (org-pandoc-export-to-latex-pdf)))
	  (when pdf-file
		(start-process "zathura-pdf" nil "zathura" pdf-file)
		(message "Opened %s in Zathura" pdf-file))))

  ;; Pandoc export menu entries
  (setq org-pandoc-menu-entry
		'((?4 "to html5 and open" org-pandoc-export-to-html5-and-open)
		  (?$ "as html5" org-pandoc-export-as-html5)
		  (?< "to markdown" org-pandoc-export-to-markdown)
		  (?d "to docx and open" org-pandoc-export-to-docx-and-open)
		  (?z "to pdf and open (Zathura)" my/org-pandoc-export-to-pdf-and-open))))

;; github flavored markdown
;; (use-package ox-gfm
;;   :after ox)

;; JIRA markup
;; (use-package ox-jira
;;   :after ox)

;; Confluence Wiki markup
;; (use-package ox-confluence
;;   :after ox)

(provide 'org-export-config)
;;; org-export-config.el ends here.
