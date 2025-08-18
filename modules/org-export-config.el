;;; org-export-config.el --- Org Export Configuration -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:

;;; Code:

;; --------------------------------- Org Export --------------------------------
;; backends for exporting from org-mode

(use-package ox
  :defer .5
  :ensure nil
  :after org
  :config
  ;; load every built-in backend
  (dolist (feat '(ox-ascii
				  ox-beamer
				  ox-html
				  ox-icalendar
				  ox-latex
				  ox-md
				  ox-odt
				  ox-texinfo
				  ox-man))
	(require feat))

  ;; now tell Org exactly which backends to use
  (setq org-export-backends
		'(ascii
		  beamer
		  html
		  icalendar
		  latex
		  md
		  odt
		  org
		  texinfo
		  man))

  ;; Other Settings
  (setq org-export-coding-system 'utf-8)        ;; force utf-8 in org
  (setq org-export-headline-levels 6)           ;; export headlines 6 levels deep
  (setq org-export-initial-scope 'subtree)      ;; export the current subtree by default
  (setq org-export-with-author nil)             ;; export without author by default
  (setq org-export-with-section-numbers nil)    ;; export without section numbers by default
  (setq org-export-with-tags nil)               ;; export without tags by default
  (setq org-export-with-tasks '("TODO"))        ;; export with tasks by default
  (setq org-export-with-toc nil))               ;; export without table of contents by default

;; hugo markdown
(use-package ox-hugo
  :after ox)

;; export via pandoc
(use-package ox-pandoc
  :after ox)

;; github flavored markdown
(use-package ox-gfm
  :after ox)

;; reveal .js
(use-package ox-reveal
  :after ox
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

;; JIRA markup
;; (use-package ox-jira
;;   :after ox)

;; ;; Confluence Wiki markup
;; (use-package ox-confluence
;;   :after ox)

(provide 'org-export-config)
;;; org-export-config.el ends here.
