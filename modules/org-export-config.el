;;; org-export-config.el --- Org Export Configuration -*- lexical-binding: t; coding: utf-8; -*-
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
				  ox-odt))
	(require feat))

  ;; now tell Org exactly which backends to use
  (setq org-export-backends
		'(ascii
		  beamer
		  html
		  latex
		  md
		  odt))

  ;; Other Settings
  (setq org-export-preserve-breaks t)        ;; keep line breaks in all Org export back-ends
  (setq org-export-coding-system 'utf-8)     ;; force utf-8 in org
  (setq org-export-headline-levels 6)        ;; export headlines 6 levels deep
  (setq org-export-initial-scope 'subtree)   ;; export the current subtree by default
  (setq org-export-with-author nil)          ;; export without author by default
  (setq org-export-with-section-numbers nil) ;; export without section numbers by default
  (setq org-export-with-tags nil)            ;; export without tags by default
  (setq org-export-with-tasks '("TODO"))     ;; export with tasks by default
  ;;  (setq org-export-with-tasks nil)           ;; export WITHOUT tasks by default
  ;;  (setq org-export-with-toc nil)             ;; export without table of contents by default
  (setq org-export-with-toc t))              ;; export WITH table of contents by default

;; hugo markdown
(use-package ox-hugo
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

;; ------------------------------- Org Tufte CSS -------------------------------
;; Hacks to inline a superior tufte.css style sheet into your generated html files.

;; WIP
;; Optionally inline some css into all generated HTML documents
;; NOTE You should be able to choose what css you want inlined first!

(defun cj/org-inline-css (path)
  "Return the contents of the file at PATH as a string.
If the file doesn’t exist, return an empty string."
  (if (file-readable-p path)
	  (with-temp-buffer
		(insert-file-contents path)
		(buffer-string))
	(progn
	  (message "[org-inline-css] could not read %s" path)
	  "")))

(with-eval-after-load 'ox-html
  ;; Disable the postamble (footer) completely:
  (setq org-html-postamble nil)

  ;; inject tufte.css from the assets folder.
  (setq org-html-html5-fancy t)
  (setq org-html-head-include-default-style nil))
;; org-html-head-extra
;; (let* ((css-file (expand-file-name "assets/tufte.css"
;;                                 user-emacs-directory))
;;     (css      (cj/org-inline-css css-file)))
;;   (format "<style type=\"text/css\">\n%s\n</style>" css))))
;;
;; Don't allow Table of Contents to link to TODO items
;;
(defun cj/org-html-toc-remove-todo (toc-entry backend _info)
  "Remove any <span class=\"todo …\">TODO</span> from a single TOC entry."
  (when (eq backend 'html)
    (replace-regexp-in-string
     "<span class=\"todo [^\"]*\">[^<]*</span>[[:space:]]*"
     "" toc-entry)))

(with-eval-after-load 'ox-html
  ;; register our filter so each TOC line is run through it
  (add-hook 'org-export-filter-toc-entry-functions
            #'cj/org-html-toc-remove-todo))

(provide 'org-export-config)
;;; org-export-config.el ends here.
