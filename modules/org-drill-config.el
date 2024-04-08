;;; org-drill-config.el --- Org Drill Settings -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Notes: Org-Drill
;; Start out your org-drill with C-d s, then select your file.

;; the javascript bookmark I use to capture information from the web is below:
;; javascript:location.href='org-protocol://capture?template=d&url=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection())

;; create a new bookmark and add "Drill Entry" to the name field and the above
;; snippet to the URL field.

;; ----------------------------------- Tasks -----------------------------------


;;; Code:
(require 'custom-functions) ;; for cj/merge-list-to-list

(defvar org-drill-files (directory-files (concat sync-dir "drill/")
										 t directory-files-no-dot-files-regexp))

(with-eval-after-load 'org
  (cj/merge-list-to-list
   'org-capture-templates
   '(("d" "Drill Question - Web"
	  entry
	  (file (lambda () (completing-read "Choose file: " org-drill-files)))
	  "* Item   :drill:\n%?\n** Answer\n%i\nSource: [[%:link][%:description]]\nCaptured On: %U" :prepend t)
	 ("b" "Drill Question - EPUB"
	  entry
	  (file (lambda () (completing-read "Choose file: " org-drill-files)))
	  "* Item   :drill:\n%?\n** Answer\n%i\nSource: [[%:link][%(buffer-name (org-capture-get :original-buffer))]]\nCaptured On: %U" :prepend t)
	 ("f" "Drill Question - PDF"
	  entry
	  (file (lambda () (completing-read "Choose file: " org-drill-files)))
	  "* Item   :drill:\n%?\n** Answer\n%(org-capture-pdf-active-region)\nSource:[[%L][%(buffer-name (org-capture-get :original-buffer))]]\nCaptured On: %U" :prepend t))))


(use-package org-drill
  :after org
  :preface
  (defun cj/drill-start ()
	"Prompt user to pick a drill org file, then starts an org-drill session."
	(interactive)
	(let ((choices org-drill-files))
	  (setq chosen-drill-file (completing-read "Choose Flashcard File:" choices ))
	  (find-file chosen-drill-file)
	  (org-drill)))

  (defun cj/drill-edit ()
	"Prompts the user to pick a drill org file, then opens it for editing."
	(interactive)
	(let* ((choices org-drill-files)
		   (chosen-drill-file (completing-read "Choose Flashcards to Edit:" choices)))
	  (find-file chosen-drill-file)))

  (defun cj/drill-capture ()
	"Quickly capture a drill question."
	(interactive)
	(org-capture nil "d"))

  (defun cj/drill-refile ()
	"Refile to a drill file."
	(interactive)
	(setq org-refile-targets '((nil :maxlevel . 1)
							   (org-drill-files :maxlevel . 1)))
	(call-interactively 'org-refile))

  ;; create an org-drill-map so you can use C-d as prefix
  (define-prefix-command 'org-drill-map)
  (global-set-key (kbd "C-d") 'org-drill-map)
  :bind
  (:map org-drill-map
		("s" . cj/drill-start)
		("e" . cj/drill-edit)
		("c" . cj/drill-capture)
		("r" . cj/drill-refile)
		("R" . org-drill-resume))
  :config
  (setq org-drill-leech-failure-threshold 50)           ;; leech cards = 50 wrong anwers
  (setq org-drill-leech-method 'warn)                   ;; leech cards show warnings
  (setq org-drill-use-visible-cloze-face-p t)           ;; cloze text show up in a different font
  (setq org-drill-hide-item-headings-p t)               ;; don't show heading text
  (setq org-drill-maximum-items-per-session 1000)       ;; drill sessions end after 1000 cards
  (setq org-drill-maximum-duration 60)                  ;; each drill session can last up to a an hour
  (setq org-drill-add-random-noise-to-intervals-p t))   ;; slightly vary number of days to repetition

(provide 'org-drill-config)
;;; org-drill-config.el ends here.
