;;; org-drill-config.el --- Org Drill Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Notes: Org-Drill
;; Start out your org-drill with C-d s, then select your file.

;; the javascript bookmark I use to capture information from the web for org-drill files:
;; javascript:location.href='org-protocol://capture?template=d&url=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection());void(0);

;; create a new bookmark and add "Drill Entry" to the name field and the above
;; snippet to the URL field.

;;; Code:

(require 'user-constants)
(require 'org-capture-config) ;; for adding org-capture-templates
(require 'cl-lib)

;; --------------------------------- Org Drill ---------------------------------

(use-package org-drill
  :after org
  :config
  (setq org-drill-leech-failure-threshold 50)           ;; leech cards = 50 wrong anwers
  (setq org-drill-leech-method 'warn)                   ;; leech cards show warnings
  (setq org-drill-use-visible-cloze-face-p t)           ;; cloze text show up in a different font
  (setq org-drill-hide-item-headings-p t)               ;; don't show heading text
  (setq org-drill-maximum-items-per-session 1000)       ;; drill sessions end after 1000 cards
  (setq org-drill-maximum-duration 60)                  ;; each drill session can last up to a an hour
  (setq org-drill-add-random-noise-to-intervals-p t)    ;; slightly vary number of days to repetition

  (defun cj/drill-start ()
	"Prompt user to pick a drill org file, then start an org-drill session."
	(interactive)
	(let* ((choices (directory-files drill-dir nil "^[^.].*\\.org$"))
		   (chosen-drill-file (completing-read "Choose Flashcard File:" choices)))
	  (find-file (concat drill-dir chosen-drill-file))
      (org-drill)))

  (defun cj/drill-edit ()
	"Prompts the user to pick a drill org file, then opens it for editing."
	(interactive)
	(let* ((choices (directory-files drill-dir nil "^[^.].*\\.org$")))
	  (find-file chosen-drill-file)))

  (defun cj/drill-capture ()
	"Quickly capture a drill question."
	(interactive)
	(org-capture nil "d"))

  (defun cj/drill-refile ()
	"Refile to a drill file."
	(interactive)
	(setq org-refile-targets '((nil :maxlevel . 1)
							   (drill-dir :maxlevel . 1)))
	(call-interactively 'org-refile))

  ;; add useful org drill capture templates
  (setq org-capture-templates
		(append org-capture-templates
				'(("d" "Drill Question - Web" entry
				   (file (lambda ()
						   (let ((files (directory-files drill-dir nil "^[^.].*\\.org$")))
							 (expand-file-name
							  (completing-read "Choose file: " files)
							  drill-dir))))
				   "* Item   :drill:\n%?\n** Answer\n%i\nSource: [[%:link][%:description]]\nCaptured On: %U" :prepend t)

				  ("b" "Drill Question - EPUB" entry
				   (file (lambda ()
						   (let ((files (directory-files drill-dir nil "^[^.].*\\.org$")))
							 (expand-file-name
							  (completing-read "Choose file: " files)
							  drill-dir))))
				   "* Item   :drill:\n%?\n** Answer\n%i\nSource: [[%:link][%(buffer-name (org-capture-get :original-buffer))]]\nCaptured On: %U" :prepend t)

				  ("f" "Drill Question - PDF" entry
				   (file (lambda ()
						   (let ((files (directory-files drill-dir nil "^[^.].*\\.org$")))
							 (expand-file-name
							  (completing-read "Choose file: " files)
							  drill-dir))))
				   "* Item   :drill:\n%?\n** Answer\n%(cj/org-capture-pdf-active-region)\nSource:[[%L][%(buffer-name (org-capture-get :original-buffer))]]\nCaptured On: %U" :prepend t)))))

;; ------------------------------ Org Drill Keymap -----------------------------

;; Buffer & file operations prefix and keymap
(define-prefix-command 'cj/drill-map nil
					   "Keymap for org-drill.")
(define-key cj/custom-keymap "D" 'cj/drill-map)
(define-key cj/drill-map "s" 'cj/drill-start)
(define-key cj/drill-map "e" 'cj/drill-edit)
(define-key cj/drill-map "c" 'cj/drill-capture)
(define-key cj/drill-map "r" 'cj/drill-refile)
(define-key cj/drill-map "R" 'org-drill-resume)

;;(define-key cj/drill-map "P" 'cj/disabled)
;;(define-key cj/drill-map "b" 'cj/disabled)
;;(define-key cj/drill-map "d" 'cj/disabled)
;;(define-key cj/drill-map "l" 'cj/disabled)
;;(define-key cj/drill-map "m" 'cj/disabled)
;;(define-key cj/drill-map "p" 'cj/disabled)
;;(define-key cj/drill-map "t" 'cj/disabled)
;;(define-key cj/drill-map "x" 'cj/disabled)


(provide 'org-drill-config)
;;; org-drill-config.el ends here.
