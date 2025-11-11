;;; org-drill-config.el --- Org Drill Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Notes: Org-Drill
;; Start out your org-drill with C-d s, then select your file.

;; Capture templates:
;; - "d" for web/EPUB drill captures (uses %i for selected text, %:link for source)
;; - "f" for PDF drill captures (uses special PDF region extraction)

;; The javascript bookmarklet for capturing from web to org-drill:
;; javascript:location.href='org-protocol://capture?template=d&url=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection());void(0);

;; Create a bookmark, add "Drill Entry" to the name field and the above snippet to the URL field.

;;; Code:

;; --------------------------------- Org Drill ---------------------------------

(use-package org-drill
  :vc (:url "https://github.com/cjennings/org-drill"
       :branch "main"
       :rev :newest)
  :after (org org-capture)
  :demand t
  :commands (org-drill cj/drill-start)
  :config
  (setq org-drill-leech-failure-threshold 50)           ;; leech cards = 50 wrong anwers
  (setq org-drill-leech-method 'warn)                   ;; leech cards show warnings
  (setq org-drill-use-visible-cloze-face-p t)           ;; cloze text show up in a different font
  (setq org-drill-hide-item-headings-p t)               ;; don't show heading text
  (setq org-drill-maximum-items-per-session 100)        ;; drill sessions end after 100 cards
  (setq org-drill-maximum-duration 30)                  ;; each drill session can last up to 30 mins
  (setq org-drill-add-random-noise-to-intervals-p t)    ;; slightly vary number of days to repetition

  ;; ------------------------------ Display Settings -----------------------------

  ;; Customize how drill cards are displayed: bigger text + proportional font
  ;; Only affects the drill buffer, not other buffers

  (defun cj/org-drill-setup-display ()
    "Set up larger text and proportional font for drill sessions.
This runs when each drill card is displayed. Uses buffer-local settings
so it doesn't affect other buffers."
    ;; Only apply in org-mode buffers (drill files)
    (when (derived-mode-p 'org-mode)
      ;; Make text bigger (3 = 30% larger, adjust to taste)
      (text-scale-set 3)

      ;; Use proportional (variable-width) font for readability
      ;; This is buffer-local so won't affect other buffers
      (variable-pitch-mode 1)

      ;; Optional: Center the text for better focus (requires visual-fill-column package)
      ;; (when (fboundp 'visual-fill-column-mode)
      ;;   (setq-local visual-fill-column-width 100)
      ;;   (setq-local visual-fill-column-center-text t)
      ;;   (visual-fill-column-mode 1))
      ))

  ;; Hook runs when each answer is displayed
  (add-hook 'org-drill-display-answer-hook #'cj/org-drill-setup-display)

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
	(let* ((choices (directory-files drill-dir nil "^[^.].*\\.org$"))
		   (chosen-drill-file (completing-read "Choose Flashcard File:" choices)))
	  (find-file (concat drill-dir chosen-drill-file))))

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

  ;; ------------------------------ Org Drill Keymap -----------------------------

  ;; Org drill operations keymap
  (defvar-keymap cj/drill-map
	:doc "Keymap for org-drill"
	"s" #'cj/drill-start
	"e" #'cj/drill-edit
	"c" #'cj/drill-capture
	"r" #'cj/drill-refile
	"R" #'org-drill-resume)

  (keymap-set cj/custom-keymap "D" cj/drill-map)
  (with-eval-after-load 'which-key
	(which-key-add-key-based-replacements
      "C-; D" "org-drill menu"
      "C-; D s" "start drill"
      "C-; D e" "edit drill file"
      "C-; D c" "capture question"
      "C-; D r" "refile to drill"
      "C-; D R" "resume drill")))

(provide 'org-drill-config)
;;; org-drill-config.el ends here.
