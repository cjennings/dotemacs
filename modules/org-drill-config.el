;;; org-drill-config.el --- Org Drill Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Notes: Org-Drill
;; `C-; D s' picks a flashcard file from `drill-dir' and starts a session;
;; `C-u C-; D s' lets you pick the directory first.  `C-; D f' drills
;; whatever Org file is current, so any drill file anywhere works.

;; Capture templates:
;; - "d" for web/EPUB drill captures (uses %i for selected text, %:link for source)
;; - "f" for PDF drill captures (uses special PDF region extraction)

;; The javascript bookmarklet for capturing from web to org-drill:
;; javascript:location.href='org-protocol://capture?template=d&url=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection());void(0);

;; Create a bookmark, add "Drill Entry" to the name field and the above snippet to the URL field.

;;; Code:

;; --------------------------------- Org Drill ---------------------------------

(use-package org-drill
  ;; :vc (:url "git@cjennings.net:org-drill.git"
  ;;      :branch "main"
  ;;      :rev :newest)
  :load-path "~/code/org-drill"  ;; local dev checkout — switch back to :vc above when done
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

  ;; Configure display settings for drill sessions
  (setq org-drill-text-size-during-session 24)          ;; 24-point font for comfortable reading
  (setq org-drill-use-variable-pitch t)                 ;; use variable-pitch font for readability
  (setq org-drill-hide-modeline-during-session t)       ;; hide modeline for cleaner display

  (defun cj/--drill-files-in (dir)
	"Return the drill Org file names directly inside DIR (no leading dots)."
	(directory-files dir nil "^[^.].*\\.org$"))

  (defun cj/--drill-pick-file (dir)
	"Prompt for one of the drill Org files in DIR; return its absolute path."
	(expand-file-name
	 (completing-read "Choose flashcard file: " (cj/--drill-files-in dir) nil t)
	 dir))

  (defun cj/--drill-pick-dir (other-dir)
	"Return the directory to pick drill files from.
With OTHER-DIR non-nil, prompt for one; otherwise use `drill-dir'."
	(if other-dir (read-directory-name "Drill files in: ") drill-dir))

  (defun cj/drill-start (&optional other-dir)
	"Pick a drill Org file and start an `org-drill' session.
With a prefix arg OTHER-DIR, prompt for the directory to choose from
instead of the default `drill-dir'."
	(interactive "P")
	(find-file (cj/--drill-pick-file (cj/--drill-pick-dir other-dir)))
	(org-drill))

  (defun cj/drill-this-file ()
	"Start an `org-drill' session on the current Org buffer.
Use this to drill any drill file you have open, wherever it lives."
	(interactive)
	(unless (derived-mode-p 'org-mode)
	  (user-error "Not an Org buffer -- visit a `.org' file first"))
	(org-drill))

  (defun cj/drill-edit (&optional other-dir)
	"Pick a drill Org file and open it for editing.
With a prefix arg OTHER-DIR, prompt for the directory instead of `drill-dir'."
	(interactive "P")
	(find-file (cj/--drill-pick-file (cj/--drill-pick-dir other-dir))))

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
	"f" #'cj/drill-this-file
	"e" #'cj/drill-edit
	"c" #'cj/drill-capture
	"r" #'cj/drill-refile
	"R" #'org-drill-resume)

  (keymap-set cj/custom-keymap "D" cj/drill-map)
  (with-eval-after-load 'which-key
	(which-key-add-key-based-replacements
      "C-; D" "org-drill menu"
      "C-; D s" "start drill (C-u: pick dir)"
      "C-; D f" "drill current file"
      "C-; D e" "edit drill file (C-u: pick dir)"
      "C-; D c" "capture question"
      "C-; D r" "refile to drill"
      "C-; D R" "resume drill")))

(provide 'org-drill-config)
;;; org-drill-config.el ends here.
