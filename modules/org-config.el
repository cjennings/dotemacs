;;; org-config --- Settings and Enhancements to Org Mode -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>
;;; Commentary:

;; Note:
;; Setting org-modules to org-protocol, ol-eww, ol-w3m, and ol-info removes
;; several modules that org would otherwise load automatically.

;; For clarity and reference sake, this is what's removed in Emacs 29.1:
;; ol-doi     - links to Digital Object Identifiers. See: https://www.doi.org/
;; ol-bbdb    - implements links to BBDB database entries
;; ol-bibtex  - implements links to database entries in BibTeX files
;; ol-docview - implements links to open files in doc-view-mode
;; ol-gnus    - implements links to Gnus groups and messages
;; ol-irc     - implements links to an IRC session
;; ol-mhe     - implements links to MH-E (Rand Mail Handler) messages
;; ol-rmail   - implements links to Rmail messages

;;; Code:

;;;; --------------------------- Constants ---------------------------

;; note: some constants used here are defined in init.el
(defvar org-archive-location (concat sync-dir "/archives/archive.org::datetree/"))  ;; location of archive file
(defvar org-project-files (list schedule-file))

;; ---------------------------- APT Sorting Function ---------------------------

(defun cj/org-reorder-list-apt ()
  "Sort the org header by three criteria: alpha, pri, then todo."
  (interactive)
  (save-excursion
	(ignore-errors
		(progn
		  (org-sort-entries t ?a)
		  (org-sort-entries t ?p)
		  (org-sort-entries t ?t)
		  (org-cycle)
		  (org-cycle)))))

;; ---------------------------------- Org Mode ---------------------------------

(use-package org
  :defer .5
  :ensure nil ;; use the built-in package
  :pin manual ;; never upgrade from the version built-into Emacs
  :preface
  ;; create an org-table-map so you can use C-c t as prefix
  (define-prefix-command 'org-table-map)
  (global-set-key (kbd "C-c T") 'org-table-map)
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
		("C-c ?"     . hydra-general/body)   ;; was org-table-field-info
		("C-c I"     . org-table-field-info) ;; was C-c ?
		("C-c w"     . cj/org-refile-in-file)
		("C-\\"      . org-match-sparse-tree)
		("C-c t"     . org-set-tags-command)
		("C-c l"     . org-store-link)
		("C-c r"     . cj/org-reorder-list-apt)
		("C-c C-l"   . org-insert-link)
		("s-<up>"    . org-priority-up)
		("s-<down>"  . org-priority-down)
		("C-c N"     . org-narrow-to-subtree)
		("C-c >"     . cj/org-narrow-forward)
		("C-c <"     . cj/org-narrow-backwards)
		("<f5>"      . org-reveal)
		("C-c <ESC>" . widen))
  (:map org-table-map
		("r i" . org-table-insert-row)
		("r d" . org-table-kill-row)
		("c i" . org-table-insert-column)
		("c d" . org-table-delete-column))

  ;; backward and forward day are ','  and '.'
  ;; shift & meta moves by week or year
  ;; C-. jumps to today
  ;; original keybindings blocked by windmove keys
  ;; these are consistent with plain-old calendar mode
  (:map org-read-date-minibuffer-local-map
        (","   . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-backward-day 1))))
        ("."   . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-forward-day 1))))
        ("<"   . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-backward-month 1))))
        (">"   . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-forward-month 1))))
        ("M-," . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-backward-year 1))))
        ("M-." . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-forward-year 1)))))

  :init
  ;; windmove's keybindings conflict with org-agenda-todo-nextset/previousset keybindings
  ;; solution:  map the super key so that
  ;; - super up/down increases and decreases the priority
  ;; - super left/right changes the todo state
  (setq org-replace-disputed-keys t)
  (custom-set-variables
   '(org-disputed-keys
     '(([(shift left)]          . [(super left)])
       ([(shift right)]         . [(super right)])
       ([(shift up)]            . [(super up)])
       ([(shift down)]          . [(super down)])
       ([(control shift right)] . [(meta shift +)])
	   ([(control shift left)]  . [(meta shift -)]))))
  :hook
  (org-mode . flyspell-mode)
  (org-mode . turn-on-visual-line-mode)
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (interactive) (company-mode -1))) ;; no company-mode in org

  :config
  ;; Unbind org-cycle-agenda-files keys for use elsewhere
  (unbind-key "C-'" org-mode-map)
  (unbind-key "C-," org-mode-map)


  ;; ORG-PROTOCOL
  ;; enable recognition of org-protocol:// as a parameter
  (require 'org-protocol)
  (setq org-modules '(org-protocol ol-eww ol-w3m ol-info))

  ;; GENERAL
  (setq org-startup-folded t)               ;; all org files should start in the folded state
  (setq org-cycle-open-archived-trees t)    ;; re-enable opening headings with archive tags with TAB
  (setq org-outline-path-complete-in-steps nil)
  (setq org-return-follows-link t)          ;; hit return to follow an org-link
  (setq org-list-allow-alphabetical t)      ;; allow alpha ordered lists (i.e., a), A), a., etc.)

  ;; INDENTATION
  (setq org-startup-indented t)                                   ;; load org files indented
  (setq org-adapt-indentation t)                                  ;; adapt indentation to outline node level
  (setq org-indent-indentation-per-level 2)                       ;; indent two character-widths per level

  ;; INLINE IMAGES
  (setq org-startup-with-inline-images t)                         ;; preview images by default
  (setq org-image-actual-width '(500))                            ;; keep image
  ;; sizes in check

  (setq org-bookmark-names-plist nil)                             ;; don't set org-capture bookmarks

  ;; force pdfs exported from org to open in emacs
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))


;; https://www.reddit.com/r/orgmode/comments/n56fcv/important_the_contrib_directory_now_lives_outside/
;; (use-package org-contrib
;;   :after org)

(provide 'org-config)
;;; org-config.el ends here
