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

;; ------------------------------- Org Constants -------------------------------

;; note: some constants used here are defined in init.el
(defvar org-archive-location
  (concat sync-dir "/archives/archive.org::datetree/")
  "Location of the archive file.
The archive file is where org entries that are archived via
org-archive-subtree-default are placed.")

;; ---------------------------- Org General Settings ---------------------------

(defun cj/org-general-settings ()
  "All general \='org-mode\=' settings are grouped and set in this function."

  ;; Unbind org-cycle-agenda-files keys for use elsewhere
  (unbind-key "C-'" org-mode-map)
  (unbind-key "C-," org-mode-map)

  ;; ORG-MODULES
  ;; enable recognition of org-protocol:// as a parameter
  ;; add org-habits
  (require 'org-protocol)
  (setq org-modules '(org-protocol ol-eww ol-w3m ol-info org-habit))

  ;; GENERAL
  (setq org-startup-folded t)               ;; all org files should start in the folded state
  (setq org-cycle-open-archived-trees t)    ;; re-enable opening headings with archive tags with TAB
  (setq org-outline-path-complete-in-steps nil)
  (setq org-return-follows-link t)          ;; hit return to follow an org-link
  (setq org-list-allow-alphabetical t)      ;; allow alpha ordered lists (i.e., a), A), a., etc.)

  ;; INDENTATION
  (setq org-startup-indented t)             ;; load org files indented
  (setq org-adapt-indentation t)            ;; adapt indentation to outline node level
  (setq org-indent-indentation-per-level 2) ;; indent two character-widths per level

  ;; INLINE IMAGES
  (setq org-startup-with-inline-images t)   ;; preview images by default
  (setq org-image-actual-width '(500))      ;; keep image sizes in check

  (setq org-bookmark-names-plist nil)       ;; don't set org-capture bookmarks

  ;; force pdfs exported from org to open in emacs
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))

;; -------------------------- Org Appearance Settings --------------------------

(defun cj/org-appearance-settings()
  "Set foreground, background, and font styles for org mode."
  (interactive)
  ;; org-hide should use fix-pitch to align indents for proportional fonts
  (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit 'shadow)

  ;; Remove foreground and background from block faces
  (set-face-attribute 'org-block nil :foreground 'unspecified :background 'unspecified)
  (set-face-attribute 'org-block-begin-line nil :foreground 'unspecified :background 'unspecified)
  (set-face-attribute 'org-block-end-line nil :foreground 'unspecified :background 'unspecified)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background 'unspecified)
  (set-face-attribute 'org-column-title nil :background 'unspecified)

  ;; make sure org-links are underlined
  (set-face-attribute 'org-link nil :underline t)

  (setq org-ellipsis " ▾")                                  ;; change ellipses to down arrow
  (setq org-hide-emphasis-markers t)                        ;; remove emphasis markers to keep the screen clean
  (setq org-hide-leading-stars t)                           ;; hide leading stars, just show one per line
  (setq org-pretty-entities t)                              ;; render special symbols
  (setq org-pretty-entities-include-sub-superscripts nil)   ;; ...except superscripts and subscripts
  (setq org-fontify-emphasized-text nil)                    ;; ...and don't render bold and italic markup
  (setq org-fontify-whole-heading-line t)                   ;; fontify the whole line for headings (for face-backgrounds)
  (add-hook 'org-mode-hook 'prettify-symbols-mode))

;; ----------------------------- Org TODO Settings ---------------------------

(defun cj/org-todo-settings ()
  "All org-todo related settings are grouped and set in this function."

  ;; logging task creation, task start, and task resolved states
  (setq org-todo-keywords '((sequence "TODO(t!)" "PROJECT(p)" "DOING(i!)"
									  "WAITING(w)" "VERIFY(v)" "STALLED(s)"
									  "DELEGATED(x)" "|"
									  "FAILED(f!)" "DONE(d!)" "CANCELLED(c!)")))

  (setq org-todo-keyword-faces
		'(("TODO"      . "green")
		  ("PROJECT"   . "blue")
		  ("DOING"     . "yellow")
		  ("WAITING"   . "white")
		  ("VERIFY"    . "orange")
		  ("STALLED"   . "light blue")
		  ("DELEGATED" . "green")
		  ("FAILED"    . "red")
		  ("DONE"      . "dark grey")
		  ("CANCELLED" . "dark grey")))

  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?D)
  (setq org-default-priority ?D)
  (setq org-priority-faces '((?A . (:foreground "Cyan" :weight bold))
							 (?B . (:foreground "Yellow"))
							 (?C . (:foreground "Green"))
							 (?D . (:foreground "Grey"))))

  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-deadline-warning-days 7)    ;; warn me w/in a week of deadlines
  (setq org-treat-insert-todo-heading-as-state-change t) ;; log task creation
  (setq org-log-into-drawer t) ;; log into the drawer
  (setq org-habit-graph-column 75) ;; allow space for task name

  ;; inherit parents properties (sadly not schedules or deadlines)
  (setq org-use-property-inheritance t))

;; ---------------------------------- Org Mode ---------------------------------

(use-package org
  :defer .5
  :ensure nil ;; use the built-in package
  :pin manual ;; never upgrade from the version built-into Emacs
  :preface
  ;; create an org-table-map so we can use C-c t as prefix
  (define-prefix-command 'org-table-map)
  (global-set-key (kbd "C-c T") 'org-table-map)
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
		("C-c ?"     . hydra-general/body)   ;; was org-table-field-info
		("C-c I"     . org-table-field-info) ;; was C-c ?
		("C-\\"      . org-match-sparse-tree)
		("C-c t"     . org-set-tags-command)
		("C-c l"     . org-store-link)
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

  :config
  (cj/org-general-settings)
  (cj/org-appearance-settings)
  (cj/org-todo-settings))


;; ------------------------------- Org Superstar -------------------------------

;; nicer bullets than simple asterisks.
(use-package org-superstar
  :after org
  :config
  (org-superstar-configure-like-org-bullets)
  (setq org-superstar-leading-bullet ?\s)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; ------------------------------- Org-Checklist -------------------------------
;; needed for org-habits to reset checklists once task is complete
;; this was a part of org-contrib which was deprecated

(use-package org-checklist
  :ensure nil ;; in custom folder
  :after org
  :load-path "custom/org-checklist.el")

;; -------------------------- Org Link To Current File -------------------------
;; get a link to the file the current buffer is associated with.

(defun cj/org-link-to-current-buffer-file ()
  "Create an Org mode link to the current file and copy it to the clipboard.

The link is formatted as [[file:<file-path>][<file-name>]],
where <file-path> is the full path to the current file and <file-name>
is the name of the current file without any directory information.

If the current buffer is not associated with a file, the function will throw an
error."
  (interactive)
  (if (buffer-file-name)
      (let* ((filename (buffer-file-name))
             (description (file-name-nondirectory filename))
             (link (format "[[file:%s][%s]]" filename description)))
        (kill-new link)
        (message "Copied Org link to current file to clipboard: %s" link))
    (user-error "Buffer isn't associated with a file, so no link sent to clipboard")))

(provide 'org-config)
;;; org-config.el ends here
