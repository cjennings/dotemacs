;;; org-config --- Settings and Enhancements to Org Mode -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;; Commentary:

;; note: org-archive-location is set in the :config section after org loads

;;; Code:

(require 'keybindings)  ;; provides cj/custom-keymap (used in :init below)

;; ---------------------------- Org General Settings ---------------------------

(defun cj/org-general-settings ()
  "All general `org-mode' settings are grouped and set in this function."

  ;; Unbind org-cycle-agenda-files keys for use elsewhere
  (keymap-unset org-mode-map "C-'" t)
  (keymap-unset org-mode-map "C-," t)

  ;; ORG-MODULES
  ;; enable recognition of org-protocol:// as a parameter
  (setq org-modules '(org-protocol ol-eww ol-w3m ol-info))

  ;; GENERAL
  (setq org-startup-folded t)               ;; all org files should start in the folded state
  (setq org-cycle-open-archived-trees t)    ;; re-enable opening headings with archive tags with TAB
  (setq org-cycle-hide-drawers 'all)        ;; collapse :PROPERTIES: drawers when a heading folds
  (setq org-id-locations-file
        (expand-file-name "persist/org-id-locations" user-emacs-directory))
  (setopt org-outline-path-complete-in-steps nil)
  (setq org-return-follows-link t)          ;; hit return to follow an org-link
  (setq org-list-allow-alphabetical t)      ;; allow alpha ordered lists (i.e., a), A), a., etc.)

  ;; INDENTATION
  (setq org-startup-indented t)             ;; load org files indented
  (setq org-adapt-indentation t)            ;; adapt indentation to outline node level

  ;; TASK: this variable doesn't exist. Remove
  ;;  (setq org-indent-indentation-per-level 2) ;; indent two character-widths per level

  ;; IMAGES / MEDIA
  (setq org-startup-with-inline-images t)   ;; preview images by default
  (setq org-image-actual-width '(500))      ;; keep image sizes in check
  (setq org-yank-image-save-method 'attach) ;; attach images; save to data directory

  (setq org-bookmark-names-plist nil)       ;; don't set org-capture bookmarks

  ;; make org-store-link binding global
  (keymap-global-set "C-c l" #'org-store-link)

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
  (setq org-hide-emphasis-markers t)                        ;; hide emphasis markers (org-appear shows them when editing)
  (setq org-hide-leading-stars t)                           ;; hide leading stars, just show one per line
  (setq org-pretty-entities t)                              ;; render special symbols
  (setq org-pretty-entities-include-sub-superscripts nil)   ;; ...except superscripts and subscripts
  (setq org-fontify-emphasized-text t)                      ;; render bold and italic markup
  (setq org-fontify-whole-heading-line t)                   ;; fontify the whole line for headings (for face-backgrounds)
  (add-hook 'org-mode-hook 'prettify-symbols-mode))

;; ----------------------------- Org TODO Settings ---------------------------

(defun cj/org-todo-settings ()
  "All org-todo related settings are grouped and set in this function."

  ;; logging task creation, task start, and task resolved states
  (setq org-todo-keywords '((sequence "TODO(t)" "PROJECT(p)" "DOING(i)"
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
  (setq org-treat-insert-todo-heading-as-state-change nil) ;; log task creation
  (setq org-log-into-drawer nil) ;; don't log into drawer
  (setq org-log-done 'time) ;; record a CLOSED timestamp on TODO->DONE

  ;; inherit parents properties (sadly not schedules or deadlines)
  (setq org-use-property-inheritance t))

;; ---------------------------------- Org Mode ---------------------------------

(use-package org
  :defer t
  :ensure nil ;; use the built-in package
  :pin manual ;; never upgrade from the version built-into Emacs
  :init
  (defvar-keymap cj/org-map
    :doc "General org-mode operations and utilities.")
  (keymap-set cj/custom-keymap "O" cj/org-map)
  ;; Keymap conventions for this prefix:
  ;; - Table operations claim `r' (row) and `c' (column) as
  ;;   sub-prefixes, so single-key commands that would otherwise
  ;;   want lowercase `r' or `c' use capitals (e.g. `C' for clear
  ;;   element cache).
  ;; - Narrow and sparse-tree commands follow a lowercase-creates /
  ;;   uppercase-cancels pattern.  `n' narrows to the current
  ;;   subtree; `N' widens.  `s' is the sparse-tree sub-prefix and
  ;;   `s S' cancels the sparse tree.

  ;; Narrow / widen (direct, flat under the org menu)
  (keymap-set cj/org-map "n" #'org-narrow-to-subtree)
  (keymap-set cj/org-map "N" #'widen)
  (keymap-set cj/org-map ">" #'cj/org-narrow-forward)
  (keymap-set cj/org-map "<" #'cj/org-narrow-backwards)

  ;; Sparse trees: lowercase creates, capital of the same letter cancels.
  ;; Both `S' and `T' resolve to `org-show-all' -- same cancel command,
  ;; paired with each lowercase create so the mental model is "capital
  ;; cancels the lowercase command I just ran" without having to recall
  ;; which letter the cancel actually lives on.
  (keymap-set cj/org-map "s" #'org-match-sparse-tree)
  (keymap-set cj/org-map "S" #'org-show-all)
  (keymap-set cj/org-map "t" #'org-show-todo-tree)
  (keymap-set cj/org-map "T" #'org-show-all)
  (keymap-set cj/org-map "R" #'org-reveal)
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
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
        ("C-c <ESC>" . widen)
        ("C-c C-a"   . cj/org-appear-toggle))
  (:map cj/org-map
        ("r i" . org-table-insert-row)
        ("r d" . org-table-kill-row)
        ("c i" . org-table-insert-column)
        ("c d" . org-table-delete-column))

  ;; . jumps to today, < and > move by day, M-, and M-. move by month
  ;; original shift-arrow keybindings blocked by windmove keys
  (:map org-read-date-minibuffer-local-map
        ("."   . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-goto-today))))
        ("<"   . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-backward-day 1))))
        (">"   . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-forward-day 1))))
        ("M-," . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-backward-month 1))))
        ("M-." . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-forward-month 1)))))

  :init
  ;; windmove's keybindings conflict with org-agenda-todo-nextset/previousset keybindings
  ;; solution:  map the super key so that
  ;; - super up/down increases and decreases the priority
  ;; - super left/right changes the todo state
  (setq org-replace-disputed-keys t)
  (setq org-disputed-keys
        '(([(shift left)]          . [(super left)])
          ([(shift right)]         . [(super right)])
          ([(shift up)]            . [(super up)])
          ([(shift down)]          . [(super down)])
          ([(control shift right)] . [(meta shift +)])
          ([(control shift left)]  . [(meta shift -)])))

  (defun cj/org-narrow-forward ()
    "Narrow to the next subtree at the same level."
    (interactive)
    (widen)
    (org-forward-heading-same-level 1)
    (org-narrow-to-subtree))

  (defun cj/org-narrow-backwards ()
    "Narrow to the previous subtree at the same level."
    (interactive)
    (widen)
    (org-backward-heading-same-level 1)
    (org-narrow-to-subtree))

  :hook
  (org-mode . turn-on-visual-line-mode)
  (org-mode . (lambda () (setq-local tab-width 8)))

  :config
  ;; Load org-protocol for org-protocol:// URL handling
  (require 'org-protocol nil t)

  ;; Set archive location (must be done after org loads)
  (setq org-archive-location
        (concat org-dir "/archives/archive.org::datetree/"))

  (cj/org-general-settings)
  (cj/org-appearance-settings)
  (cj/org-todo-settings))

;; ------------------------------- Org Superstar -------------------------------

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets)
  (setq org-superstar-leading-bullet ?\s))

;; -------------------------------- Org-Appear ---------------------------------

(use-package org-appear
  ;; Default: OFF (toggle with cj/org-appear-toggle)
  ;; Useful for editing links, but can make tables hard to read when links expand
  :custom
  (org-appear-autoemphasis t)   ;; Show * / _ when cursor is on them
  (org-appear-autolinks t)      ;; Also works for links
  (org-appear-autosubmarkers t)) ;; And sub/superscripts

(defun cj/org-appear-toggle ()
  "Toggle org-appear-mode in the current org-mode buffer.
When enabled, org-appear shows emphasis markers and link URLs only when
point is on them. When disabled, they stay hidden (cleaner for reading,
especially in tables with long URLs)."
  (interactive)
  (if (bound-and-true-p org-appear-mode)
      (progn
        (org-appear-mode -1)
        (message "org-appear disabled (links/emphasis stay hidden)"))
    (org-appear-mode 1)
    (message "org-appear enabled (links/emphasis show when editing)")))

;; --------------------------------- Org-Tidy ----------------------------------

;; Hide :PROPERTIES: drawers behind a small inline marker so headings stay
;; clean.  Drawers remain editable -- cycle through them with TAB or toggle
;; the package off with M-x org-tidy-mode.
(use-package org-tidy
  :hook (org-mode . org-tidy-mode)
  :custom
  (org-tidy-properties-style 'inline))  ;; 'inline | 'fringe | 'invisible

;; ------------------------------- Org-Checklist -------------------------------

;; needed for org-habits to reset checklists once task is complete
;; this was a part of org-contrib which was deprecated
(use-package org-checklist
  :ensure nil ;; in custom folder
  :load-path "custom"
  :after org)

;; -------------------------- Org Link To Current File -------------------------

(defun cj/org-link-to-current-buffer-file ()
  "Create an Org mode link to the current file and copy it to the clipboard.
The link is formatted as [[file:<file-path>][<file-name>]], where <file-path>
is the full path to the current file and <file-name> is the name of the current
 file without any directory information. If the current buffer is not associated
with a file, the function will throw an error."
  (interactive)
  (if (buffer-file-name)
      (let* ((filename (buffer-file-name))
             (description (file-name-nondirectory filename))
             (link (format "[[file:%s][%s]]" filename description)))
        (kill-new link)
        (message "Copied Org link to current file to clipboard: %s" link))
    (user-error "Buffer isn't associated with a file, so no link sent to clipboard")))

;; ----------------------- Org Element Cache Management ------------------------

(defun cj/org-clear-element-cache ()
  "Clear the org-element cache for the current buffer or all buffers.
By default, clear cache for all org buffers. With prefix argument, clear only
the current buffer's cache. Useful when encountering parsing errors like
'wrong-type-argument stringp nil' during agenda generation."
  (interactive)
  (if current-prefix-arg
      (if (derived-mode-p 'org-mode)
        (progn
          (org-element-cache-reset)
          (message "Cleared org-element cache for current buffer"))
        (user-error "Current buffer is not in org-mode"))
    (org-element-cache-reset 'all)
    (message "Cleared org-element cache for all buffers")))

(keymap-set cj/org-map "C" #'cj/org-clear-element-cache)

;; ----------------------- Org Multi-Level Sorting -----------------------------

(defun cj/org-sort-by-todo-and-priority ()
  "Sort org entries by TODO status (TODO before DONE) and priority (A to D).
Sorts the current level's entries. Within each TODO state group, entries are
sorted by priority. Uses stable sorting: sort by priority first, then by TODO
status to preserve priority ordering within TODO groups."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  (save-excursion
    ;; First sort by priority (A, B, C, D, then no priority)
    ;; Ignore "Nothing to sort" errors for empty sections
    (condition-case nil
        (org-sort-entries nil ?p)
      (user-error nil))
    ;; Then sort by TODO status (TODO before DONE)
    ;; This preserves the priority ordering within each TODO group
    (condition-case nil
        (org-sort-entries nil ?o)
      (user-error nil)))
  (message "Sorted entries by TODO status and priority"))

;; --------------------- Finalize Task (done + journal + log) ------------------
;; `cj/org-finalize-task' (C-; O d) marks the task at point done with a chosen
;; finalized keyword -- which fires the org-roam journal-copy hook -- then
;; reshapes the heading per todo-format: a dated log entry for sub-tasks (and
;; VERIFY at any depth), or a kept keyword plus a date-only CLOSED line for
;; top-level tasks.

(defun cj/--org-finalize-dated-p (level keyword)
  "Return non-nil when a finalized heading should become a dated log entry.
Per todo-format: sub-tasks at LEVEL 3 or deeper flip to a dated entry, and a
VERIFY (by KEYWORD) flips at any depth.  Top-level tasks stay task-shaped."
  (or (>= level 3)
      (equal keyword "VERIFY")))

(defun cj/--org-finalize-rewrite-dated (&optional time)
  "Rewrite the heading at point as a dated log entry.
Strip the todo keyword and [#X] priority cookie, prepend a sortable timestamp
built from TIME (default now), and keep the tags."
  (org-back-to-heading t)
  (let ((stamp (format-time-string "%Y-%m-%d %a @ %H:%M:%S %z" time))
        (org-inhibit-logging t))
    (when (org-get-todo-state) (org-todo 'none))
    (when (nth 3 (org-heading-components))   ; only if a [#X] cookie is present
      (org-priority 'remove))
    (org-edit-headline (concat stamp " " (org-get-heading t t t t)))))

(defun cj/--org-finalize-close-in-place (&optional time)
  "Add a date-only CLOSED line under the heading at point, keeping the keyword.
TIME defaults to now.  Used for top-level tasks that stay task-shaped."
  (org-back-to-heading t)
  (let ((org-inhibit-logging t))
    (org-add-planning-info 'closed (or time (current-time))))
  ;; org's CLOSED stamp may carry HH:MM; normalize to date-only per todo-format.
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point)))
          (stamp (format-time-string "[%Y-%m-%d %a]" time)))
      (when (re-search-forward "CLOSED: \\[[^]]*\\]" end t)
        (replace-match (concat "CLOSED: " stamp) t t)))))

(defun cj/org-finalize-task (&optional state time)
  "Finalize the task at point: mark it done and reshape it per todo-format.
Prompt for a finalized keyword from `org-done-keywords' (STATE skips the
prompt, TIME sets the timestamp -- both for testing).  Marking the task done
fires the journal-copy hook, then a sub-task (level >= 3, or a VERIFY at any
depth) becomes a dated log entry while a top-level task keeps its keyword and
gains a date-only CLOSED line."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only available in Org buffers"))
  (org-back-to-heading t)
  (let ((keyword (org-get-todo-state))
        (level (org-current-level)))
    (unless (member keyword org-not-done-keywords)
      (user-error "Not on an open task (no actionable TODO keyword)"))
    (let ((finalized
           (or state
               (let ((default (if (member "DONE" org-done-keywords)
                                  "DONE"
                                (car org-done-keywords))))
                 (completing-read "Finalize as: " org-done-keywords
                                  nil t nil nil default)))))
      (let ((org-inhibit-logging t))
        (org-todo finalized))           ; fires the journal-copy hook
      (if (cj/--org-finalize-dated-p level keyword)
          (cj/--org-finalize-rewrite-dated time)
        (cj/--org-finalize-close-in-place time)))))

(keymap-set cj/org-map "d" #'cj/org-finalize-task)

;; ------------------------------ Org Keybindings ------------------------------

;; which-key labels for org keymaps
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    ;; org general operations
    "C-; O" "org menu"
    "C-; O C" "clear element cache"
    ;; org table operations (live directly under the org menu)
    "C-; O r" "table row"
    "C-; O r i" "insert row"
    "C-; O r d" "delete row"
    "C-; O c" "table column"
    "C-; O c i" "insert column"
    "C-; O c d" "delete column"
    ;; org narrowing (flat under the org menu; lowercase narrows, capital widens)
    "C-; O n" "narrow to subtree"
    "C-; O N" "widen"
    "C-; O >" "narrow forward sibling"
    "C-; O <" "narrow backward sibling"
    ;; org sparse tree (flat; lowercase creates, capital of same letter cancels)
    "C-; O s" "match sparse tree"
    "C-; O S" "show all (cancel)"
    "C-; O t" "show all TODOs"
    "C-; O T" "show all (cancel)"
    "C-; O R" "reveal context"
    ;; org global bindings
    "C-c a" "org agenda"
    "C-c c" "org capture"
    "C-c l" "org store link"))

(provide 'org-config)
;;; org-config.el ends here
