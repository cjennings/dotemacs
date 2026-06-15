;;; org-config --- Settings and Enhancements to Org Mode -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: C/D/P.
;; Load shape: eager.
;; Eager reason: core Org behavior and org-protocol setup; a daily-driver hot path.
;; Top-level side effects: org-protocol setup, a cj/custom-keymap binding,
;;   package configuration via use-package.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
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
  "Set org-mode appearance options (org faces are left to the theme)."
  (interactive)

  (setq org-ellipsis " ▾")                                  ;; change ellipses to down arrow
  (setq org-hide-emphasis-markers t)                        ;; hide emphasis markers (org-appear shows them when editing)
  (setq org-hide-leading-stars t)                           ;; hide leading stars, just show one per line
  (setq org-pretty-entities t)                              ;; render special symbols
  (setq org-pretty-entities-include-sub-superscripts nil)   ;; ...except superscripts and subscripts
  (setq org-fontify-emphasized-text t)                      ;; render bold and italic markup
  (setq org-fontify-whole-heading-line t)                   ;; fontify the whole line for headings (for face-backgrounds)
  (add-hook 'org-mode-hook 'prettify-symbols-mode))

;; ----------------------- Right-Aligned Org Tags ------------------------------
;; org-tags-column only right-aligns tags to a fixed column by baking literal
;; spaces into the file, so it can't track window width.  Instead, set the
;; column to 0 (org keeps a single space, no padding) and stretch that space
;; with a display property pinned to the window's right edge.  `:align-to' is
;; resolved at redisplay, so the tags follow window width and splits live and
;; nothing alignment-specific is written to disk.  Agenda has a native
;; equivalent (`org-agenda-tags-column' 'auto').

(setq org-tags-column 0)
(setq org-agenda-tags-column 'auto)

(defconst cj/org-tag-line-re
  "^\\*+ .*?\\([ \t]\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
  "Match an org headline ending in tags.
Group 1 is the single gap character before the tags; group 2 is the tag
string itself.")

(defconst cj/org-tag-right-margin 9
  "Columns of gap left between right-aligned tags and the window's right edge.
At least 1 keeps a glyph out of the final column, which would wrap a
non-truncated line.  The remaining columns reserve room for the glyphs org
appends AFTER the tags on a folded, property-drawer heading: the org-tidy
inline symbol (`org-tidy-properties-inline-symbol', \" ·\") and the fold
ellipsis (`org-ellipsis', \" ▾\").  Their nominal width is 4 columns, but the
fallback font renders the triangle wider than its reported width and the
`:align-to' stretch rounds, so the true overflow exceeds the nominal count.
The value is set from rendered measurement (see the headline-wrap harness),
not arithmetic -- that mismatch is what made the earlier 5-column reserve
wrap.  Verify changes with a screenshot, never column math alone.")

(defun cj/org--tag-align-spec (tag-string)
  "Return a display spec that right-aligns TAG-STRING to the window edge.
The spec stretches a space so the tag ends `cj/org-tag-right-margin' columns
short of the window's right edge, leaving the tag flush right without wrapping
the line."
  `(space :align-to (- right ,(+ (string-width tag-string) cj/org-tag-right-margin))))

(defconst cj/org-right-align-tags-keyword
  `((,cj/org-tag-line-re
     (1 (progn
          (put-text-property
           (match-beginning 1) (match-end 1)
           'display (cj/org--tag-align-spec (match-string 2)))
          nil)
        t)))
  "Font-lock keyword right-aligning org headline tags via a display property.
The stretched space before the tag string is pinned to the window's right
edge, less the tag width.")

(defun cj/org--manage-tag-display-prop ()
  "Let font-lock clear the right-align display property on refontify."
  (add-to-list 'font-lock-extra-managed-props 'display))

(add-hook 'org-mode-hook #'cj/org--manage-tag-display-prop)
(font-lock-add-keywords 'org-mode cj/org-right-align-tags-keyword t)

;; ------------------------ Org Table Header Highlighting --------------------
;; Org faces the whole table -- header rows included -- with `org-table'; it has
;; no in-buffer header-row face.  `org-table-header' is used only by the sticky
;; header line of `org-table-header-line-mode'.  This font-lock keyword prepends
;; `org-table-header' onto a table's header rows (the non-hline rows above its
;; first hline), so the themed header style lands in place in the buffer.

(declare-function org-at-table-p "org")
(declare-function org-at-table-hline-p "org")
(declare-function org-table-begin "org-table")
(declare-function org-table-end "org-table")

(defcustom cj/org-fontify-table-headers t
  "When non-nil, highlight org table header rows with the `org-table-header' face.
A header row is a non-hline table row above its table's first hline.  Org has no
in-buffer header-row face of its own, so this supplies one, deferring its whole
appearance to the themed `org-table-header' face."
  :type 'boolean
  :group 'org)

(defun cj/--org-table-first-hline-position ()
  "Return the start position of the first hline in the table at point, or nil.
Point must be inside an org table."
  (save-excursion
    (let ((end (org-table-end))
          (found nil))
      (goto-char (org-table-begin))
      (while (and (not found) (< (point) end))
        (when (org-at-table-hline-p)
          (setq found (line-beginning-position)))
        (forward-line 1))
      found)))

(defun cj/--org-table-header-row-p ()
  "Return non-nil if the line at point is a header row of its org table.
A header row is a non-hline table row positioned above the table's first hline.
A table with no hline has no header rows."
  (and (org-at-table-p)
       (not (org-at-table-hline-p))
       (let ((hline (cj/--org-table-first-hline-position)))
         (and hline (< (line-beginning-position) hline)))))

(defun cj/--org-fontify-table-header-matcher (limit)
  "Font-lock matcher for the next org table header row before LIMIT.
Returns non-nil when a header row is found, with match group 0 spanning the
whole row line."
  (let (beg end found)
    (while (and (not found)
                (re-search-forward "^[ \t]*|.*$" limit t))
      (setq beg (match-beginning 0)
            end (match-end 0))
      (save-excursion
        (goto-char beg)
        (when (cj/--org-table-header-row-p)
          (setq found t))))
    (when found
      (set-match-data (list beg end))
      t)))

(defconst cj/org-table-header-keyword
  '((cj/--org-fontify-table-header-matcher (0 'org-table-header prepend)))
  "Font-lock keyword prepending `org-table-header' onto org table header rows.")

(when cj/org-fontify-table-headers
  (font-lock-add-keywords 'org-mode cj/org-table-header-keyword t))

;; ----------------------------- Org TODO Settings ---------------------------

(defun cj/org-todo-settings ()
  "All org-todo related settings are grouped and set in this function."

  ;; logging task creation, task start, and task resolved states
  (setq org-todo-keywords '((sequence "TODO(t)" "PROJECT(p)" "DOING(i)"
                                      "WAITING(w)" "VERIFY(v)" "STALLED(s)"
                                      "DELEGATED(x)" "|"
                                      "FAILED(f!)" "DONE(d!)" "CANCELLED(c!)")))

  ;; Keyword and priority faces are defined and wired in org-faces-config.el
  ;; (loaded just after this module): each keyword and priority maps to its own
  ;; org-faces-* face, which the active theme recolors.
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?D)
  (setq org-default-priority ?D)

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
  (cj/register-prefix-map "O" cj/org-map)
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

  (defun cj/--org-follow-link-same-window ()
    "Follow the Org link at point, opening file links in the current window.
Org's default for file links is `find-file-other-window' (via
`org-link-frame-setup'); this overrides it so the file replaces the buffer
the link sits in.  Off a link this does nothing, so a stray click is a silent
no-op rather than a \"No link found\" error."
    (when (eq (org-element-type (org-element-context)) 'link)
      (let ((org-link-frame-setup (cons '(file . find-file) org-link-frame-setup)))
        (org-open-at-point))))

  (defun cj/org-follow-link-at-mouse-same-window (event)
    "Follow the Org link clicked in EVENT, opening file links in the same window.
Bound to S-mouse-1 and mouse-3 in `org-mouse-map' -- the keymap org attaches
to each link as a `keymap' text property.  That layer outranks both
`org-mode-map' and the `mouse-trap-mode' emulation keymap, so the gesture
lands even where mouse-trap otherwise disables clicks.  A shift-click or
right-click on a link opens it in place; org's other-window default
(mouse-2 / plain click) is left alone."
    (interactive "e")
    (mouse-set-point event)
    (cj/--org-follow-link-same-window))

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
  (cj/org-todo-settings)

  ;; Open a file link in the current window on shift-left-click or right-click.
  ;; These bind into `org-mouse-map' (the per-link `keymap' text property)
  ;; rather than `org-mode-map' so they outrank the `mouse-trap-mode' emulation
  ;; keymap, which otherwise swallows clicks in org buffers.  mouse-2 / plain
  ;; click keep org's other-window default.
  (keymap-set org-mouse-map "S-<mouse-1>" #'cj/org-follow-link-at-mouse-same-window)
  (keymap-set org-mouse-map "<mouse-3>" #'cj/org-follow-link-at-mouse-same-window))

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
  (org-tidy-properties-style 'inline)   ;; 'inline | 'fringe | 'invisible
  (org-tidy-properties-inline-symbol "·"))  ;; middle dot, subtler than the default sharp

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
                 (completing-read "Finalize task as: " org-done-keywords
                                  nil t nil nil default)))))
      (let ((org-inhibit-logging t))
        (org-todo finalized))           ; fires the journal-copy hook
      (if (cj/--org-finalize-dated-p level keyword)
          (cj/--org-finalize-rewrite-dated time)
        (cj/--org-finalize-close-in-place time)))))

(keymap-set cj/org-map "d" #'cj/org-finalize-task)

;; which-key labels for the C-; O org prefix.  Registered lazily so this
;; module's load doesn't depend on which-key being present.
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements cj/org-map
    "n" "narrow to subtree"
    "N" "widen"
    ">" "narrow forward"
    "<" "narrow backward"
    "s" "sparse tree (match)"
    "S" "cancel sparse tree"
    "t" "todo tree"
    "T" "cancel todo tree"
    "R" "reveal"
    "C" "clear element cache"
    "d" "finalize task (dated)"
    "r" "table row"
    "r i" "insert row"
    "r d" "kill row"
    "c" "table column"
    "c i" "insert column"
    "c d" "delete column"))

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
