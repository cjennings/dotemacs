;;; calibredb-epub-config --- Functionality for Ebook Management and Display -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; optional ebook workflow, a command-loaded deferral
;;   candidate for Phase 4.
;; Top-level side effects: one add-hook, one advice-add, package config.
;; Runtime requires: user-constants, subr-x.
;; Direct test load: yes.
;;
;; This module provides a comprehensive ebook management and reading experience
;; within Emacs, integrating CalibreDB for library management and Nov for EPUB
;; reading.
;;
;; FEATURES:
;; - CalibreDB integration for managing your Calibre ebook library
;; - Nov mode for reading EPUB files with customized typography and layout
;; - Seamless navigation between Nov reading buffers and CalibreDB entries
;; - Image centering in EPUB documents without modifying buffer text
;; - Quick filtering and searching within your ebook library
;;
;; KEY BINDINGS:
;; - M-B: Open CalibreDB library browser
;; - In CalibreDB search mode:
;;   - l: Filter by tag
;;   - L: Clear all filters
;; - In Nov mode:
;;   - z: Open current EPUB in external viewer (zathura)
;;   - C-c C-b: Jump to CalibreDB entry for current book
;;   - m: Set bookmark
;;   - b: List bookmarks
;;
;; WORKFLOW:
;; 1. Press M-B to browse your Calibre library
;; 2. Use filters (l for tags, L to clear) to narrow results
;; 3. Open an EPUB to read it in Nov with optimized typography
;; 4. While reading, use C-c C-b to jump back to the book's metadata
;; 5. Use z to open in external reader when needed
;;
;; CONFIGURATION NOTES:
;; - Prefers EPUB format when available, falls back to PDF
;; - Centers images in EPUB documents using display properties
;; - Applies custom typography with larger fonts for comfortable reading
;; - Uses visual-fill-column for centered text with appropriate margins

;;; Code:

(require 'user-constants)  ;; for books-dir
(require 'subr-x)
(require 'transient)       ;; cj/calibredb-menu is a transient prefix

;; Declare functions from lazy-loaded packages
(declare-function calibredb-find-create-search-buffer "calibredb" ())
(declare-function calibredb-search-keyword-filter "calibredb" (keyword))
(declare-function cj/open-file-with-command "system-utils" (command))
(declare-function nov-render-document "nov" ())
(defvar nov-text-width)                 ; from nov.el; set buffer-local here

;; calibredb commands the curated menu drives (all autoloaded by calibredb)
(declare-function calibredb-switch-library "calibredb" ())
(declare-function calibredb-filter-by-book-format "calibredb" ())
(declare-function calibredb-filter-by-author-sort "calibredb" ())
(declare-function calibredb-search-clear-filter "calibredb" ())
(declare-function calibredb-sort-by-author "calibredb" ())
(declare-function calibredb-sort-by-title "calibredb" ())
(declare-function calibredb-sort-by-pubdate "calibredb" ())
(declare-function calibredb-sort-by-format "calibredb" ())
(declare-function calibredb-find-file "calibredb" ())
(declare-function calibredb-dispatch "calibredb" ())
(declare-function calibredb-show-entry "calibredb" (entry &optional switch))
(declare-function calibredb-find-candidate-at-point "calibredb" ())
(declare-function calibredb-search-refresh-or-resume "calibredb" (&optional begin position))
(defvar calibredb-show-entry-switch)    ; from calibredb-show.el
(defvar calibredb-sort-by)              ; from calibredb-core.el
(defvar calibredb-search-filter)        ; from calibredb-search.el
;; calibredb filter-state vars (set by cj/calibredb-clear-filters and friends)
(defvar calibredb-tag-filter-p)         ; from calibredb-search.el
(defvar calibredb-favorite-filter-p)    ; from calibredb-search.el
(defvar calibredb-author-filter-p)      ; from calibredb-search.el
(defvar calibredb-date-filter-p)        ; from calibredb-search.el
(defvar calibredb-format-filter-p)      ; from calibredb-search.el
(defvar calibredb-search-current-page)  ; from calibredb-search.el

;; -------------------------- CalibreDB Ebook Manager --------------------------

(defun cj/calibredb-clear-filters ()
  "Clear active filters and show all results."
  (interactive)
  (setq calibredb-tag-filter-p nil
		calibredb-favorite-filter-p nil
		calibredb-author-filter-p nil
		calibredb-date-filter-p nil
		calibredb-format-filter-p nil
		calibredb-search-current-page 1)
  ;; empty string resets keyword filter and refreshes listing
  (calibredb-search-keyword-filter ""))

(defun cj/calibredb-describe-at-point ()
  "Show the book at point in the docked *calibredb-entry* buffer.
Displays the entry without switching focus back to the list, so it lands
in the bottom-docked window (see the `display-buffer-alist' entry below)
and q (`calibredb-entry-quit') dismisses it."
  (interactive)
  (calibredb-show-entry (car (calibredb-find-candidate-at-point))))

(defun cj/--calibredb-sort-preserving-filter (field)
  "Set `calibredb-sort-by' to FIELD and refresh, keeping the active filter.
calibredb's own `calibredb-sort-by-*' commands refresh with
`calibredb-search-refresh-and-clear-filter', which drops the active filter
on every sort.  This refreshes with `calibredb-search-refresh-or-resume',
which re-applies `calibredb-search-filter' instead."
  (setq calibredb-sort-by field)
  (calibredb-search-refresh-or-resume))

(use-package calibredb
  :commands calibredb
  :bind
  ("M-S-b" . calibredb)  ;; was M-B, overrides backward-word
  ;; use built-in filter by tag, add clear-filters
  (:map calibredb-search-mode-map
		("l" . calibredb-filter-by-tag)
		("L" . cj/calibredb-clear-filters)
		;; "?" -> curated menu of frequent workflows; "H" -> the full dispatch
		("?" . cj/calibredb-menu)
		("H" . calibredb-dispatch))
  :config
  ;; basic config
  (setq calibredb-root-dir books-dir)
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-program "/usr/bin/calibredb")
  (setq calibredb-preferred-format "epub")
  (setq calibredb-search-page-max-rows 500)
  ;; Dock the book-detail buffer to the bottom 30%; q dismisses it.
  ;; `pop-to-buffer' honours `display-buffer-alist' (the default
  ;; `switch-to-buffer-other-window' would not).
  (setq calibredb-show-entry-switch #'pop-to-buffer)
  (add-to-list 'display-buffer-alist
			   '("\\`\\*calibredb-entry\\*\\'"
				 (display-buffer-at-bottom)
				 (window-height . 0.3)))
  ;; A curated menu of the frequent calibredb workflows, bound to `?' in the
  ;; search buffer; calibredb's own full dispatch (the wall of every command)
  ;; moves to `H'.  Defined here in `:config' so it only builds once calibredb
  ;; (and its matching transient) is loaded.  This is the "? brings up a
  ;; discoverable help menu" convention.
  (transient-define-prefix cj/calibredb-menu ()
	"Frequent calibredb workflows."
	[["Library"
	  ("l" "switch library"      calibredb-switch-library)]
	 ["Filter"
	  ("f" "format"              calibredb-filter-by-book-format)
	  ("a" "author"              calibredb-filter-by-author-sort)
	  ("x" "reset filter"        calibredb-search-clear-filter)]
	 ["Sort"
	  ("A" "author (last name)"  calibredb-sort-by-author)
	  ("t" "title"               calibredb-sort-by-title)
	  ("p" "pubdate"             calibredb-sort-by-pubdate)
	  ("g" "group by format"     calibredb-sort-by-format)]
	 ["Book"
	  ("o" "open"                calibredb-find-file)
	  ("d" "describe"            cj/calibredb-describe-at-point)
	  ("H" "full calibredb menu" calibredb-dispatch)]]
	[("q" "quit" transient-quit-one)])

  ;; Keep the active filter when sorting.  calibredb's macro-generated
  ;; `calibredb-sort-by-*' commands refresh-and-clear-filter, dropping the
  ;; filter on every sort; override each to refresh-or-resume so the filter
  ;; survives.  Named advice keeps the override idempotent across reloads.
  (dolist (field '(id title author format date pubdate tag size language))
	(let ((cmd (intern (format "calibredb-sort-by-%s" field)))
		  (adv (intern (format "cj/--calibredb-sort-keep-filter-%s" field)))
		  (f field))
	  (defalias adv
		(lambda (&rest _) (interactive) (cj/--calibredb-sort-preserving-filter f))
		(format "Sort by %s, keeping the active filter (override)." field))
	  (advice-add cmd :override adv)))

  ;; search window display
  (setq calibredb-size-show nil)
  (setq calibredb-order "asc")
  (setq calibredb-id-width 7)
  (setq calibredb-favorite-icon "🔖")
  (setq calibredb-favorite-keyword "in-progress"))

;; ------------------------------ Nov Epub Reader ------------------------------

(defvar cj/nov-margin-percent 10
  "Percent of the window's natural width used as a margin on each side in epubs.
10 leaves 80% of the columns for text.  Clamped to 0..25, so the text column
runs from 50% (margin 25) to 100% (margin 0) of the window.
Adjust it live with `cj/nov-widen-text' and `cj/nov-narrow-text'.")

(defvar cj/nov-min-text-width 40
  "Minimum text width in columns for Nov reading buffers.")

(defvar cj/nov-margin-step 2
  "Percentage points each `cj/nov-widen-text'/`cj/nov-narrow-text' press changes.")

;; Prevent magic-fallback-mode-alist from opening epub as archive-mode
;; Advise set-auto-mode to force nov-mode for .epub files before magic-fallback runs
(defun cj/force-nov-mode-for-epub (orig-fun &rest args)
  "Force nov-mode for .epub files, bypassing archive-mode detection."
  (if (and buffer-file-name
           (string-match-p "\\.epub\\'" buffer-file-name))
      (progn
        ;; Load nov if not already loaded
        (unless (featurep 'nov)
          (require 'nov nil t))
        ;; Call nov-mode if available, otherwise fallback to default behavior
        (if (fboundp 'nov-mode)
            (nov-mode)
          (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'set-auto-mode :around #'cj/force-nov-mode-for-epub)

;; Define helper functions before use-package so they're available for hooks
(defun cj/forward-paragraph-and-center ()
  "Forward one paragraph and center the page."
  (interactive)
  (forward-paragraph)
  (recenter))

(defun cj/nov--text-width (total-cols)
  "Return the Nov text-column width for TOTAL-COLS of usable window width.
`cj/nov-margin-percent' is clamped to 0..25 and taken off each side; the
result is at least `cj/nov-min-text-width'."
  (let* ((margin-percent (max 0 (min 25 cj/nov-margin-percent)))
         (text-width-ratio (- 1.0 (* 2 (/ margin-percent 100.0)))))
    (max cj/nov-min-text-width
         (floor (* text-width-ratio total-cols)))))

(defun cj/nov--natural-window-width (&optional window)
  "Return WINDOW's column count, adding back any display margins already set.
WINDOW defaults to the window showing the current buffer on any frame; 80 if
there is none.  Adding the margins back makes the value stable under repeated
layout passes -- each pass narrows the body width but not the natural width."
  (if-let ((win (or window (get-buffer-window (current-buffer) t))))
      (let ((m (window-margins win)))
        (+ (window-body-width win) (or (car m) 0) (or (cdr m) 0)))
    80))

(defun cj/nov--text-width-for-window (&optional window)
  "Return the preferred EPUB text column count for WINDOW."
  (cj/nov--text-width (cj/nov--natural-window-width window)))

(defun cj/nov--rerender-preserving-position ()
  "Re-render the nov document, restoring point's relative position.
Capture point as a fraction of the buffer, re-render, then move point to the
same fraction of the re-rendered buffer so the reading position is kept
approximately."
  (let ((frac (when (> (point-max) (point-min))
                (/ (float (- (point) (point-min)))
                   (- (point-max) (point-min))))))
    (nov-render-document)
    (when frac
      (goto-char (+ (point-min)
                    (round (* frac (- (point-max) (point-min)))))))))

(defun cj/nov--center-in-window (win total width)
  "Center a WIDTH-column text block in WIN, given its TOTAL natural width.
Set equal left/right display margins and push the fringes to the window edge."
  ;; floor: never let the margins squeeze the text area below WIDTH.
  (let ((margin (max 0 (/ (- total width) 2))))
    (set-window-margins win margin margin))
  ;; Push the fringes out to the window's edge; otherwise they sit between the
  ;; margin and the text and show as thin vertical lines beside it.
  (set-window-fringes win nil nil t))

(defun cj/nov-update-layout (&optional _frame)
  "Size the EPUB text column for this buffer and center it in its window.
`nov-text-width' is set so nov's `shr' fills the text to roughly 80% of the
window, and the window's display margins are set to center that block (no
`visual-fill-column' -- its margin-setting wasn't taking effect in nov-mode).
When the width changes the document is re-rendered, restoring the reading
position approximately.  Runs from `window-configuration-change-hook' and is a
command."
  (interactive)
  (when (derived-mode-p 'nov-mode)
    (let* ((win (get-buffer-window (current-buffer) t))
           (total (cj/nov--natural-window-width win))
           (width (cj/nov--text-width total)))
      (unless (eql nov-text-width width)
        (setq-local nov-text-width width)
        (cj/nov--rerender-preserving-position))
      (when win
        (cj/nov--center-in-window win total width)))))

(defun cj/--nov-adjust-margin (delta)
  "Add DELTA to `cj/nov-margin-percent' (clamped 0..25), re-lay-out, and report.
A positive DELTA narrows the text column; a negative DELTA widens it."
  (setq cj/nov-margin-percent
        (max 0 (min 25 (+ cj/nov-margin-percent delta))))
  (cj/nov-update-layout)
  (message "EPUB text width: %d%% of the window (margin %d%% each side)"
           (- 100 (* 2 cj/nov-margin-percent)) cj/nov-margin-percent))

(defun cj/nov-widen-text ()
  "Give the EPUB text column more of the window, up to the full width."
  (interactive)
  (cj/--nov-adjust-margin (- cj/nov-margin-step)))

(defun cj/nov-narrow-text ()
  "Give the EPUB text column less of the window, down to 50%."
  (interactive)
  (cj/--nov-adjust-margin cj/nov-margin-step))

(defun cj/nov-apply-preferences ()
  "Apply preferences after nov-mode has launched."
  (interactive)
  ;; Use Merriweather for comfortable reading with appropriate scaling.
  ;; (Reading fg color stripped; falls back to the theme default until a
  ;; themeable reading face exists -- see todo.org.)
  (face-remap-add-relative 'variable-pitch :family "Merriweather" :height 1.0)
  (face-remap-add-relative 'default :family "Merriweather" :height 180)
  (face-remap-add-relative 'fixed-pitch :height 180)
  ;; Enable visual-line-mode for proper text wrapping
  (visual-line-mode 1)
  ;; Set fill-column as a fallback
  (setq-local fill-column 100)
  ;; Have nov's `shr' fill the text to a column count, and `cj/nov-update-layout'
  ;; center it with the window's display margins.  This deliberately does NOT
  ;; use `visual-fill-column' -- its margin-setting wasn't taking effect here.
  (setq-local nov-text-width (cj/nov--text-width-for-window))
  (cj/nov-update-layout)
  ;; Keep the width and centering responsive to splits/resizes.
  (add-hook 'window-configuration-change-hook #'cj/nov-update-layout nil t)
  ;; Drop the centering margins from the window when this EPUB buffer goes away,
  ;; so a later buffer in the same window isn't left indented.
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when-let ((w (get-buffer-window (current-buffer))))
                (set-window-margins w nil)
                (set-window-fringes w nil)))
            nil t)
  ;; Render once now; the window may not exist yet (so this uses the fallback
  ;; width), and `cj/nov-update-layout' on the first window-config change
  ;; re-flows at the real width.
  (nov-render-document))

(defun cj/nov-open-external ()
  "Open the current EPUB with zathura."
  (interactive)
  (cj/open-file-with-command "zathura"))

;; Jump from a Nov buffer to the corresponding CalibreDB entry.
(defun cj/nov--metadata-get (key)
  "Return a metadata value from nov-metadata trying KEY as symbol and string."
  (let* ((v (or (and (boundp 'nov-metadata)
					 (or (alist-get key nov-metadata nil nil #'equal)
						 (alist-get (if (symbolp key) (symbol-name key) key)
									nov-metadata nil nil #'equal)))
				nil)))
	(cond
	 ((and (listp v) (= (length v) 1)) (car v))
	 ((stringp v) v)
	 (t v))))

(defun cj/nov--file-path ()
  "Return the current EPUB file path when in nov-mode, or nil.
Falls back to nov's own `nov-file-name' (set by `nov-mode' from the visited
file) so the function still resolves when `buffer-file-name' has been cleared."
  (when (derived-mode-p 'nov-mode)
	(or buffer-file-name
		(and (boundp 'nov-file-name) nov-file-name))))

(defun cj/nov-jump-to-calibredb ()
  "Open CalibreDB focused on the current EPUB's book entry.
Try to use the Calibre book id from the parent folder name (for example,
\"Title (123)\"). Fall back to a title or author search when no id exists."
  (interactive)
  (require 'calibredb)
  (let* ((file (cj/nov--file-path))
		 (title (or (cj/nov--metadata-get 'title)
					(cj/nov--metadata-get "title")))
		 (author (or (cj/nov--metadata-get 'creator)
					 (cj/nov--metadata-get 'author)
					 (cj/nov--metadata-get "creator")
					 (cj/nov--metadata-get "author")))
		 (id (when file
			   (let* ((parent (file-name-nondirectory
							   (directory-file-name (file-name-directory file)))))
				 (when (string-match " (\\([0-9]+\\))\\'" parent)
				   (match-string 1 parent))))))
	(calibredb)
	(with-current-buffer (calibredb-find-create-search-buffer)
	  (setq calibredb-search-current-page 1)
	  (cond
	   (id
		(calibredb-search-keyword-filter (format "id:%s" id))
		(message "CalibreDB: focused by id:%s" id))
	   ((or title author)
		(let* ((q (string-join
				   (delq nil (list (and title (format "title:\"%s\"" title))
								   (and author (format "authors:\"%s\"" author))))
				   " and ")))
		  (calibredb-search-keyword-filter q)
		  (message "CalibreDB: search %s" (if (string-empty-p q) "<all>" q))))
	   (t
		(calibredb-search-keyword-filter "")
		(message "CalibreDB: no metadata; showing all"))))))

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . cj/nov-apply-preferences)
  :bind
  (:map nov-mode-map
		("m" . bookmark-set)
		("b" . bookmark-bmenu-list)
		("r" . nov-render-document)
		("l" . recenter-top-bottom)
		("d" . sdcv-search-input)
		("." . cj/forward-paragraph-and-center)
		("<" . nov-history-back)
		(">" . nov-history-forward)
		("," . backward-paragraph)
		;; +/= widen the text column, -/_ narrow it (50%..100% of the window)
		("+" . cj/nov-widen-text)
		("=" . cj/nov-widen-text)
		("-" . cj/nov-narrow-text)
		("_" . cj/nov-narrow-text)
		;; open current EPUB with zathura (same key in pdf-view)
		("z" . cj/nov-open-external)
		("t" . nov-goto-toc)
		("C-c C-b" . cj/nov-jump-to-calibredb)))

;; ------------------------- Nov bookmark naming -------------------------------
;; In a nov buffer "m" is bound to `bookmark-set' (above).  nov's
;; `nov-bookmark-make-record' names the record after `(buffer-name)' -- the EPUB
;; filename, extension and all.  Rebuild it as "Author, Title" parsed from the
;; filename: under Calibre's "<Title> - <Author>.epub" naming the filename is
;; more complete than the EPUB's embedded metadata (which carries truncated
;; titles and author-sort "Last, First" forms).

(defun cj/--nov-clean-title (s)
  "Clean a title or author S parsed from an EPUB filename, or nil when blank.
Restores a colon where Calibre sanitized \":\" to \"_\" (\"Frege_ A Guide\"
-> \"Frege: A Guide\"), turns any leftover underscore into a space, and
collapses runs of whitespace."
  (when (stringp s)
    (let* ((colon (replace-regexp-in-string "_ " ": " s))
           (spaced (replace-regexp-in-string "_" " " colon))
           (out (string-trim (replace-regexp-in-string "[ \t]+" " " spaced))))
      (and (not (string-empty-p out)) out))))

(defun cj/--nov-bookmark-name-from-file (path)
  "Return \"Author, Title\" derived from an EPUB PATH's filename, or nil.
Splits the filename (sans extension) on its last \" - \" into title and
author per Calibre's \"<Title> - <Author>\" convention, restoring colons and
reordering to \"Author, Title\".  Falls back to the cleaned whole name when
there is no \" - \" separator."
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((base (file-name-sans-extension (file-name-nondirectory path))))
      (if (string-match "\\`\\(.+\\) - \\(.+\\)\\'" base)
          (let ((title (cj/--nov-clean-title (match-string 1 base)))
                (author (cj/--nov-clean-title (match-string 2 base))))
            (cond ((and author title) (format "%s, %s" author title))
                  (title title)
                  (author author)
                  (t nil)))
        (cj/--nov-clean-title base)))))

(defun cj/--nov-bookmark-rename-record (record)
  "Replace RECORD's bookmark name with \"Author, Title\" from its EPUB filename.
Advice (:filter-return) on `nov-bookmark-make-record'.  RECORD is
\(NAME . ALIST) carrying a `filename'; left unchanged when no name derives."
  (let ((name (cj/--nov-bookmark-name-from-file
               (alist-get 'filename (cdr record)))))
    (if name (cons name (cdr record)) record)))

(with-eval-after-load 'nov
  (advice-add 'nov-bookmark-make-record :filter-return
              #'cj/--nov-bookmark-rename-record))

(defun cj/--nov-image-padding-cols (col-width img-px font-width-px)
  "Return left-padding columns to center an IMG-PX-wide image in COL-WIDTH cols.
FONT-WIDTH-PX is the column width in pixels; clamped up to 1 so a zero or
negative value can't divide.  When the image is at least as wide as COL-WIDTH
the result is 0 -- no centering is possible."
  (let* ((fw (max 1 font-width-px))
         (img-cols (max 1 (ceiling (/ (float img-px) fw))))
         (pad (/ (- col-width img-cols) 2)))
    (max 0 pad)))

(defun cj/nov-center-images ()
  "Center images in the current Nov buffer without modifying text.

Use `line-prefix' and `wrap-prefix' with a space display property aligned to a
computed column based on the window text area width."
  (let ((inhibit-read-only t))
	;; Clear any prior centering prefixes first (fresh render usually makes this
	;; unnecessary, but it makes the function idempotent).
	(remove-text-properties (point-min) (point-max)
							'(line-prefix nil wrap-prefix nil))
	(save-excursion
	  (goto-char (point-min))
	  ;; Work in the selected window showing this buffer (if any).
	  (when-let* ((win (get-buffer-window (current-buffer) t))
				  (col-width (window-body-width win)) ;; columns
				  (col-px (* col-width (window-font-width win))))
		(while (let ((m (text-property-search-forward
						 'display nil
						 (lambda (_ p) (and (consp p) (eq (car-safe p) 'image))))))
				 (when m
				   (let* ((img (prop-match-value m))
						  (img-px (car (image-size img t)))   ;; pixel width
						  (pad-cols (cj/--nov-image-padding-cols
									 col-width img-px (window-font-width win)))
						  (prefix (propertize " " 'display `(space :align-to ,pad-cols))))
					 (save-excursion
					   (goto-char (prop-match-beginning m))
					   (beginning-of-line)
					   (let ((bol (point))
							 (eol (line-end-position)))
						 (add-text-properties bol eol
											  `(line-prefix ,prefix
															wrap-prefix ,prefix)))))
				   t)))))))

(add-hook 'nov-post-html-render-hook #'cj/nov-center-images)

(provide 'calibredb-epub-config)
;;; calibredb-epub-config.el ends here
