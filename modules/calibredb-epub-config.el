;;; calibredb-epub-config --- Functionality for Ebook Management and Display -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

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

;; Declare functions from lazy-loaded packages
(declare-function calibredb-find-create-search-buffer "calibredb" ())
(declare-function calibredb-search-keyword-filter "calibredb" (keyword))
(declare-function cj/open-file-with-command "system-utils" (command))
(declare-function visual-fill-column-mode "visual-fill-column" (&optional arg))
(declare-function visual-fill-column--adjust-window "visual-fill-column" ())
(declare-function nov-render-document "nov" ())

;; -------------------------- CalibreDB Ebook Manager --------------------------

(use-package calibredb
  :commands calibredb
  :bind
  ("M-S-b" . calibredb)  ;; was M-B, overrides backward-word
  ;; use built-in filter by tag, add clear-filters
  (:map calibredb-search-mode-map
		("l" . calibredb-filter-by-tag)
		("L" . cj/calibredb-clear-filters))
  :config
  ;; basic config
  (setq calibredb-root-dir books-dir)
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-program "/usr/bin/calibredb")
  (setq calibredb-preferred-format "epub")
  (setq calibredb-search-page-max-rows 500)

  ;; search window display
  (setq calibredb-size-show nil)
  (setq calibredb-order "asc")
  (setq calibredb-id-width 7)
  (setq calibredb-favorite-icon "🔖")
  (setq calibredb-favorite-keyword "in-progress"))

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

;; Visual-fill-column provides centered text with margins
(use-package visual-fill-column
  :defer t
  :config
  (setq-default visual-fill-column-center-text t))

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

(defun cj/nov--text-width-for-window (&optional window)
  "Return preferred Nov text width for WINDOW.
Computed from the window's natural column count -- its current body width
plus any margins already set -- so that re-running `cj/nov-update-layout' is
idempotent: each pass would otherwise shave the column by another margin
fraction, since setting margins narrows the body width."
  (let* ((window (or window (get-buffer-window (current-buffer) t)))
         (margins (and window (window-margins window)))
         (natural-cols (if window
                           (+ (window-body-width window)
                              (or (car margins) 0)
                              (or (cdr margins) 0))
                         80)))
    (cj/nov--text-width natural-cols)))

(defun cj/nov-update-layout (&optional _frame)
  "Recalculate Nov's centered text column (width + margins) for this buffer.
Also runs from `window-configuration-change-hook' and
`window-size-change-functions' to stay responsive to splits and resizes."
  (interactive)
  (when (derived-mode-p 'nov-mode)
    (when (require 'visual-fill-column nil t)
      (setq-local visual-fill-column-center-text t)
      (setq-local visual-fill-column-width (cj/nov--text-width-for-window))
      (visual-fill-column-mode 1)
      (when (bound-and-true-p visual-fill-column-mode)
        (visual-fill-column--adjust-window)))))

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
  ;; Use Merriweather for comfortable reading with appropriate scaling
  ;; Darker sepia color (#E8DCC0) is easier on the eyes than pure white
  (face-remap-add-relative 'variable-pitch :family "Merriweather" :height 1.0 :foreground "#E8DCC0")
  (face-remap-add-relative 'default :family "Merriweather" :height 180 :foreground "#E8DCC0")
  (face-remap-add-relative 'fixed-pitch :height 180 :foreground "#E8DCC0")
  ;; Make this buffer-local so other Nov buffers can choose differently
  ;; Setting to t makes nov respect visual-fill-column margins
  (setq-local nov-text-width t)
  ;; Enable visual-line-mode for proper text wrapping
  (visual-line-mode 1)
  ;; Set fill-column as a fallback
  (setq-local fill-column 100)
  ;; Enable visual-fill-column for centered text with margins
  (cj/nov-update-layout)
  ;; Keep centered text width responsive after splits/resizes.
  (add-hook 'window-configuration-change-hook #'cj/nov-update-layout nil t)
  ;; Re-render so the first page is laid out inside the margins just set;
  ;; nov-mode's initial render ran before `nov-text-width'/visual-fill-column.
  (nov-render-document))

(defun cj/nov-open-external ()
  "Open the current EPUB with zathura."
  (interactive)
  (cj/open-file-with-command "zathura"))

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
						  ;; Convert pixel image width to columns for alignment.
						  (img-cols (max 1 (ceiling (/ (float img-px)
													   (max 1 (window-font-width win))))))
						  (pad-cols (max 0 (/ (- col-width img-cols) 2)))
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
  "Return the current EPUB file path when in nov-mode, or nil."
  (when (derived-mode-p 'nov-mode)
	;; In nov, the buffer visits the .epub; buffer-file-name is usually the EPUB.
	(or buffer-file-name
		(and (boundp 'nov-epub-filename) nov-epub-filename)
		(and (boundp 'nov-epub-file) nov-epub-file))))

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

(provide 'calibredb-epub-config)
;;; calibredb-epub-config.el ends here
