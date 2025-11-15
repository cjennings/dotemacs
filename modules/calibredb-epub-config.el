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

;; Declare functions from lazy-loaded packages
(declare-function calibredb-find-create-search-buffer "calibredb" ())
(declare-function calibredb-search-keyword-filter "calibredb" (keyword))
(declare-function cj/open-file-with-command "system-utils" (command))
(declare-function visual-fill-column-mode "visual-fill-column" (&optional arg))
(declare-function visual-fill-column--adjust-window "visual-fill-column" ())

;; -------------------------- CalibreDB Ebook Manager --------------------------

(use-package calibredb
  :defer 1
  :commands calibredb
  :bind
  ("M-B" . calibredb)
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
  (setq calibredb-search-page-max-rows 20000)

  ;; search window display
  (setq calibredb-size-show nil)
  (setq calibredb-order "asc")
  (setq calibredb-id-width 7))

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

(defvar cj/nov-margin-percent 25
  "Percentage of window width to use as margins on each side when reading epubs.
For example, 25 means 25% left margin + 25% right margin, with 50% for text.")

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
  (when (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t)
    ;; Calculate text width based on configurable margin percentage
    (let ((window (get-buffer-window (current-buffer)))
          (text-width-ratio (- 1.0 (* 2 (/ cj/nov-margin-percent 100.0)))))
      (if window
          (setq-local visual-fill-column-width
                      (floor (* text-width-ratio (window-body-width window))))
        ;; Fallback if no window yet
        (setq-local visual-fill-column-width 80)))
    (visual-fill-column-mode 1))
  (nov-render-document)
  ;; Force visual-fill-column to recalculate after rendering
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column--adjust-window)))

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
		;; open current EPUB with zathura (same key in pdf-view)
		("z" . (lambda () (interactive) (cj/open-file-with-command "zathura")))
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
