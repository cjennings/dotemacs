;;; org-noter-config.el --- Org-noter configuration -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Org-noter configuration for taking notes on PDF and EPUB documents.
;;
;; Workflow:
;; 1. Open a PDF (pdf-view-mode) or EPUB (nov-mode) in Emacs
;; 2. Press F6 to start org-noter session
;; 3. If new book: prompted for title, creates notes file as org-roam node
;; 4. If existing book: finds and opens associated notes file
;; 5. Window splits with document on left (2/3) and notes on right (1/3)
;; 6. Use 'i' to insert notes at current location
;; 7. Notes are saved as org-roam nodes in org-roam-directory
;;
;; Can also start from notes file: open notes via org-roam, press F6 to open document.
;;
;; See docs/org-noter-workflow-spec.org for full specification.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function org-id-uuid "org-id")
(declare-function nov-mode "ext:nov")
(declare-function pdf-view-mode "ext:pdf-view")
(defvar nov-file-name)
(defvar org-roam-directory)
(defvar org-dir)

;;; Configuration Variables

(defvar cj/org-noter-notes-directory
  (if (boundp 'org-roam-directory)
      org-roam-directory
    (expand-file-name "~/sync/org/roam/"))
  "Directory where org-noter notes files are stored.
Defaults to `org-roam-directory' so notes are indexed by org-roam.")

(defvar cj/org-noter-keybinding (kbd "<f6>")
  "Keybinding to start org-noter session.")

(defvar cj/org-noter-split-direction 'horizontal
  "Direction to split window for notes.
`vertical' puts notes on the right (side-by-side).
`horizontal' puts notes on the bottom (stacked).")

(defvar cj/org-noter-split-fraction 0.67
  "Fraction of window for document (notes get the remainder).
Default 0.67 means document gets 2/3, notes get 1/3.")

;;; Helper Functions

(defun cj/org-noter--title-to-slug (title)
  "Convert TITLE to lowercase hyphenated slug for filename.
Example: \"The Pragmatic Programmer\" -> \"the-pragmatic-programmer\""
  (let ((slug (downcase title)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-\\|-$" "" slug))
    slug))

(defun cj/org-noter--generate-notes-template (title doc-path)
  "Generate org-roam notes template for TITLE and DOC-PATH."
  (format ":PROPERTIES:
:ID: %s
:ROAM_REFS: %s
:NOTER_DOCUMENT: %s
:END:
#+title: Notes on %s
#+FILETAGS: :ReadingNotes:
#+CATEGORY: %s

* Notes
"
          (org-id-uuid)
          doc-path
          doc-path
          title
          title))

(defun cj/org-noter--in-document-p ()
  "Return non-nil if current buffer is a PDF or EPUB document."
  (or (derived-mode-p 'pdf-view-mode)
      (derived-mode-p 'nov-mode)))

(defun cj/org-noter--in-notes-file-p ()
  "Return non-nil if current buffer is an org-noter notes file."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (goto-char (point-min))
         (org-entry-get nil "NOTER_DOCUMENT"))))

(defun cj/org-noter--get-document-path ()
  "Get file path of current document."
  (cond
   ((derived-mode-p 'nov-mode) nov-file-name)
   ((derived-mode-p 'pdf-view-mode) (buffer-file-name))
   (t nil)))

(defun cj/org-noter--extract-document-title ()
  "Extract title from current document filename.
Uses filename (without extension) for both PDFs and EPUBs."
  (file-name-base (cj/org-noter--get-document-path)))

(defun cj/org-noter--find-notes-file ()
  "Find existing notes file for current document.
Searches `cj/org-noter-notes-directory' for org files with matching
NOTER_DOCUMENT property. Returns path to notes file or nil."
  (let ((doc-path (cj/org-noter--get-document-path)))
    (when doc-path
      (cl-find-if
       (lambda (file)
         (with-temp-buffer
           (insert-file-contents file nil 0 1000)
           (string-match-p (regexp-quote doc-path) (buffer-string))))
       (directory-files cj/org-noter-notes-directory t "\\.org$")))))

(defun cj/org-noter--create-notes-file ()
  "Create new org-roam notes file for current document.
Prompts user to confirm/edit title (pre-slugified), generates filename,
creates org-roam node with proper properties. Returns path to new file."
  (let* ((doc-path (cj/org-noter--get-document-path))
         (default-title (cj/org-noter--title-to-slug
                         (cj/org-noter--extract-document-title)))
         (title (read-string "Notes title: " default-title))
         (slug (cj/org-noter--title-to-slug title))
         (filename (format "notes-on-%s.org" slug))
         (filepath (expand-file-name filename cj/org-noter-notes-directory)))
    (unless (file-exists-p filepath)
      (with-temp-file filepath
        (insert (cj/org-noter--generate-notes-template title doc-path))))
    (find-file-noselect filepath)
    filepath))

;;; Main Entry Point

(defun cj/org-noter--session-active-p ()
  "Return non-nil if an org-noter session is active for current buffer."
  (and (boundp 'org-noter--session)
       org-noter--session))

(defun cj/org-noter--toggle-notes-window ()
  "Toggle visibility of notes window in active org-noter session.
Preserves PDF fit setting when toggling."
  (let ((notes-window (org-noter--get-notes-window))
        (pdf-fit (and (derived-mode-p 'pdf-view-mode)
                      (bound-and-true-p pdf-view-display-size))))
    (if notes-window
        (delete-window notes-window)
      (org-noter--get-notes-window 'start))
    ;; Restore PDF fit setting
    (when pdf-fit
      (pcase pdf-fit
        ('fit-width (pdf-view-fit-width-to-window))
        ('fit-height (pdf-view-fit-height-to-window))
        ('fit-page (pdf-view-fit-page-to-window))
        (_ nil)))))

(defun cj/org-noter-start ()
  "Start org-noter session or toggle notes window if session active.
When called from a document (PDF/EPUB):
  - If session active: toggle notes window visibility
  - If no session: find or create notes file, start session
When called from a notes file:
  - If session active: switch to document window
  - If no session: start session"
  (interactive)
  (cond
   ;; In document with active session - toggle notes
   ((and (cj/org-noter--in-document-p)
         (cj/org-noter--session-active-p))
    (cj/org-noter--toggle-notes-window))
   ;; In notes file with active session - switch to document
   ((and (cj/org-noter--in-notes-file-p)
         (cj/org-noter--session-active-p))
    (let ((doc-window (org-noter--get-doc-window)))
      (when doc-window
        (select-window doc-window))))
   ;; In document without session - start new session
   ((cj/org-noter--in-document-p)
    (let ((notes-file (or (cj/org-noter--find-notes-file)
                          (cj/org-noter--create-notes-file))))
      (when notes-file
        ;; Open notes file and call org-noter from there
        (find-file notes-file)
        (goto-char (point-min))
        (org-noter))))
   ;; In notes file without session - start session
   ((cj/org-noter--in-notes-file-p)
    (org-noter))
   (t
    (message "Not in a document or org-noter notes file"))))

;;; Package Configuration

(use-package djvu
  :defer 0.5)

(use-package org-pdftools
  :after (org pdf-tools)
  :hook (org-mode . org-pdftools-setup-link))

(global-set-key (kbd "<f6>") #'cj/org-noter-start)

(use-package org-noter
  :after (:any org pdf-tools djvu nov)
  :commands org-noter
  :config
  ;; Window layout based on cj/org-noter-split-direction
  (setq org-noter-notes-window-location
        (if (eq cj/org-noter-split-direction 'vertical)
            'horizontal-split    ; confusingly named: horizontal-split = side-by-side
          'vertical-split))      ; vertical-split = stacked

  ;; Split ratio from configuration (first is notes, second is doc)
  (setq org-noter-doc-split-fraction
        (cons (- 1.0 cj/org-noter-split-fraction)
              cj/org-noter-split-fraction))

  ;; Basic settings
  (setq org-noter-always-create-frame nil)
  (setq org-noter-notes-window-behavior '(start scroll))
  (setq org-noter-notes-search-path (list cj/org-noter-notes-directory))
  (setq org-noter-separate-notes-from-heading t)
  (setq org-noter-kill-frame-at-session-end nil)

  (setq org-noter-auto-save-last-location t)
  (setq org-noter-insert-selected-text-inside-note t)
  (setq org-noter-closest-tipping-point 0.3)
  (setq org-noter-hide-other t)

  ;; Load integration file if exists
  (let ((integration-file (expand-file-name "org-noter-integration.el"
                                            (file-name-directory (locate-library "org-noter")))))
    (when (file-exists-p integration-file)
      (load integration-file)))

  ;; PDF tools integration
  (when (featurep 'org-noter-integration)
    (setq org-noter-use-pdftools-link-location t)
    (setq org-noter-use-org-id t)
    (setq org-noter-use-unique-org-id t))

  ;; Defer org-roam integration to avoid slowing PDF load
  (with-eval-after-load 'org-roam
    (org-noter-enable-org-roam-integration)))

(provide 'org-noter-config)
;;; org-noter-config.el ends here
