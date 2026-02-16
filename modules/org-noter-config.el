;;; org-noter-config.el --- Org-noter configuration -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Org-noter configuration for taking notes on PDF and EPUB documents.
;;
;; Workflow:
;; 1. Open a PDF (pdf-view-mode) or EPUB (nov-mode) in Emacs
;; 2. Press 'i' to start org-noter session and insert a note
;; 3. If new book: prompted for title, creates notes file as org-roam node
;; 4. If existing book: finds and opens associated notes file
;; 5. Window splits with document (70%) and notes (30%)
;;    Split direction chosen automatically based on frame aspect ratio
;; 6. Notes are saved as org-roam nodes in roam-dir
;;
;; Keybindings (C-; n prefix):
;;   i - insert note (starts session if needed)
;;   t - toggle notes window
;;   T - toggle window position (side/bottom)
;;   n/p/. - sync next/prev/current note
;;   s - headings from TOC
;;   q - kill session

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function org-id-uuid "org-id")
(declare-function nov-mode "ext:nov")
(declare-function pdf-view-mode "ext:pdf-view")
(defvar nov-file-name)
;;; Configuration Variables

(defvar cj/org-noter-notes-directory roam-dir
  "Directory where org-noter notes files are stored.
Uses `roam-dir' from user-constants so notes are indexed by org-roam.")

(defun cj/org-noter--preferred-split ()
  "Return preferred split direction based on frame aspect ratio.
Returns `horizontal-split' (side-by-side) if frame is wide,
`vertical-split' (stacked) otherwise."
  (let ((width (frame-pixel-width))
        (height (frame-pixel-height)))
    (if (> (/ (float width) height) 1.4)
        'horizontal-split
      'vertical-split)))

(defvar cj/org-noter-split-fraction 0.70
  "Fraction of window for document (notes get the remainder).
Default 0.70 means document gets 70%, notes get 30%.")

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
        ;; Recalculate split direction based on current frame dimensions
        (setq org-noter-notes-window-location (cj/org-noter--preferred-split))
        ;; Start org-noter from the notes buffer without leaving the document
        (let ((notes-buf (find-file-noselect notes-file)))
          (with-current-buffer notes-buf
            (goto-char (point-min))
            (org-noter))))))
   ;; In notes file without session - start session
   ((cj/org-noter--in-notes-file-p)
    (org-noter))
   (t
    (message "Not in a document or org-noter notes file"))))

(defun cj/org-noter-insert-note-dwim ()
  "Insert an org-noter note, starting a session first if needed.
From a PDF/EPUB: starts org-noter session if inactive, then inserts note."
  (interactive)
  (unless (cj/org-noter--session-active-p)
    (cj/org-noter-start)
    ;; Return to the document window for the insert
    (when (cj/org-noter--session-active-p)
      (let ((doc-window (org-noter--get-doc-window)))
        (when doc-window
          (select-window doc-window)))))
  (when (cj/org-noter--session-active-p)
    (org-noter-insert-note)))

;;; Package Configuration

(use-package djvu
  :defer 0.5)

(use-package org-pdftools
  :after (org pdf-tools)
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter
  :after (:any org pdf-tools djvu nov)
  :commands org-noter
  :config
  ;; Window layout calculated dynamically at session start
  (setq org-noter-notes-window-location (cj/org-noter--preferred-split))

  ;; Split ratio: document gets cj/org-noter-split-fraction, notes get the rest
  (setq org-noter-doc-split-fraction
        (cons cj/org-noter-split-fraction
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

;;; ---------------------- Notes Window Background Highlight --------------------

(defvar-local cj/org-noter--bg-remap-cookie nil
  "Cookie for the active-window background face remapping.")

(defun cj/org-noter--update-active-bg (&rest _)
  "Toggle notes buffer background based on whether its window is selected."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p org-noter-notes-mode)
        (let ((active (eq buf (window-buffer (selected-window)))))
          (cond
           ((and active (not cj/org-noter--bg-remap-cookie))
            (setq cj/org-noter--bg-remap-cookie
                  (face-remap-add-relative 'default :background "#1d1b19")))
           ((and (not active) cj/org-noter--bg-remap-cookie)
            (face-remap-remove-relative cj/org-noter--bg-remap-cookie)
            (setq cj/org-noter--bg-remap-cookie nil))))))))

(defun cj/org-noter--setup-notes-bg ()
  "Set up focus-based background tracking in the notes buffer."
  (add-hook 'window-selection-change-functions #'cj/org-noter--update-active-bg nil t))

(add-hook 'org-noter-notes-mode-hook #'cj/org-noter--setup-notes-bg)

;;; ----------------------------- Org-Noter Keymap -----------------------------

(defvar-keymap cj/org-noter-map
  :doc "Keymap for org-noter operations."
  "i" #'cj/org-noter-insert-note-dwim
  "n" #'org-noter-sync-next-note
  "p" #'org-noter-sync-prev-note
  "." #'org-noter-sync-current-note
  "s" #'org-noter-create-skeleton
  "q" #'org-noter-kill-session
  "t" #'cj/org-noter-start
  "T" #'org-noter-toggle-notes-window-location)
(keymap-set cj/custom-keymap "n" cj/org-noter-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; n" "org-noter menu"
    "C-; n i" "insert note"
    "C-; n n" "sync next note"
    "C-; n p" "sync prev note"
    "C-; n ." "sync current note"
    "C-; n s" "headings from TOC"
    "C-; n q" "kill session"
    "C-; n t" "toggle window"
    "C-; n T" "toggle window position"))

(provide 'org-noter-config)
;;; org-noter-config.el ends here
