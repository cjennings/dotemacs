;;; org-noter-config.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Org-noter configuration for taking notes on PDF and DjVu documents. Workflow:
;; open a PDF/DjVu file in Emacs, press F6 to start org-noter session, frame
;; splits with document on one side and notes on the other, notes are saved to
;; ~/sync/org-noter/reading-notes.org by default, and position is automatically
;; saved when closing session. Features include integration with pdf-tools and
;; djvu, org-roam integration for linking notes, automatic session resumption at
;; last position, inserting highlighted text into notes, notes following
;; TASK: Aborted Commentary

;;; Code:

(use-package djvu
  :defer 0.5)

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package org-pdftools
  :after (org pdf-tools)
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter
  :after (:any org pdf-tools djvu)
  :commands org-noter
  :config
  ;; Basic settings
  (setq org-noter-always-create-frame nil)
  (setq org-noter-notes-window-location 'horizontal-split)
  (setq org-noter-notes-window-behavior '(start scroll))  ; note: must be a list!
  (setq org-noter-doc-split-fraction '(0.5 . 0.5))
  (setq org-noter-notes-search-path (list (concat org-dir "/org-noter/")))
  (setq org-noter-default-notes-file-names '("reading-notes.org"))
  (setq org-noter-separate-notes-from-heading t)
  (setq org-noter-kill-frame-at-session-end t)  ; kill frame when closing session

  (setq org-noter-auto-save-last-location t)  ; Save position when closing
  (setq org-noter-insert-selected-text-inside-note t)  ; Insert highlighted text
  (setq org-noter-closest-tipping-point 0.3)  ; When to show closest previous note
  (setq org-noter-hide-other t)  ; Hide unrelated notes

  ;; Load the integration file if it exists in your config
  (let ((integration-file (expand-file-name "org-noter-integration.el"
											(file-name-directory (locate-library "org-noter")))))
	(when (file-exists-p integration-file)
	  (load integration-file)))

  ;; If you want to use the org-noter-pdftools integration features
  (when (featurep 'org-noter-integration)
    (setq org-noter-use-pdftools-link-location t)
    (setq org-noter-use-org-id t)
    (setq org-noter-use-unique-org-id t))

  (org-noter-enable-org-roam-integration))

(provide 'org-noter-config)
;;; org-noter-config.el ends here.
