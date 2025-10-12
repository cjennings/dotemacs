;;; org-noter-config.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Open a PDF or DjVu file, hit F6, and org-noter splits the frame with notes beside the document.
;; Notes live under ~/sync/org-noter/reading-notes.org by default; adjust the path when prompted the first time.
;; Use org-noter capture keys while annotating—`C-c n c` checks linked documents, and `C-c n u` rewrites stale paths after moving files.
;; Sessions resume where you stopped thanks to automatic location saves.

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
  :bind ("<f6>" . org-noter)
  :config
  ;; Basic settings
  (setq org-noter-always-create-frame nil)
  (setq org-noter-notes-window-location 'horizontal-split)
  (setq org-noter-notes-window-behavior '(start scroll))  ; note: must be a list!
  (setq org-noter-doc-split-fraction '(0.5 . 0.5))
  (setq org-noter-notes-search-path (list (concat sync-dir "/org-noter/")))
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
	(org-noter-enable-org-roam-integration)

  (org-noter-enable-org-roam-integration))

(provide 'org-noter-config)
;;; org-noter-config.el ends here.
