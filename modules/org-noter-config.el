;;; org-noter-config.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

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
  (setq org-noter-kill-frame-at-session-end nil)  ; Don't kill frame when closing session

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
	(setq org-noter-use-unique-org-id t)))

(defun cj/org-noter-check-document-path ()
  "Check if the document path in the current heading exists.
This is just a temporary debug function."
  (interactive)
  (let ((doc-path (org-entry-get nil "NOTER_DOCUMENT" t)))
	(if doc-path
		(let ((full-path (expand-file-name doc-path
										   (file-name-directory (buffer-file-name)))))
		  (if (file-exists-p full-path)
			  (message "Document found: %s" full-path)
			(message "Document NOT found: %s" full-path)))
	  (message "No NOTER_DOCUMENT property found"))))

;; Additional helper function to fix paths if needed
(defun cj/org-noter-update-document-path ()
  "Update the document path to be relative to the current org file."
  (interactive)
  (when-let ((doc-path (org-entry-get nil "NOTER_DOCUMENT" t)))
	(let* ((org-dir (file-name-directory (buffer-file-name)))
		   (full-path (expand-file-name doc-path org-dir)))
	  (if (file-exists-p full-path)
		  (let ((relative-path (file-relative-name full-path org-dir)))
			(org-entry-put nil "NOTER_DOCUMENT" relative-path)
			(message "Updated path to: %s" relative-path))
		(message "Document not found: %s" full-path)))))

;; Keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n c") 'cj/org-noter-check-document-path)
  (define-key org-mode-map (kbd "C-c n u") 'cj/org-noter-update-document-path))

(provide 'org-noter-config)
;;; org-noter-config.el ends here.
