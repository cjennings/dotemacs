;;; org-webclipper.el --- Web Page Clipping Workflow to Org Roam -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Allows saving a copy of the page EWW is visiting for offline reading.
;; In other words, it's a "Pocket/Instapaper" that collects the articles in an Emacs org-mode file.
;;
;; I review the articles, then add the ones I want for future reference by moving it to an
;; org-roam file.
;;
;;; Code:

(require 'user-constants) ;; for location of 'webclipped-file'

;; ---------------------------- Org Webpage Clipper ----------------------------

(defun cj/org-webpage-clipper ()
  "Capture the current web page for later viewing in an Org file.

Return the yanked content as a string so templates can insert it."
  (interactive)
  (let* ((source-buffer (org-capture-get :original-buffer))
		 (source-mode (with-current-buffer source-buffer major-mode)))
	(cond
	 ((eq source-mode 'w3m-mode)
	  (with-current-buffer source-buffer
		(org-w3m-copy-for-org-mode)))
	 ((eq source-mode 'eww-mode)
	  (with-current-buffer source-buffer
		(org-eww-copy-for-org-mode)))
	 (t
	  (error "Not valid -- must be in w3m or eww mode")))
	;; extract the webpage content from the kill ring
	(car kill-ring)))

;; ------------------------------ Capture Template -----------------------------

(with-eval-after-load 'org-capture
  ;; Ensure org-capture-templates exists before adding to it
  (unless (boundp 'org-capture-templates)
	(setq org-capture-templates nil))

  ;; Add the webclipper template to org-capture-templates
  (add-to-list 'org-capture-templates
			   '("w" "Web Page Clipper" entry
				 (file+headline webclipped-file "Webclipped Inbox")
				 "* %a\nURL: %L\nCaptured On:%U\n%(cj/org-webpage-clipper)\n"
				 :prepend t :immediate-finish t)
			   t))

;; ------------------------ Org-Branch To Org-Roam-Node ------------------------

(defun cj/org-link-get-description (text)
  "Extract the description from an org link, or return the text unchanged.
If TEXT contains an org link like [[url][description]], return description.
If TEXT contains multiple links, only process the first one.
Otherwise return TEXT unchanged."
  (if (string-match "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]" text)
	  (let ((description (match-string 2 text))
			(url (match-string 1 text)))
		;; If there's a description, use it; otherwise use the URL
		(or description url))
	text))

(defun cj/move-org-branch-to-roam ()
  "Move the org subtree at point to a new org-roam node.
The node filename will be timestamp-based with the heading name.
The heading becomes the node title, and the entire subtree is demoted to level 1.
If the heading contains a link, extract the description for the title."
  (interactive)
  (unless (org-at-heading-p)
	(user-error "Not at an org heading"))

  (let* ((heading-components (org-heading-components))
		 (current-level (nth 0 heading-components))
		 (raw-title (nth 4 heading-components))
		 ;; Extract clean title from potential link
		 (title (cj/org-link-get-description raw-title))
		 (timestamp (format-time-string "%Y%m%d%H%M%S"))
		 ;; Convert title to filename-safe format
		 (title-slug (replace-regexp-in-string
					  "[^a-zA-Z0-9]+" "-"
					  (downcase title)))
		 ;; Remove leading/trailing hyphens
		 (title-slug (replace-regexp-in-string
					  "^-\\|-$" "" title-slug))
		 (filename (format "%s-%s.org" timestamp title-slug))
		 (filepath (expand-file-name filename org-roam-directory))
		 ;; Generate a unique ID for the node
		 (node-id (org-id-new))
		 ;; Store the subtree in a temporary buffer
		 subtree-content)

	;; Copy the subtree content
	(org-copy-subtree)
	(setq subtree-content (current-kill 0))

	;; Now cut it to remove from original buffer
	(org-cut-subtree)

	;; Process the subtree to demote it to level 1
	(with-temp-buffer
	  (org-mode)
	  (insert subtree-content)
	  ;; Demote the entire tree so the top level becomes level 1
	  (goto-char (point-min))
	  (when (> current-level 1)
		(let ((demote-count (- current-level 1)))
		  (while (re-search-forward "^\\*+ " nil t)
			(beginning-of-line)
			(dotimes (_ demote-count)
			  (when (looking-at "^\\*\\*")
				(delete-char 1)))
			(forward-line))))
	  (setq subtree-content (buffer-string)))

	;; Create the new org-roam file
	(with-temp-file filepath
	  ;; Insert the org-roam template with ID at file level
	  (insert ":PROPERTIES:\n")
	  (insert ":ID:       " node-id "\n")
	  (insert ":END:\n")
	  (insert "#+TITLE: " title "\n")
	  (insert "#+CATEGORY: " title "\n")
	  (insert "#+FILETAGS: Topic\n\n")

	  ;; Insert the demoted subtree content
	  (insert subtree-content))

	;; Sync the org-roam database
	(org-roam-db-sync)

	;; Message to user
	(message "'%s' added as an org-roam node." title)))

;; ----------------------------- Webclipper Keymap -----------------------------

;; Buffer & file operations prefix and keymap
(define-prefix-command 'cj/webclipper-map nil
					   "Keymap for weblipper operations.")
(define-key cj/custom-keymap "w" 'cj/webclipper-map)
(define-key cj/webclipper-map "N" 'cj/move-org-branch-to-roam) ;; for node

(provide 'org-webclipper)
;;; org-webclipper.el ends here.
