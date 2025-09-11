;;; org-refile-config.el --- Org Refile Customizations -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; Configuration and custom functions for org-mode refiling.

;;; Code:

;; ----------------------------- Org Refile Targets ----------------------------
;; sets refile targets
;; - adds project files in org-roam to the refile targets
;; - adds todo.org files in subdirectories of the code and project directories

(defun cj/build-org-refile-targets ()
  "Build `org-refile-targets`."
  (interactive)
  (let ((new-files
		 (list
		  (cons inbox-file     '(:maxlevel . 1))
		  (cons reference-file '(:maxlevel . 2))
		  (cons schedule-file  '(:maxlevel . 1)))))

	;; Extend with org-roam files if available.
	(when (fboundp 'cj/org-roam-list-notes-by-tag)
	  (let* ((project-and-topic-files
			  (append (cj/org-roam-list-notes-by-tag "Project")
					  (cj/org-roam-list-notes-by-tag "Topic")))
			 (file-rule '(:maxlevel . 1)))
		(dolist (file project-and-topic-files)
		  (unless (assoc file new-files)
			(push (cons file file-rule) new-files)))))

	;; Add todo.org files from known directories
    (dolist (dir (list user-emacs-directory code-dir projects-dir))
	  (let* ((todo-files (directory-files-recursively
						  dir "^[Tt][Oo][Dd][Oo]\\.[Oo][Rr][Gg]$"))
			 (file-rule '(:maxlevel . 1)))
		(dolist (file todo-files)
		  (unless (assoc file new-files)
			(push (cons file file-rule) new-files))))))

  (setq org-refile-targets (nreverse new-files)))

(defun cj/org-refile (&optional ARG DEFAULT-BUFFER RFLOC MSG)
  "Simply rebuilds the refile targets before calling org-refile.
ARG DEFAULT-BUFFER RFLOC and MSG parameters passed to org-refile."
  (interactive "P")
  (cj/build-org-refile-targets)
  (org-refile ARG DEFAULT-BUFFER RFLOC MSG))

;; ----------------------------- Org Refile In File ----------------------------
;; convenience function for scoping the refile candidates to the current buffer.

(defun cj/org-refile-in-file ()
  "Refile to a target within the current file and save the buffer."
  (interactive)
  (let ((org-refile-targets `(((,(buffer-file-name)) :maxlevel . 6))))
	(call-interactively 'org-refile)
	(save-buffer)))


;; --------------------------------- Org Refile --------------------------------
;;

(use-package org-refile
  :ensure nil ;; built-in
  :defer .5
  :bind
  (:map org-mode-map
		("C-c C-w"   . cj/org-refile)
		("C-c w"     . cj/org-refile-in-file))
  :config
  ;; save all open org buffers after a refile is complete
  (advice-add 'org-refile :after
			  (lambda (&rest _)
				(org-save-all-org-buffers))))

(provide 'org-refile-config)
;;; org-refile-config.el ends here.
