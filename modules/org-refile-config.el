;;; org-refile-config.el --- Org Refile Customizations -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(use-package org-refile
  :ensure nil ;; built-in
  :defer .5
  :bind
  (:map org-mode-map
		("C-c C-w"   . cj/org-refile)
		("C-c w"     . cj/org-refile-in-file))
  :config

  ;; ----------------------------- Org Refile Targets ----------------------------
  ;; sets refile targets
  ;; - adds project files in org-roam to the refile targets
  ;; - adds todo.org files in subdirectories of the code and project directories

  (defun cj/build-org-refile-targets()
	"Build org-refile-targets."
	(interactive)
	(let (new-files)
	  ;; Start with the inbox and the schedule files.
	  (setq new-files `((,inbox-file . (:maxlevel . 1)) (,schedule-file . (:maxlevel . 1))))

	  ;; Extend new-files with the project and topic files.
	  (let ((project-and-topic-files (append (cj/org-roam-list-notes-by-tag "Project")
											 (cj/org-roam-list-notes-by-tag "Topic"))))
		(let ((file-rule `(:level . 1)))
		  (dolist (file project-and-topic-files)
			(unless (assoc file new-files)
			  (push `(,file . ,file-rule) new-files)))))

	  ;; Extend new-files with todo.org files in the specified directories.
	  (dolist (dir (list user-emacs-directory code-dir projects-dir))
		(let ((todo-files (directory-files-recursively dir "^[Tt][Oo][Dd][Oo]\\.[Oo][Rr][Gg]$")))
		  (let ((file-rule `(:level . 1)))
			(dolist (file todo-files)
			  (unless (assoc file new-files)
				(push `(,file . ,file-rule) new-files))))))

	  ;; Set org-refile-targets.
	  (setq org-refile-targets (nreverse new-files))))

  ;; -------------------------------- Org-Refile -------------------------------
  ;; used in place of org-refile to ensure the refile targets are rebuilt.

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

  ) ; end use-package statement


;; ----------------------- Save Org Files After Refile -----------------------
;; advice that saves all open org buffers after a refile is complete

(advice-add 'org-refile :after
			(lambda (&rest _)
			  (org-save-all-org-buffers)))



(provide 'org-refile-config)
;;; org-refile-config.el ends here.
