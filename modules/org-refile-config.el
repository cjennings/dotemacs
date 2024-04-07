;;; org-refile-config.el --- Org Refile Customizations -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(with-eval-after-load 'org-roam-config
  (require 'org-refile)

  ;; ----------------------------- Org Refile Targets ----------------------------
  ;; sets refile targets
  ;; - adds project files in org-roam to the refile targets
  ;; - adds todo.org files in subdirectories of the code and project directories

  (defun cj/add-files-to-org-refile-targets (directory)
    "Recursively searches for all files named 'todo.org' in DIRECTORY and adds them to org-project-files."
    (interactive "D")
    (let ((files (directory-files-recursively directory "^[Tt][Oo][Dd][Oo]\\.[Oo][Rr][Gg]$" t)))
      (dolist (file files)
        (add-to-list 'org-refile-targets `(,file . (:level . 1))))))

  (defun cj/build-org-refile-targets()
	"Build org-refile-targets.
Starts with the schedule file, then adds the Emacs task list,
and any task list in the code or projects directories."
	(interactive)
	(let ((new-files
		   (append
			(cj/org-roam-list-notes-by-tag "Project")
			(cj/org-roam-list-notes-by-tag "Topic"))))
	  (dolist (file new-files)
		(unless (member file org-agenda-files)
		  (setq org-agenda-files (cons file org-agenda-files))))
	  )
	(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
	(cj/add-files-to-org-refile-targets user-emacs-directory)
	(cj/add-files-to-org-refile-targets code-dir)
	(cj/add-files-to-org-refile-targets projects-dir))

  ;; --------------------------------- Org-Refile -------------------------------
  ;; convenience function for scoping the refile candidates to the current buffer.

  (defun cj/org-refile-in-file ()
	"Refile to a target within the current file and save the buffer."
    (interactive)
    (let ((org-refile-targets `(((,(buffer-file-name)) :maxlevel . 6))))
	  (call-interactively 'org-refile)
	  (save-buffer)))

  ;; ----------------------- Save Org Files After Refile -----------------------
  ;; advice that saves all open org buffers after a refile is complete

  (advice-add 'org-refile :after
			  (lambda (&rest _)
				(org-save-all-org-buffers)))


  ) ;; end with eval-after-load 'org-roam

(provide 'org-refile-config)
;;; org-refile-config.el ends here.
