;;; org-roam-config.el --- Org-Roam Config -*- lexical-binding: t; -*-

;;; Commentary:
;; Currently a work in progress. The initial version of this was taken from David Wilson:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

;;; Code:


;; ---------------------------------- Org Roam ---------------------------------

(use-package org-roam
  :after org
  :defer .5
  :custom
  (org-roam-directory "~/sync/org/roam/")
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M:%S %p %Z> %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+FILETAGS: Journal\n#+TITLE: %<%Y-%m-%d>"))))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("v" "v2mom" plain (file "~/sync/org/roam/templates/v2mom.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("r" "recipe" plain  (file "~/sync/org/roam/templates/recipe.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Recipe")
      :unnarrowed t)
     ("p" "project" plain  (file "~/sync/org/roam/templates/project.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Project")
      :unnarrowed t)
     ("t" "topic" plain  (file "~/sync/org/roam/templates/topic.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Topic")
      :unnarrowed t)))
  :bind (("C-c n ?" . org-roam-hydra/body)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n p" . cj/org-roam-find-project)
         ("C-c n r" . cj/org-roam-find-recipe)
         ("C-c n t" . cj/org-roam-find-topic)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (setq org-log-done  'time)
  (setq org-agenda-timegrid-use-ampm t)

  ;; remove/disable if performance slows
  ;; (setq org-element-use-cache nil) ;; disables caching org files

  ;; move closed tasks to today's journal when marked done
  (add-to-list 'org-after-todo-state-change-hook
			 (lambda ()
			   (when (equal org-state "DONE")
				 (cj/org-roam-copy-todo-to-today))))

  (require 'org-roam-dailies)      ;; Ensures the keymap is available
  (org-roam-db-autosync-mode))

;; ------------------------- Org Roam Insert Immediate -------------------------

(defun cj/org-roam-node-insert-immediate (arg &rest args)
  "Create new node and insert its link immediately.
The prefix ARG .
ARGS represents the node name to link."
  (interactive "P")
  (let ((args (cons arg args))
		(org-roam-capture-templates (list (append (car org-roam-capture-templates)
												  '(:immediate-finish t)))))
	(apply #'org-roam-node-insert args)))
(global-set-key (kbd "C-c n I") 'cj/org-roam-node-insert-immediate)

;; ------------------------- Tag Listing And Filtering -------------------------

(defun cj/org-roam-filter-by-tag (tag-name)
  (lambda (node)
	(member tag-name (org-roam-node-tags node))))

(defun cj/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
		  (seq-filter
		   (cj/org-roam-filter-by-tag tag-name)
		   (org-roam-node-list))))

;; -------------------------- Org Roam Find Functions --------------------------

(defun cj/org-roam-find-node (tag template-key template-file)
  "List all node of type \\=`TAG\\=` in completing read for selection or creation."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'cj/org-roam-add-node-to-agenda-files-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (cj/org-roam-filter-by-tag tag)
   nil
   :templates
   `((,template-key ,tag plain  (file ,template-file)
					:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ,(concat "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: " tag))
					:unnarrowed t))))

(defun cj/org-roam-find-topic ()
  "List all node of type \\=`topic\\=` in completing read for selection or creation."
  (interactive)
  (cj/org-roam-find-node "Topic" "t" "~/sync/org/roam/templates/topic.org"))

(defun cj/org-roam-find-recipe ()
  "List all node of type \\=`recipe\\=` in completing read for selection or creation."
  (interactive)
  (cj/org-roam-find-node "Recipe" "r" "~/sync/org/roam/templates/recipe.org"))

(defun cj/org-roam-find-project ()
  "List all node of type \\='project\\=' in completing read for selection or creation."
  (interactive)
  (cj/org-roam-find-node "Project" "p" "~/sync/org/roam/templates/project.org"))

;; ---------------------- Org Capture After Finalize Hook ----------------------

(defun cj/org-roam-add-node-to-agenda-files-finalize-hook ()
  "Add the captured project file to \\='org-agenda-files\\='."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'cj/org-roam-add-node-to-agenda-files-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
	(with-current-buffer (org-capture-get :buffer)
	  (add-to-list 'org-agenda-files (buffer-file-name)))))

;; ------------------------------- Capture Inbox -------------------------------

(defun cj/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
					 :templates '(("i" "inbox" plain "** %?"
								   :if-new (file+head+olp "~/sync/org/roam/inbox.org"
														  "#+TITLE: Inbox\n#+CATEGORY: Inbox\n#+FILETAGS: Project"
														  ("Inbox"))))))
(global-set-key (kbd "C-t") #'cj/org-roam-capture-inbox)

;; -------------------------------- Capture Task -------------------------------

(defun cj/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'cj/org-roam-add-node-to-agenda-files-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
							nil
							(cj/org-roam-filter-by-tag "Project"))
					 :templates '(("p" "project" plain "** TODO %?"
								   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
														  "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Project"
														  ("${title}"))))))
(global-set-key (kbd "C-c n t") #'cj/org-roam-capture-task)

;; ------------------------ Org Roam Copy Done To Daily ------------------------

(defun cj/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
		(org-roam-dailies-capture-templates
		 '(("t" "tasks" entry "%?"
			:if-new (file+head+olp "%<%Y-%m-%d>.org" "#+FILETAGS: Journal\n#+TITLE: %<%Y-%m-%d>\n" ("Completed Tasks")))))
		(org-after-refile-insert-hook #'save-buffer)
		today-file
		pos)
	(save-window-excursion
	  (org-roam-dailies--capture (current-time) t)
	  (setq today-file (buffer-file-name))
	  (setq pos (point)))

	;; Only refile if the target file is different than the current file
	(unless (equal (file-truename today-file)
				   (file-truename (buffer-file-name)))
	  (org-refile nil nil (list "Completed Tasks" today-file nil pos)))))



(provide 'org-roam-config)
;;; org-roam-config.el ends here.
