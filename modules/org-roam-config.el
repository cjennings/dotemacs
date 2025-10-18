;;; org-roam-config.el --- Org-Roam Config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; Currently a work in progress. The initial version of this was taken from David Wilson:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

;;; Code:

(require 'user-constants)

;; ---------------------------------- Org Roam ---------------------------------

(use-package org-roam
  :after org
  :defer 1
  :commands (org-roam-node-find org-roam-node-insert)
  :hook (after-init . org-roam-db-autosync-mode)
  :custom
  (org-roam-directory roam-dir)
  (org-roam-dailies-directory journals-dir)
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M:%S %p %Z> %?"
	  :if-new (file+head "%<%Y-%m-%d>.org"
						 "#+FILETAGS: Journal #+TITLE: %<%Y-%m-%d>"))))

  (org-roam-capture-templates
   `(("d" "default" plain "%?"
	  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
						 "#+TITLE: ${title}\n")
	  :unnarrowed t)

	 ("v" "v2mom" plain
	  (file ,(concat user-emacs-directory "org-roam-templates/v2mom.org"))
	  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
	  :unnarrowed t)

	 ("r" "recipe" plain
	  (file ,(concat user-emacs-directory "org-roam-templates/recipe.org"))
	  :if-new (file+head "recipes/%<%Y%m%d%H%M%S>-${slug}.org" "")
	  :unnarrowed t)

	 ("t" "topic" plain
	  (file ,(concat user-emacs-directory "org-roam-templates/topic.org"))
	  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
	  :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n p" . cj/org-roam-find-node-project)
         ("C-c n r" . cj/org-roam-find-node-recipe)
         ("C-c n t" . cj/org-roam-find-node-topic)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n w" . cj/org-roam-find-node-webclip)
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

  (when (fboundp 'cj/build-org-refile-targets)
	(cj/build-org-refile-targets))

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
This is mainly a wrapper around org-roam-node-insert to achieve immediate finish
to the capture. The prefix ARG and ARGS are the filter function and the rest of
the arguments that org-roam-node-insert expects."
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

;;;###autoload
(defun cj/org-roam-find-node (tag template-key template-file &optional subdir)
  "List all nodes of type TAG in completing read for selection or creation.
Interactively find or create an Org-roam node with a given TAG. Newly
created nodes are added to the agenda and follow a template defined by
TEMPLATE-KEY and TEMPLATE-FILE."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook
			#'cj/org-roam-add-node-to-agenda-files-finalize-hook)
  (org-roam-node-find
   nil nil (cj/org-roam-filter-by-tag tag) nil
   :templates
   `((,template-key ,tag plain (file ,template-file)
	  :if-new (file+head ,(concat (or subdir "") "%<%Y%m%d%H%M%S>-${slug}.org") "")
	  :unnarrowed t))))

;;;###autoload
(defun cj/org-roam-find-node-topic ()
  "List nodes of type \=`topic\=` in completing read for selection or creation."
  (interactive)
  (cj/org-roam-find-node "Topic" "t" (concat roam-dir "templates/topic.org")))

;;;###autoload
(defun cj/org-roam-find-node-recipe ()
  (interactive)
  (cj/org-roam-find-node "Recipe" "r" (concat roam-dir "templates/recipe.org") "recipes/"))

;; ---------------------- Org Capture After Finalize Hook ----------------------

(defun cj/org-roam-add-node-to-agenda-files-finalize-hook ()
  "Add the captured project file to \='org-agenda-files\='."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook
               #'cj/org-roam-add-node-to-agenda-files-finalize-hook)


  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

;; ------------------------ Org Roam Copy Done To Daily ------------------------

(defun cj/org-roam-copy-todo-to-today ()
  "Copy completed tasks to today's daily org-roam node."
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                   "#+FILETAGS: Journal
#+TITLE: %<%Y-%m-%d>\n" ("Completed Tasks")))))
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

;;;###autoload
(defun cj/move-org-branch-to-roam ()
  "Move the org subtree at point to a new org-roam node.
The node filename will be timestamp-based with the heading name.
The heading becomes the node title, and the entire subtree is demoted to level 1.
If the heading contains a link, extract the description for the title."
  (interactive)
  ;; Lazy load org and org-roam when needed
  (require 'org)
  (require 'org-id)
  (require 'org-roam)

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

(provide 'org-roam-config)
;;; org-roam-config.el ends here.
