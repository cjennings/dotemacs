;;; org-roam-config.el --- Org-Roam Config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; Configuration and utilities for org-roam knowledge management.
;;
;; Key features:
;; - Custom capture templates for different node types (v2mom, recipe, topic)
;; - Automatic moving of completed tasks to daily journal
;; - Tag-based node filtering and finding
;; - Branch extraction to new roam nodes (cj/move-org-branch-to-roam)
;;
;; The initial version was adapted from David Wilson:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

;;; Code:

(require 'user-constants)

;; ---------------------------------- Org Roam ---------------------------------

(use-package org-roam
  :defer 1
  :commands (org-roam-node-find org-roam-node-insert org-roam-db-autosync-mode)
  :config
  ;; Enable autosync mode after org-roam loads
  (org-roam-db-autosync-mode)
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

  (require 'org-roam-dailies)      ;; Ensures the keymap is available
  (org-roam-db-autosync-mode))

;; Move closed tasks to today's journal when marked done
(with-eval-after-load 'org
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (and (member org-state org-done-keywords)
                            (not (member org-last-state org-done-keywords))
                            ;; Don't run for gcal.org - it's managed by org-gcal
                            (not (string= (buffer-file-name) (expand-file-name gcal-file))))
                   (cj/org-roam-copy-todo-to-today)))))

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
(keymap-global-set "C-c n I" #'cj/org-roam-node-insert-immediate)

;; ------------------------- Tag Listing And Filtering -------------------------

(defun cj/org-roam-filter-by-tag (tag-name)
  "Return a predicate function that filters org-roam nodes by TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun cj/org-roam-list-notes-by-tag (tag-name)
  "Return a list of file paths for all org-roam nodes tagged with TAG-NAME."
  (mapcar #'org-roam-node-file
          (seq-filter
           (cj/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

;; -------------------------- Org Roam Find Functions --------------------------

(defun cj/org-roam-find-node (tag template-key template-file &optional subdir)
  "List all nodes of type TAG in completing read for selection or creation.
Interactively find or create an Org-roam node with a given TAG. Newly
created nodes are added to the agenda and follow a template defined by
TEMPLATE-KEY and TEMPLATE-FILE. If SUBDIR is provided, new nodes are
created in that subdirectory of `org-roam-directory'."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook
			#'cj/org-roam-add-node-to-agenda-files-finalize-hook)
  (org-roam-node-find
   nil nil (cj/org-roam-filter-by-tag tag) nil
   :templates
   `((,template-key ,tag plain (file ,template-file)
	  :if-new (file+head ,(concat (or subdir "") "%<%Y%m%d%H%M%S>-${slug}.org") "")
	  :unnarrowed t))))


(defun cj/org-roam-find-node-topic ()
  "List nodes of type \=`Topic\=` in completing read for selection or creation."
  (interactive)
  (cj/org-roam-find-node "Topic" "t" (concat roam-dir "templates/topic.org")))


(defun cj/org-roam-find-node-recipe ()
  "List nodes of type \"Recipe\" in completing read for selection or creation."
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

(defun cj/--generate-roam-slug (title)
  "Convert TITLE to a filename-safe slug.
Converts to lowercase, replaces non-alphanumeric characters with hyphens,
and removes leading/trailing hyphens.
Returns the slugified string."
  (let ((slug (replace-regexp-in-string
               "[^a-zA-Z0-9]+" "-"
               (downcase title))))
    (replace-regexp-in-string "^-\\|-$" "" slug)))

(defun cj/--demote-org-subtree (content from-level to-level)
  "Demote org subtree CONTENT from FROM-LEVEL to TO-LEVEL.
CONTENT is the org-mode text with headings.
FROM-LEVEL is the current level of the top heading (integer).
TO-LEVEL is the desired level for the top heading (integer).
Returns the demoted content as a string.
All headings in the tree are adjusted proportionally."
  (if (<= from-level to-level)
      ;; No demotion needed
      content
    (let ((demote-count (- from-level to-level)))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "^\\(\\*+\\) " nil t)
          (let* ((stars (match-string 1))
                 (level (length stars))
                 (new-level (max 1 (- level demote-count)))
                 (new-stars (make-string new-level ?*)))
            (replace-match (concat new-stars " "))))
        (buffer-string)))))

(defun cj/--format-roam-node (title node-id content)
  "Format org-roam node file CONTENT with TITLE and NODE-ID.
TITLE is the node title string.
NODE-ID is the unique identifier for the node.
CONTENT is the main body content (already demoted if needed).
Returns the complete file content as a string."
  (concat ":PROPERTIES:\n"
          ":ID:       " node-id "\n"
          ":END:\n"
          "#+TITLE: " title "\n"
          "#+CATEGORY: " title "\n"
          "#+FILETAGS: Topic\n\n"
          content))

(defun cj/move-org-branch-to-roam ()
  "Move the org subtree at point to a new org-roam node.
The node filename will be timestamp-based with the heading name.
The heading becomes the node title, and the entire subtree is demoted to
level 1.  If the heading contains a link, extract the description for the
title."
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
		 (title-slug (cj/--generate-roam-slug title))
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
	(setq subtree-content (cj/--demote-org-subtree subtree-content current-level 1))

	;; Create the new org-roam file
	(with-temp-file filepath
	  (insert (cj/--format-roam-node title node-id subtree-content)))

	;; Sync the org-roam database
	(org-roam-db-sync)

	;; Message to user
	(message "'%s' added as an org-roam node." title)))

;; TASK: Need to decide keybindings before implementation and testing
;; (use-package consult-org-roam
;;    :ensure t
;;    :after org-roam
;;    :init
;;    (require 'consult-org-roam)
;;    ;; Activate the minor mode
;;    (consult-org-roam-mode 1)
;;    :custom
;;    ;; Use `ripgrep' for searching with `consult-org-roam-search'
;;    (consult-org-roam-grep-func #'consult-ripgrep)
;;    ;; Configure a custom narrow key for `consult-buffer'
;;    (consult-org-roam-buffer-narrow-key ?r)
;;    ;; Display org-roam buffers right after non-org-roam buffers
;;    ;; in consult-buffer (and not down at the bottom)
;;    (consult-org-roam-buffer-after-buffers t)
;;    :config
;;    ;; Eventually suppress previewing for certain functions
;;    (consult-customize
;;     consult-org-roam-forward-links
;;     :preview-key "M-.")
;;    :bind
;;    ;; Define some convenient keybindings as an addition
;;    ("C-c n e" . consult-org-roam-file-find)
;;    ("C-c n b" . consult-org-roam-backlinks)
;;    ("C-c n B" . consult-org-roam-backlinks-recursive)
;;    ("C-c n l" . consult-org-roam-forward-links)
;;    ("C-c n r" . consult-org-roam-search))


;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c n" "org-roam menu"
    "C-c n l" "roam buffer toggle"
    "C-c n f" "roam find node"
    "C-c n p" "roam find project"
    "C-c n r" "roam find recipe"
    "C-c n t" "roam find topic"
    "C-c n i" "roam insert node"
    "C-c n w" "roam find webclip"
    "C-c n I" "roam insert immediate"
    "C-c n d" "roam dailies menu"))

(provide 'org-roam-config)
;;; org-roam-config.el ends here.
