;;; org-roam-config.el --- Org-Roam Config -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; Currently a work in progress. The initial version of this was taken from David Wilson:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

;;; Code:

;; ---------------------------------- Org Roam ---------------------------------

(use-package org-roam
  :after org
  :defer .5
  :custom
  (org-roam-directory roam-dir)
  (org-roam-dailies-directory journals-dir)
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M:%S %p %Z> %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+FILETAGS: Journal
#+TITLE: %<%Y-%m-%d>"))))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("v" "v2mom" plain
      (function (lambda () (concat roam-dir "templates/v2mom.org")))
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("r" "recipe" plain
      (function (lambda () (concat roam-dir "templates/recipe.org")))
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Recipe\n#+STARTUP: showall")
      :unnarrowed t)
     ("p" "project" plain
      (function (lambda () (concat roam-dir "templates/project.org")))
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Project")
      :unnarrowed t)
     ("t" "topic" plain
      (function (lambda () (concat roam-dir "templates/topic.org")))
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Topic")
      :unnarrowed t)))
  :bind (("C-c n ?" . org-roam-hydra/body)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n p" . cj/org-roam-find-node-project)
         ("C-c n r" . cj/org-roam-find-node-recipe)
         ("C-c n t" . cj/org-roam-find-node-topic)
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

(defun cj/org-roam-find-node (tag template-key template-file)
  "List all nodes of type \='TAG\=' in completing read for selection or creation.
Interactively find or create an Org-roam node with a given \='TAG\='. Newly
created nodes are added to the agenda and follow a template defined by
\='TEMPLATE-KEY\=' and \='TEMPLATE-FILE\='."

  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook
            #'cj/org-roam-add-node-to-agenda-files-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (cj/org-roam-filter-by-tag tag)
   nil
   :templates
   `((,template-key ,tag plain  (file ,template-file)
                    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                       ,(concat "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: " tag))
                    :unnarrowed t))))

(defun cj/org-roam-find-node-topic ()
  "List nodes of type \=`topic\=` in completing read for selection or creation."
  (interactive)
  (cj/org-roam-find-node "Topic" "t" (concat roam-dir "templates/topic.org")))

(defun cj/org-roam-find-node-recipe ()
  "List nodes of type \=`recipe\=` in completing read for selection or creation."
  (interactive)
  (cj/org-roam-find-node "Recipe" "r" (concat roam-dir "templates/recipe.org")))

(defun cj/org-roam-find-node-project ()
  "List nodes of type \='project\=' in completing read for selection or creation."

  (interactive)
  (cj/org-roam-find-node "Project" "p" (concat roam-dir "templates/project.org")))

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

;; -------------------------------- Capture Task -------------------------------

(defun cj/org-roam-capture-task-into-project ()
  "Create a new project and add a task immediately to it."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook
            #'cj/org-roam-add-node-to-agenda-files-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture-
   :node (org-roam-node-read
          nil
          (cj/org-roam-filter-by-tag "Project"))
   :templates '(("p" "project" plain "** TODO %?"
                 :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                        "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: Project"
                                        ("${title}"))))))
(global-set-key (kbd "C-c n t") #'cj/org-roam-capture-task-into-project)

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

(provide 'org-roam-config)
;;; org-roam-config.el ends here.
