;;; org-agenda-config --- Org-Agenda/Todo Config -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Agenda views are tied to the F8 (fate) key.

;; f8   - DAILY SCHEDULE containing a events with a scheduled date or deadline of
;;        the current day. This is followed by...
;;      - TASK LIST containing tasks from  all agenda sources.

;; C-f8 - TASK LIST containing all tasks from all agenda sources

;; M-f8 - TASK LIST containing all tasks from the current org-mode buffer.

;; NOTE:
;; Files that contain information relevant to the agenda will be found in the
;; following places: the schedule-file, org-roam notes tagged as 'Projects' and
;; project todo.org files found in project-dir and code-dir.

;; How the agenda is created:
;; The inbox and schedule files are always included first. However, in order to
;; stay current, the files containing agenda information are queried before
;; calling the functions in the section org-agenda functions to display the
;; data. This way, any newly created events from project todo.org files, or
;; org-roam Project files will be included.

;;; Code:

(with-eval-after-load 'org-roam

  ;; ----------------------------- Org TODO Settings ---------------------------

  (setq org-todo-keywords '((sequence "TODO(t)" "PROJECT(p)" "DOING(i)"
                                      "WAITING(w)" "VERIFY(v)" "STALLED(s)"
                                      "DELEGATED(x)" "|"
                                      "FAILED(f)" "DONE(d)" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO"      . "green")
          ("PROJECT"   . "blue")
          ("DOING"     . "yellow")
          ("WAITING"   . "white")
          ("VERIFY"    . "orange")
          ("STALLED"   . "light blue")
          ("DELEGATED" . "green")
          ("FAILED"    . "red")
          ("DONE"      . "dark grey")
          ("CANCELLED" . "dark grey")))

  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?D)
  (setq org-default-priority ?C)
  (setq org-priority-faces '((?A . (:foreground "Cyan" :weight bold))
                             (?B . (:foreground "Yellow"))
                             (?C . (:foreground "Green"))
                             (?D . (:foreground "Grey"))))

  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-deadline-warning-days 7)    ;; warn me w/in a week of deadlines
  (setq org-log-done nil)               ;; don't log when tasks was done

  ;; inherit parents properties (no schedules or deadlines)
  (setq org-use-property-inheritance t)

  ;; ------------------ Org TODO Next/Previous Set Keybindings -----------------

  (add-hook 'org-agenda-mode-hook (lambda ()
									(local-set-key (kbd "s-<right>")  #'org-agenda-todo-nextset)
									(local-set-key (kbd "s-<left>")  #'org-agenda-todo-previousset)))

  ;; ------------------------------ Org Super Agenda -----------------------------

  (use-package org-super-agenda
    :config (org-super-agenda-mode))

;;;; ORG AGENDA VIEW DEFINITIONS
  (defun cj/agenda-today-view()
    "This agenda from all tasks that are scheduled or have a deadline."
    (setq org-super-agenda-groups
          '((:log t)  ; Automatically named "Log"
            (:name "SCHEDULED AND DUE"
                   :time-grid t
                   :deadline past
                   :deadline today
                   :scheduled past
                   :scheduled today)
            (:habit t)
            (:name "DUE SOON"
                   :deadline future)
            (:name "LESS IMPORTANT"
                   :scheduled future
                   :order 100)
            (:discard (:anything t)))))

  (defun cj/agenda-all-view()
    "This agenda is built from all tasks."
    (setq org-super-agenda-groups
          '(
            (:name "Ready To Go"
                   :todo "STAGED"
                   :todo "READY"
                   :order 5)
            (:name "Due Today"
                   :deadline past
                   :deadline today
                   :order 2)
            (:name "Empty Projects"
                   :todo "PROJECT"
                   :order 85)
            (:name "Delegated"
                   :todo "DELEGATED"
                   :order 50)
            (:name "Scheduled Later"
                   :scheduled future
                   :order 75)
            (:name "Scheduled Today"
                   :scheduled today
                   :scheduled past
                   :order 4)
            (:name "In Progress"
                   :todo "DOING"
                   :order 7)
            (:name "High Priority"
                   :priority "A"
                   :order 10)
            (:name "Waiting"
                   :todo "WAITING"
                   :order 60)
            (:name "Upcoming"
                   :deadline future
                   :order 30)
            (:name "Next Priority"
                   :priority "B"
                   :order 70)
            (:name "Everything Else"
                   :anything t
                   :order 90))))

  ;; ------------------------------ Add Agenda Time ------------------------------

  (defun cj/add-agenda-time (s)
    "Add an event with time S to appear underneath the line-at-point.
This allows a line to show in an agenda without being scheduled or a deadline."
    (interactive "sTime: ")
    (defvar cj/timeformat "%Y-%m-%d %a")
    (org-end-of-line)
    (save-excursion
      (open-line 1)
      (forward-line 1)
      (insert (concat "<" (format-time-string cj/timeformat (current-time)) " " s ">" ))))

  (global-set-key (kbd "M-t")       #'cj/add-agenda-time)

  ;; ---------------------------- Org Agenda Settings ----------------------------

  (setq org-agenda-prefix-format '((agenda   . " %i %-25:c%?-12t% s")
                                   (timeline . "  % s")
								   (todo     . " %i %-25:c")
								   (tags     . " %i %-12:c")
								   (search   . " %i %-12:c")))
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-include-deadlines t)
  (setq org-agenda-remove-tags t)
  (setq org-agenda-compact-blocks t)

  ;; ------------------------ Add Files To Org Agenda List -----------------------
  ;; finds files named 'todo.org' (case insensitive) and adds them to
  ;; org-agenda-files list.

  (defun cj/add-files-to-org-agenda-files (directory)
	"Recursively searches for files named 'todo.org',
  Searches in DIRECTORY and adds them to org-project-files."
	(interactive "D")
	(setq org-agenda-files
		  (append (directory-files-recursively directory
											   "^[Tt][Oo][Dd][Oo]\\.[Oo][Rr][Gg]$" t)
				  org-agenda-files)))


  ;; NOTE: the following functions require org-roam functionality
  (with-eval-after-load 'org-roam-config

	;; ---------------------------- Rebuild Org Agenda ---------------------------

	(defun cj/build-org-agenda-list ()
	  "Rebuilds the org agenda list.
Begins with the inbox-file and schedule-file, then searches for org-roam
Projects and adds all todo.org files from code and project directories."
	  (interactive)
	  ;; reset org-agenda-files to inbox-file
	  (setq org-agenda-files (list inbox-file schedule-file))
	  (let ((new-files
			 (append
			  (cj/org-roam-list-notes-by-tag "Project"))))
		(dolist (file new-files)
		  (unless (member file org-agenda-files)
			(setq org-agenda-files (cons file org-agenda-files)))))

	  (cj/add-files-to-org-agenda-files projects-dir)
	  (cj/add-files-to-org-agenda-files code-dir))

	;; build org-agenda-list for the first time once emacs init is complete.
	(add-hook 'emacs-startup-hook 'cj/build-org-agenda-list)

    ;; ------------------------ Org Agenda Display Functions -----------------------

    (defun cj/agenda-all-agenda-files-day ()
      "Display an \'org-agenda\' schedule with tasks covering today.
The contents of the agenda will be built from org-project-files and org-roam
files that have project in their filetag."
      (interactive)
      (cj/build-org-agenda-list)
      (setq org-agenda-span 'day)
      (cj/agenda-today-view)
      (org-agenda "a" "a"))
    (global-set-key (kbd "<f8>")      #'cj/agenda-all-agenda-files-day)


    (defun cj/agenda-all-agenda-files-week ()
      "Display an 'org-agenda' schedule with tasks covering this week.
The contents of the agenda will be built from org-project-files and org-roam
files that have project in their filetag."
      (interactive)
      (cj/build-org-agenda-list)
      (setq org-agenda-span 'week)
      (cj/agenda-today-view)
      (org-agenda "a" "a"))
    (global-set-key (kbd "s-<f8>")    #'cj/agenda-all-agenda-files-week)

    (defun cj/todo-list-all-agenda-files ()
      "Displays an 'org-agenda' todo list.
The contents of the agenda will be built from org-project-files and org-roam
files that have project in their filetag."
      (interactive)
      (cj/build-org-agenda-list)
      (cj/agenda-all-view)
      (org-agenda "a" "t"))
    (global-set-key (kbd "C-<f8>")    #'cj/todo-list-all-agenda-files)

    (defun cj/todo-list-from-this-buffer ()
      "Displays an 'org-agenda' todo list built from the current buffer.
   If the current buffer isn't an org buffer, inform the user."
      (interactive)
      (if (eq major-mode 'org-mode)
          (let ((org-agenda-files (list buffer-file-name)))
            (cj/agenda-all-view)
            (org-agenda "a" "t"))
        (message (concat "Your org agenda request based on '" (buffer-name (current-buffer))
                         "' failed because it's not an org buffer."))))
    (global-set-key (kbd "M-<f8>")    #'cj/todo-list-from-this-buffer)

    ) ;; end with-eval-after-load 'org-roam-config
  ) ;; end with-eval-after-load 'org-roam

(provide 'org-agenda-config)
;;; org-agenda-config.el ends here
