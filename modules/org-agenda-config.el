;;; org-agenda-config --- Org-Agenda/Todo Config -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Agenda views are tied to the F8 (fate) key.

;; f8   - MAIN AGENDA which organizes all tasks and events into:
;;        - all unfinished priority A tasks
;;        - the weekly schedule, including the habit consistency graph
;;        - all priority B and C tasks

;; C-f8 - TASK LIST containing all tasks from all agenda targets.

;; M-f8 - TASK LIST containing all tasks from just the current org-mode buffer.

;; NOTE:
;; Files that contain information relevant to the agenda will be found in the
;; following places: the schedule-file, org-roam notes tagged as 'Projects' and
;; project todo.org files found in project-dir and code-dir.

;;; Code:

(use-package org-agenda
  :ensure nil ;; built-in
  :after (org org-roam)
  :config
  (setq org-agenda-prefix-format '((agenda   . " %i %-25:c%?-12t% s")
                                   (timeline . "  % s")
                                   (todo     . " %i %-25:c")
                                   (tags     . " %i %-12:c")
                                   (search   . " %i %-12:c")))
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-remove-tags t)
  (setq org-agenda-compact-blocks t)

  ;; display the agenda from the bottom
  (add-to-list 'display-buffer-alist
               '("\\*Org Agenda\\*"
                 (display-buffer-reuse-mode-window display-buffer-below-selected)
                 (dedicated . t)
                 (window-height . fit-window-to-buffer)))

  ;; reset s-left/right each time org-agenda is enabled
  (add-hook 'org-agenda-mode-hook (lambda ()
                                    (local-set-key (kbd "s-<right>") #'org-agenda-todo-nextset)
                                    (local-set-key (kbd "s-<left>")
                                                   #'org-agenda-todo-previousset)))

  ;; build org-agenda-list for the first time after emacs init completes.
  (add-hook 'emacs-startup-hook #'cj/build-org-agenda-list))


;; ------------------------ Add Files To Org Agenda List -----------------------
;; finds files named 'todo.org' (case insensitive) and adds them to
;; org-agenda-files list.

(defun cj/add-files-to-org-agenda-files-list (directory)
  "Search for files named \\='todo.org\\=' add them to org-project-files.
DIRECTORY is a string of the path to begin the search."
  (interactive "D")
  (setq org-agenda-files
        (append (directory-files-recursively directory
                                             "^[Tt][Oo][Dd][Oo]\\.[Oo][Rr][Gg]$" t)
                org-agenda-files)))

;; ---------------------------- Rebuild Org Agenda ---------------------------
;; builds the org agenda list from all agenda targets.
;; agenda targets is the schedule, project todos, inbox, and org roam projects.

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

  (cj/add-files-to-org-agenda-files-list projects-dir)
  (cj/add-files-to-org-agenda-files-list code-dir))

;; ------------------------------ Agenda List All ------------------------------
;; an agenda listing tasks from all available agenda targets.

(defun cj/todo-list-all-agenda-files ()
  "Displays an \\='org-agenda\\=' todo list.
The contents of the agenda will be built from org-project-files and org-roam
files that have project in their filetag."
  (interactive)
  (cj/build-org-agenda-list)
  (org-agenda "a" "t"))
(global-set-key (kbd "C-<f8>") #'cj/todo-list-all-agenda-files)

;; ------------------------- Agenda List Current Buffer ------------------------
;; an agenda listing tasks from just the current buffer.

(defun cj/todo-list-from-this-buffer ()
  "Displays an \\='org-agenda\\=' todo list built from the current buffer.
If the current buffer isn't an org buffer, inform the user."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((org-agenda-files (list buffer-file-name)))
        (org-agenda "a" "t"))
    (message (concat "Your org agenda request based on '" (buffer-name (current-buffer))
                     "' failed because it's not an org buffer."))))
(global-set-key (kbd "M-<f8>")  #'cj/todo-list-from-this-buffer)

;; -------------------------------- Main Agenda --------------------------------
;; my custom agenda command from all available agenda targets. adapted from:
;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html

(defvar cj/main-agenda-hipri-title "\nHIGH PRIORITY UNRESOLVED TASKS\n"
  "String to announce the high priority section of the main agenda.")

(defvar cj/main-agenda-schedule-title "\nSCHEDULE\n"
  "String to announce the schedule section of the main agenda.")

(defvar cj/main-agenda-tasks-title "\nPRIORITY B AND C\n"
  "String to announce the schedule section of the main agenda.")

(defun cj/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun cj/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun cj/org-skip-subtree-if-keyword (keywords)
  "Skip an agenda subtree if it has a TODO keyword in KEYWORDS.
KEYWORDS must be a list of strings."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (member (org-get-todo-state) keywords)
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("d" "Daily Agenda with Tasks"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header cj/main-agenda-hipri-title)))
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-overriding-header cj/main-agenda-schedule-title)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (cj/org-skip-subtree-if-habit)
                                                   (cj/org-skip-subtree-if-priority ?A)
                                                   (cj/org-skip-subtree-if-priority ?D)
                                                   (cj/org-skip-subtree-if-keyword '("PROJECT"))
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header cj/main-agenda-tasks-title))))
		 ((org-agenda-compact-blocks nil)))))


	(defun cj/main-agenda-display ()
	  "Display the main \'org-agenda\' display.
This uses all org-agenda targes and presents three sections:
- all unfinished priority A tasks
- the weekly schedule, including the habit consistency graph
- all priority B and C tasks"
	  (interactive)
	  (cj/build-org-agenda-list)
	  (org-agenda "a" "d"))
(global-set-key (kbd "<f8>") #'cj/main-agenda-display)

;; ------------------------- Add Timestamp To Org Entry ------------------------
;; simply adds a timestamp to put the org entry on an agenda

(defun cj/add-timestamp-to-org-entry (s)
  "Add an event with time S to appear underneath the line-at-point.
This allows a line to show in an agenda without being scheduled or a deadline."
  (interactive "sTime: ")
  (defvar cj/timeformat "%Y-%m-%d %a")
  (org-end-of-line)
  (save-excursion
    (open-line 1)
    (forward-line 1)
    (insert (concat "<" (format-time-string cj/timeformat (current-time)) " " s ">" ))))
;;(global-set-key (kbd "M-t")       #'cj/add-timestamp-to-org-entry)

;; --------------------------- Notifications / Alerts --------------------------
;; send libnotify notifications about agenda items

(use-package alert
  :defer .5
  :after org-agenda
  :config
  (setq alert-fade-time 10) ;; secs to vanish alert
  (setq alert-default-style 'libnotify)) ;; work with dunst

(use-package org-alert
  :defer .5
  :after alert
  :bind
  ("C-c A" . org-alert-check)
  :config
  (setq alert-default-style 'libnotify) ;; work with dunst
  (setq org-alert-interval 180) ;; seconds between agenda checks (180 = 3 mins)
  (setq org-alert-notify-cutoff 5) ;; minutes before a deadline to send alert
  (setq org-alert-notify-after-event-cutoff 10) ;; mins post deadline to stop alerts
  (setq org-alert-notification-title "Reminder")
  (org-alert-enable))

(provide 'org-agenda-config)
;;; org-agenda-config.el ends here
