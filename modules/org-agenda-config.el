;;; org-agenda-config --- Org-Agenda/Todo Config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Performance:
;; - Caches agenda file list to avoid scanning projects directory on every view
;; - Cache builds asynchronously 10 seconds after Emacs startup (non-blocking)
;; - First agenda view uses cache if ready, otherwise builds synchronously
;; - Subsequent views are instant (cached)
;; - Cache auto-refreshes after 1 hour
;; - Manual refresh: M-x cj/org-agenda-refresh-files (e.g., after adding projects)
;;
;; Agenda views are tied to the F8 (fate) key.
;;
;;  "We are what we repeatedly do.
;;   Excellence, then, is not an act, but a habit"
;;                        -- Aristotle
;;
;;  "...watch your actions, they become habits;
;;    watch your habits, they become character;
;;    watch your character, for it becomes your destiny."
;;                        -- Lao Tzu
;;
;;
;; f8   - MAIN AGENDA which organizes all tasks and events into:
;;        - all unfinished priority A tasks
;;        - the weekly schedule, including the habit consistency graph
;;        - all priority B tasks
;;
;; C-f8 - TASK LIST containing all tasks from all agenda targets.
;;
;; M-f8 - TASK LIST containing all tasks from just the current org-mode buffer.
;;
;; NOTE:
;; Files that contain information relevant to the agenda will be found in the
;; following places: the schedule-file, org-roam notes tagged as 'Projects' and
;; project todo.org files found in project-dir and code-dir.

;;; Code:
(require 'user-constants)
(require 'system-lib)

;; Load debug functions if enabled
(when (or (eq cj/debug-modules t)
          (memq 'org-agenda cj/debug-modules))
  (require 'org-agenda-config-debug
           (expand-file-name "org-agenda-config-debug.el"
                             (file-name-directory load-file-name))
           t))

(use-package org-agenda
  :ensure nil ;; built-in
  :after (org)
  :demand t
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
                                                   #'org-agenda-todo-previousset))))

;; ------------------------ Org Agenda File List Cache -------------------------
;; Cache agenda file list to avoid expensive directory scanning on every view

(defvar cj/org-agenda-files-cache nil
  "Cached agenda files list to avoid expensive directory scanning.
Set to nil to invalidate cache.")

(defvar cj/org-agenda-files-cache-time nil
  "Time when agenda files cache was last built.")

(defvar cj/org-agenda-files-cache-ttl 3600
  "Time-to-live for agenda files cache in seconds (default: 1 hour).")

(defvar cj/org-agenda-files-building nil
  "Non-nil when agenda files are being built asynchronously.
Prevents duplicate builds if user opens agenda before async build completes.")

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
;; builds the org agenda list from all agenda targets with caching.
;; agenda targets is the schedule, contacts, project todos,
;; inbox, and org roam projects.
(defun cj/build-org-agenda-list (&optional force-rebuild)
  "Build org-agenda-files list with caching.

When FORCE-REBUILD is non-nil, bypass cache and rebuild from scratch.
Otherwise, returns cached list if available and not expired.

This function scans projects-dir for todo.org files, so caching
improves performance from several seconds to instant."
  (interactive "P")
  ;; Check if we can use cache
  (let ((cache-valid (and cj/org-agenda-files-cache
                          cj/org-agenda-files-cache-time
                          (not force-rebuild)
                          (< (- (float-time) cj/org-agenda-files-cache-time)
                             cj/org-agenda-files-cache-ttl))))
    (if cache-valid
        ;; Use cached file list (instant)
        (progn
          (setq org-agenda-files cj/org-agenda-files-cache)
          ;; Always show cache-hit message (interactive or background)
          (cj/log-silently "Using cached agenda files (%d files)"
                           (length org-agenda-files)))
      ;; Check if async build is in progress
      (when cj/org-agenda-files-building
        (cj/log-silently "Waiting for background agenda build to complete..."))
      ;; Rebuild from scratch (slow - scans projects directory)
      (unwind-protect
          (progn
            (setq cj/org-agenda-files-building t)
            (let ((start-time (current-time)))
              ;; Reset org-agenda-files to base files
              (setq org-agenda-files (list inbox-file schedule-file gcal-file pcal-file))

              ;; Check all projects for scheduled tasks
              (cj/add-files-to-org-agenda-files-list projects-dir)

              ;; Update cache
              (setq cj/org-agenda-files-cache org-agenda-files)
              (setq cj/org-agenda-files-cache-time (float-time))

              ;; Always show completion message (interactive or background)
              (cj/log-silently "Built agenda files: %d files in %.3f sec"
                               (length org-agenda-files)
                               (- (float-time) (float-time start-time)))))
        ;; Always clear the building flag, even if build fails
        (setq cj/org-agenda-files-building nil)))))

;; Build cache asynchronously after startup to avoid blocking Emacs
(run-with-idle-timer
 10  ; Wait 10 seconds after Emacs is idle
 nil ; Don't repeat
 (lambda ()
   (cj/log-silently "Building org-agenda files cache in background...")
   (cj/build-org-agenda-list)))

(defun cj/org-agenda-refresh-files ()
  "Force rebuild of agenda files cache.

Use this after adding new projects or todo.org files.
Bypasses cache and scans directories from scratch."
  (interactive)
  (cj/build-org-agenda-list 'force-rebuild))

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

(defvar cj/main-agenda-hipri-title "HIGH PRIORITY UNRESOLVED TASKS"
  "String to announce the high priority section of the main agenda.")

(defvar cj/main-agenda-overdue-title "OVERDUE"
  "String to announce the overdue section of the main agenda.")

(defvar cj/main-agenda-schedule-title "SCHEDULE"
  "String to announce the schedule section of the main agenda.")

(defvar cj/main-agenda-tasks-title "PRIORITY B"
  "String to announce the schedule section of the main agenda.")

(defun cj/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
	  nil)))

(defun cj/org-agenda-skip-subtree-if-not-overdue ()
  "Skip an agenda subtree if it is not an overdue deadline or scheduled task.
An entry is considered overdue if it has a scheduled or deadline date strictly
before today, is not marked as done, and is not a habit."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (todo-state (org-get-todo-state))
		 (style (org-entry-get nil "STYLE"))
		 (deadline (org-entry-get nil "DEADLINE"))
		 (scheduled (org-entry-get nil "SCHEDULED"))
		 (today (org-time-string-to-absolute (format-time-string "%Y-%m-%d")))
		 (deadline-day (and deadline (org-time-string-to-absolute deadline)))
		 (scheduled-day (and scheduled (org-time-string-to-absolute scheduled))))
	(if (or (not todo-state) ; no todo keyword
			(member todo-state org-done-keywords) ; done/completed tasks
			(string= style "habit"))
		subtree-end  ; skip if done or habit
	  (let ((overdue (or (and deadline-day (< deadline-day today))
						 (and scheduled-day (< scheduled-day today)))))
		(if overdue
			nil  ; do not skip, keep this entry
		  subtree-end)))))  ; skip if not overdue

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
		 ((alltodo ""
				   ((org-agenda-skip-function #'cj/org-agenda-skip-subtree-if-not-overdue)
					(org-agenda-overriding-header cj/main-agenda-overdue-title)
					(org-agenda-prefix-format "  %i %-15:c%?-15t% s")))
		  (tags "PRIORITY=\"A\""
				((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				 (org-agenda-overriding-header cj/main-agenda-hipri-title)
				 (org-agenda-prefix-format "  %i %-15:c%?-15t% s")))
		  (agenda ""
				  ((org-agenda-start-day "0d")
				   (org-agenda-span 8)
				   (org-agenda-start-on-weekday nil)
				   (org-agenda-overriding-header cj/main-agenda-schedule-title)
				   (org-agenda-prefix-format "  %i %-15:c%?-15t% s")))
		  (alltodo ""
				   ((org-agenda-skip-function '(or (cj/org-skip-subtree-if-habit)
												   (cj/org-skip-subtree-if-priority ?A)
												   (cj/org-skip-subtree-if-priority ?C)
												   (cj/org-skip-subtree-if-priority ?D)
												   (cj/org-skip-subtree-if-keyword '("PROJECT"))
												   (org-agenda-skip-if nil '(scheduled deadline))))
					(org-agenda-overriding-header cj/main-agenda-tasks-title)
					(org-agenda-prefix-format "  %i %-15:c%?-15t% s"))))
		 ((org-agenda-compact-blocks nil)))))


(defun cj/main-agenda-display ()
  "Display the main daily org-agenda view.
This uses all org-agenda targets and presents three sections:
- All unfinished priority A tasks
- Today's schedule, including habits with consistency graphs
- All priority B and C unscheduled/undeadlined tasks
The agenda is rebuilt from all sources before display, including:
- inbox-file and schedule-file
- Org-roam nodes tagged as \"Project\"
- All todo.org files in projects-dir and code-dir"
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

;; --------------------------- Notifications / Alerts --------------------------
;; send libnotify notifications for agenda items

(use-package alert
  :config
  (setq alert-fade-time 10) ;; seconds to vanish alert
  (setq alert-default-style 'libnotify)) ;; works well with dunst

;; Install CHIME from GitHub using use-package :vc (Emacs 29+)
(use-package chime
  :demand t
  ;; :vc (:url "https://github.com/cjennings/chime.el" :rev :newest) ;; using latest on github
  :after alert  ; Removed org-agenda - chime.el requires it internally
  :ensure nil ;; using local version
  :load-path "~/code/chime.el"
  :init
  ;; Initialize org-agenda-files with base files before chime loads
  ;; The full list will be built asynchronously later
  (setq org-agenda-files (list inbox-file schedule-file gcal-file pcal-file))

  ;; Debug mode (keep set to nil, but available for troubleshooting)
  (setq chime-debug nil)
  :bind
  ("C-c A" . chime-check)
  :config
  ;; Polling interval: check every minute
  (setq chime-check-interval 60)

  ;; Alert intervals: 5 minutes before and at event time
  ;; All notifications use medium urgency
  (setq chime-alert-intervals '((5 . medium) (0 . medium)))

  ;; Day-wide events: notify at 9 AM for birthdays/all-day events
  (setq chime-day-wide-time "09:00")

  ;; Modeline display: show upcoming events within 6 hours
  (setq chime-modeline-lookahead-minutes (* 6 60))

  ;; Tooltip settings: show up to 10 upcoming events within 6 days
  (setq chime-modeline-tooltip-max-events 20)
  (setq chime-tooltip-lookahead-hours (* 7 24))

  ;; Modeline content: show title and countdown only (omit event time)
  (setq chime-notification-text-format "%t %u")

  ;; Time-until format: compact style like " in 10m" or " in 1h 37m"
  (setq chime-time-left-format-short " in %mm ")      ; Under 1 hour: " in 10m"
  (setq chime-time-left-format-long " in %hh %mm ")   ; 1 hour+: " in 1h 37m"
  (setq chime-time-left-format-at-event "now")

  ;; Title truncation: limit long event titles to 25 characters
  (setq chime-max-title-length 25)

  ;; Notification title
  (setq chime-notification-title "Reminder")

  ;; Calendar URL
  (setq chime-calendar-url "https://calendar.google.com/calendar/u/0/r")

  ;; Enable chime-mode
  (chime-mode 1))

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c A" "chime check"))

(provide 'org-agenda-config)
;;; org-agenda-config.el ends here
