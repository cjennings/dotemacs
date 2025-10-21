;;; org-agenda-config --- Org-Agenda/Todo Config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
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
;; KEYBINDINGS:
;; f8     - MAIN AGENDA which organizes all tasks and events into:
;;          - overdue tasks (past scheduled/deadline dates)
;;          - all unfinished priority A tasks
;;          - the 8-day schedule, including the habit consistency graph
;;          - all priority B tasks
;; C-u f8 - Force rebuild of agenda file list (ignores cache)
;;
;; C-f8     - TASK LIST containing all tasks from all agenda targets.
;; C-u C-f8 - Force rebuild of agenda file list before showing task list
;;
;; M-f8 - TASK LIST containing all tasks from just the current org-mode buffer.
;;
;; PERFORMANCE OPTIMIZATION:
;; The agenda file list is cached to avoid expensive directory scans on every
;; F8 press. The cache is automatically rebuilt every 5 minutes (configurable
;; via `cj/org-agenda-rebuild-interval'). Use C-u f8 to force an immediate
;; rebuild if you've added new todo.org files.
;;
;; NOTE:
;; Files that contain information relevant to the agenda will be found in the
;; following places: the schedule-file, gcal-file, and project todo.org files
;; found in projects-dir (searches up to 3 levels deep).

;;; Code:

(require 'seq) ;; for seq-filter to disable tramp-archive handlers

;; Forward declarations for user-constants variables
(eval-when-compile
  (defvar inbox-file)
  (defvar schedule-file)
  (defvar gcal-file)
  (defvar projects-dir))

;; Forward declarations for org-mode variables
(defvar org-agenda-files)
(defvar org-done-keywords)
(defvar org-lowest-priority)
(defvar org-agenda-dim-blocked-tasks)
(defvar org-agenda-use-tag-inheritance)
(defvar org-agenda-ignore-properties)
(defvar org-agenda-inhibit-startup)
(defvar org-agenda-include-diary)
(defvar org-agenda-skip-deadline-if-done)
(defvar org-agenda-skip-scheduled-if-done)
(defvar org-agenda-skip-scheduled-if-deadline-is-shown)
(defvar org-habit-show-habits-only-for-today)
(defvar org-habit-graph-column)
(defvar org-habit-show-graphs)
(defvar diary-file)

;; Forward declarations for org-mode functions
(declare-function org-end-of-subtree "org" (invisible-ok))
(declare-function org-get-todo-state "org" (&optional string))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-time-string-to-absolute "org" (s &optional daynr prefer buffer pos))
(declare-function org-get-priority "org" (s))
(declare-function org-agenda "org-agenda" (&optional arg keys restriction))
(declare-function org-end-of-line "org" (&optional n))
(declare-function org-agenda-todo-nextset "org-agenda" ())
(declare-function org-agenda-todo-previousset "org-agenda" ())
(declare-function thing-at-point "thingatpt" (thing &optional no-properties))

;; Forward declarations for seq (for filtering file-name-handler-alist)
(declare-function seq-filter "seq" (pred sequence))

;; Forward declarations for alert/org-alert variables
(defvar alert-fade-time)
(defvar alert-default-style)
(defvar org-alert-interval)
(defvar org-alert-notify-cutoff)
(defvar org-alert-notify-after-event-cutoff)
(defvar org-alert-notification-title)

;; ----------------------------- Configuration ---------------------------------

(defvar cj/org-agenda-rebuild-interval 300
  "Interval in seconds between automatic org-agenda-files rebuilds.
Set to nil to disable automatic rebuilding. Default is 300 seconds (5 minutes).")

;; ------------------------------ Cache Variables ------------------------------

(defvar cj/org-agenda-files-cache nil
  "Cached list of org-agenda files to avoid expensive directory scans.")

(defvar cj/org-agenda-rebuild-timer nil
  "Timer object for periodic org-agenda-files rebuilding.")

;; ========================= PERFORMANCE SETTINGS ==============================
;; Set these EARLY, before org-agenda loads, for maximum performance

;; CRITICAL: dim-blocked-tasks is VERY slow - checks all dependencies
(setopt org-agenda-dim-blocked-tasks nil)

;; Disable expensive features
(setopt org-agenda-use-tag-inheritance nil)          ;; don't inherit tags (much faster)
(setopt org-agenda-ignore-properties '(effort appt category))  ;; skip property lookups
(setopt org-agenda-inhibit-startup t)                ;; don't run startup hooks per file

;; CRITICAL: Disable diary integration - diary is VERY slow
;; This must be set BEFORE org-agenda loads AND in the config block
(setopt org-agenda-include-diary nil)
(setopt diary-file "/dev/null")  ;; Extra safety: point diary to /dev/null

;; Skip completed items (fewer items to process)
(setopt org-agenda-skip-deadline-if-done t)
(setopt org-agenda-skip-scheduled-if-done t)
(setopt org-agenda-skip-scheduled-if-deadline-is-shown t)

;; CRITICAL PERFORMANCE: Habit graphs are VERY expensive to render
;; Habit consistency graphs can add 5-10 seconds to agenda generation
;; Options:
;;   - org-habit-show-graphs nil = No graphs (FAST - habits still show)
;;   - org-habit-show-graphs t = Show graphs (SLOW but shows consistency)
;;   - org-habit-show-habits-only-for-today t = Graphs only for today (MEDIUM)

(setopt org-habit-show-habits nil)
(setopt org-habit-show-habits-only-for-today t)
(setopt org-habit-graph-column 50)

;; Display agenda from the bottom (must be set before org-agenda loads)
(with-eval-after-load 'org-agenda
  (add-to-list 'display-buffer-alist
               '("\\*Org Agenda\\*"
                 (display-buffer-reuse-mode-window display-buffer-below-selected)
                 (dedicated . t)
                 (window-height . fit-window-to-buffer))))

(use-package org-agenda
  :ensure nil ;; built-in
  :after org
  :config
  (require 'user-constants)
  (setopt org-agenda-prefix-format '((agenda   . " %i %-25:c%?-12t% s")
                                      (timeline . "  % s")
                                      (todo     . " %i %-25:c")
                                      (tags     . " %i %-12:c")
                                      (search   . " %i %-12:c")))

  (setopt org-agenda-remove-tags t)
  (setopt org-agenda-compact-blocks t)
  (setopt org-agenda-span 'day)  ;; show only today by default (override in custom commands)
  (setopt org-agenda-start-on-weekday nil)  ;; start on current day
  (setopt org-agenda-use-time-grid nil)  ;; disable time grid
  (setopt org-agenda-show-log nil)

  ;; Ensure diary stays disabled (set again in case something re-enabled it)
  (setopt org-agenda-include-diary nil)

  ;; reset s-left/right each time org-agenda is enabled
  (add-hook 'org-agenda-mode-hook #'cj/org-agenda-mode-keys))

;; ----------------------- Org Agenda Mode Keybindings ------------------------

(defun cj/org-agenda-mode-keys ()
  "Set up keybindings for org-agenda-mode."
  (local-set-key (kbd "s-<right>") #'org-agenda-todo-nextset)
  (local-set-key (kbd "s-<left>") #'org-agenda-todo-previousset))

;; ------------------------ Add Files To Org Agenda List -----------------------
;; finds files named 'todo.org' (case insensitive) and adds them to
;; org-agenda-files list.

(defun cj/find-todo-org-files (directory)
  "Find all todo.org files in DIRECTORY using fast wildcard matching.
Searches up to 3 levels deep for files named todo.org (case insensitive).
Returns a list of absolute file paths.

Note: Temporarily disables TRAMP archive file name handlers to prevent
file-expand-wildcards from trying to look inside .zip, .tar.gz, etc. files,
which would cause significant slowdown."
  (when (and directory (file-directory-p directory))
    (let ((dir (file-name-as-directory (expand-file-name directory)))
          (found-files '())
          ;; Temporarily disable tramp-archive to prevent looking inside archives
          (file-name-handler-alist
           (seq-filter (lambda (handler)
                         (not (eq (cdr handler) 'tramp-archive-file-name-handler)))
                       file-name-handler-alist)))
      ;; Search at multiple depth levels (faster than recursive search)
      ;; Level 0: todo.org directly in directory
      (setq found-files (append (file-expand-wildcards (concat dir "[Tt][Oo][Dd][Oo].[Oo][Rr][Gg]")) found-files))
      ;; Level 1: todo.org one level deep
      (setq found-files (append (file-expand-wildcards (concat dir "*/[Tt][Oo][Dd][Oo].[Oo][Rr][Gg]")) found-files))
      found-files)))

(defun cj/add-files-to-org-agenda-files-list (directory)
  "Search for files named `todo.org' and add them to `org-agenda-files'.
DIRECTORY is a string of the path to begin the search.
Modifies the global `org-agenda-files' variable as a side effect."
  (interactive "D")
  (setq org-agenda-files
        (append (cj/find-todo-org-files directory)
                org-agenda-files)))

;; ---------------------------- Rebuild Org Agenda ---------------------------
;; builds the org agenda list from all agenda targets.
;; agenda targets is the schedule, contacts, project todos, and inbox.org

(defun cj/build-org-agenda-list (&optional force)
  "Build the org agenda file list, using cache when possible.
With optional FORCE argument (or when called interactively with prefix arg),
ignores the cache and forces a full rebuild.
The file list is cached in `cj/org-agenda-files-cache' to avoid expensive
directory scans on every agenda display. The cache is automatically refreshed
every `cj/org-agenda-rebuild-interval' seconds.

Begins with inbox-file, schedule-file, and gcal-file.
Then adds all todo.org files from projects-dir.
Reports elapsed time in the messages buffer."
  (interactive "P")
  (if (and (not force) cj/org-agenda-files-cache)
      (progn
        ;; Use cached file list
        (setq org-agenda-files cj/org-agenda-files-cache)
        (message "Using cached org-agenda-files (%d files)" (length org-agenda-files)))
    ;; Force rebuild or no cache exists
    (let ((start-time (current-time)))
      ;; Reset org-agenda-files to core files
      (setq org-agenda-files (list inbox-file schedule-file gcal-file))

      ;; Add all todo.org files from projects
      (cj/add-files-to-org-agenda-files-list projects-dir)

      ;; Cache the result
      (setq cj/org-agenda-files-cache org-agenda-files)

      (message "Rebuilt org-agenda-files in %.3f sec (%d files)"
               (float-time (time-subtract (current-time) start-time))
               (length org-agenda-files)))))

;; ------------------------- Periodic Rebuild Timer ----------------------------

(defun cj/start-org-agenda-rebuild-timer ()
  "Start the periodic timer to rebuild org-agenda-files.
The timer interval is controlled by `cj/org-agenda-rebuild-interval'.
If the interval is nil, no periodic rebuilding occurs."
  (when cj/org-agenda-rebuild-timer
    (cancel-timer cj/org-agenda-rebuild-timer)
    (setq cj/org-agenda-rebuild-timer nil))
  (when cj/org-agenda-rebuild-interval
    (setq cj/org-agenda-rebuild-timer
          (run-with-timer cj/org-agenda-rebuild-interval
                          cj/org-agenda-rebuild-interval
                          (lambda () (cj/build-org-agenda-list t))))))

;; Initial build on startup (after 1 second idle) and start periodic timer
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 1 nil
                                 (lambda ()
                                   (cj/build-org-agenda-list t)
                                   (cj/start-org-agenda-rebuild-timer)))))

;;;###autoload
(defun cj/todo-list-all-agenda-files (&optional force-rebuild)
  "Display an `org-agenda' todo list from all agenda files.
With prefix argument FORCE-REBUILD, forces a complete rebuild of the
agenda file list instead of using the cache."
  (interactive "P")
  (cj/build-org-agenda-list force-rebuild)
  (org-agenda "a" "t"))

;;;###autoload (keymap-global-set "C-<f8>" #'cj/todo-list-all-agenda-files)

;; ------------------------- Agenda List Current Buffer ------------------------
;; an agenda listing tasks from just the current buffer.

;;;###autoload
(defun cj/todo-list-from-this-buffer ()
  "Displays an `org-agenda' todo list built from the current buffer.
If the current buffer isn't an org buffer, inform the user."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((org-agenda-files (list buffer-file-name)))
        (org-agenda "a" "t"))
    (message (format "Your org agenda request based on '%s' failed because it's not an org buffer."
                     (buffer-name)))))

;;;###autoload (keymap-global-set "M-<f8>" #'cj/todo-list-from-this-buffer)

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

;; Cache "today" to avoid recalculating for every heading
(defvar cj/org-agenda-today-absolute nil
  "Cached absolute day number for today, reset each agenda generation.")

(defun cj/org-agenda-reset-today-cache ()
  "Reset the cached `today' value before generating agenda."
  (setq cj/org-agenda-today-absolute
        (org-time-string-to-absolute (format-time-string "%Y-%m-%d"))))

;; Reset cache before each agenda generation
(add-hook 'org-agenda-mode-hook #'cj/org-agenda-reset-today-cache)

(defun cj/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
	  nil)))

(defun cj/org-agenda-skip-subtree-if-not-overdue ()
  "Skip an agenda subtree if it is not an overdue deadline or scheduled task.

An entry is considered overdue if it has a scheduled or deadline date strictly
before today, is not marked as done, and is not a habit.
Performance: Uses cached `today' value to avoid recalculation per heading."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (todo-state (org-get-todo-state)))
	(if (or (not todo-state) ; no todo keyword
			(member todo-state org-done-keywords)) ; done/completed tasks
		subtree-end  ; skip if done
	  ;; Only check dates for non-done tasks
	  (let* ((deadline (org-entry-get nil "DEADLINE"))
	         (scheduled (org-entry-get nil "SCHEDULED"))
	         (today (or cj/org-agenda-today-absolute
	                    (org-time-string-to-absolute (format-time-string "%Y-%m-%d"))))
	         (deadline-day (and deadline (org-time-string-to-absolute deadline)))
	         (scheduled-day (and scheduled (org-time-string-to-absolute scheduled)))
	         (overdue (or (and deadline-day (< deadline-day today))
	                      (and scheduled-day (< scheduled-day today)))))
		(if overdue
			nil  ; do not skip, keep this entry
		  subtree-end)))))  ; skip if not overdue

(defun cj/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        ;; org-get-priority returns value * 1000, so we must match that scale
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

(setopt org-agenda-custom-commands
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
                   (org-agenda-span 7)  ;; 7-day view
				   (org-agenda-start-on-weekday nil)
				   (org-agenda-include-diary nil)  ;; explicitly disable diary in this view
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


;;;###autoload
(defun cj/main-agenda-display (&optional force-rebuild)
  "Display the main daily org-agenda view.
With prefix argument FORCE-REBUILD, forces a complete rebuild of the
agenda file list instead of using the cache.
This uses all org-agenda targets and presents four sections:
- Overdue tasks (past scheduled/deadline dates)
- All unfinished priority A tasks
- Today's schedule (7-8 days), including habits
- All priority B unscheduled/undeadlined tasks

The agenda file list is cached and rebuilt automatically every
`cj/org-agenda-rebuild-interval' seconds (default 5 minutes).

Performance: Temporarily increases GC threshold during generation to reduce
GC pauses, then restores it after 10 seconds.

The agenda is built from all sources including:
- inbox-file, schedule-file, and gcal-file
- All todo.org files in projects-dir"
  (interactive "P")
  ;; Save current GC threshold and temporarily disable GC during agenda generation
  (let ((gc-cons-threshold-original gc-cons-threshold))
    (setq gc-cons-threshold most-positive-fixnum)

    ;; Generate the agenda
    (cj/build-org-agenda-list force-rebuild)
    (org-agenda "a" "d")

    ;; Restore GC threshold after 10 seconds (gives user time to view agenda)
    (run-at-time 10 nil
                 (lambda ()
                   (setq gc-cons-threshold gc-cons-threshold-original)
                   (garbage-collect)))))

;;;###autoload (keymap-global-set "<f8>" #'cj/main-agenda-display)

;; Set keybindings immediately when module loads
(keymap-global-set "<f8>" #'cj/main-agenda-display)
(keymap-global-set "C-<f8>" #'cj/todo-list-all-agenda-files)
(keymap-global-set "M-<f8>" #'cj/todo-list-from-this-buffer)

;; ------------------------- Add Timestamp To Org Entry ------------------------
;; simply adds a timestamp to put the org entry on an agenda

(defvar cj/timeformat "%Y-%m-%d %a"
  "Time format for org entry timestamps.")

(defun cj/add-timestamp-to-org-entry (s)
  "Add an event with time S to appear underneath the line-at-point.
This allows a line to show in an agenda without being scheduled or a deadline."
  (interactive "sTime: ")
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

(use-package org-alert
  :after alert org-agenda
  :commands (org-alert-enable org-alert-check)
  :bind
  ("C-c A" . org-alert-check)
  :config
  ;; Set org-alert settings
  (setq org-alert-interval 300) ;; seconds between agenda checks (5 minutes)
  (setq org-alert-notify-cutoff 10) ;; minutes before a deadline to notify
  (setq org-alert-notify-after-event-cutoff 5)  ;; stop alerts 5 mins after deadline
  (setq org-alert-notification-title "Reminder"))

;; Enable org-alert timer with message
(defun cj/org-alert-enable-with-message ()
  (org-alert-enable)
  (message "org-alert timer enabled with interval %d seconds" org-alert-interval))

;; Alert when idle post Emacs startup
(add-hook 'emacs-startup-hook
		  (lambda ()
			(run-with-idle-timer 1 nil #'cj/org-alert-enable-with-message)))


(provide 'org-agenda-config)
;;; org-agenda-config.el ends here
