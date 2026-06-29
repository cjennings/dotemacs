;;; org-agenda-config.el --- Org-Agenda/Todo Config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/S.
;; Load shape: eager.
;; Eager reason: agenda should be available in the first session.
;; Top-level side effects: agenda hooks plus guarded idle cache build.
;; Runtime requires: user-constants, system-lib, cj-cache-lib.
;; Direct test load: yes.
;;
;; Org agenda configuration for global, project-scoped, and buffer-scoped task
;; views. F8 opens the main agenda; modified F8 bindings narrow by project,
;; current buffer, or task list.
;;
;; Agenda files come from inbox, schedule files, synced calendars, and immediate
;; project todo.org files. The file list is cached and rebuilt asynchronously to
;; keep normal agenda opens fast.

;;; Code:
(require 'user-constants)
(require 'system-lib)
(require 'cj-cache-lib)

(defcustom cj/org-agenda-window-height 0.75
  "Fraction of the selected frame used for the org agenda window."
  :type 'number
  :group 'org-agenda)

(defun cj/--org-agenda-display-rule ()
  "Return the display-buffer rule for the org agenda buffer."
  `("\\*Org Agenda\\*"
    (display-buffer-reuse-mode-window display-buffer-below-selected)
    (dedicated . t)
    (window-height . ,cj/org-agenda-window-height)))

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
  ;; Backstop against a non-existent agenda file (e.g. a calendar not yet synced
  ;; on a fresh machine): skip it silently instead of prompting to create it --
  ;; the interactive-prompt class that once hung the chime daemon.
  ;; `cj/--org-agenda-base-files' already filters the list; this catches any path
  ;; that reaches `org-agenda-files' another way.
  (setq org-agenda-skip-unavailable-files t)

  ;; display the agenda from the bottom
  (add-to-list 'display-buffer-alist
               (cj/--org-agenda-display-rule))

  ;; reset s-left/right each time org-agenda is enabled
  (add-hook 'org-agenda-mode-hook (lambda ()
                                    (local-set-key (kbd "s-<right>") #'org-agenda-todo-nextset)
                                    (local-set-key (kbd "s-<left>")
                                                   #'org-agenda-todo-previousset))))

;; ----------------------- Project-name Category Override ---------------------
;; The default `org-category' for a todo.org buffer is "todo" (the filename
;; without extension), which renders as "todo:" in every agenda `%c' column
;; and tells the reader nothing useful when every project has its own
;; todo.org.  Substitute the parent-directory basename instead, so
;; `~/.emacs.d/todo.org' shows "emacs.d:" and `~/projects/foo/todo.org' shows
;; "foo:".  Files that aren't named todo.org are left alone, and a user-set
;; `#+CATEGORY:' (which leaves `org-category' at a non-default value) wins.

(defun cj/--org-todo-category-from-file (path)
  "Return the project category for a todo.org PATH, or nil if not applicable.
For a file named todo.org, returns the basename of its parent
directory with a single leading dot stripped (so `~/.emacs.d/todo.org'
yields \"emacs.d\", not \".emacs.d\").  For any other file -- or for a
PATH that is nil, empty, or has no usable parent directory -- returns
nil so the org default category applies."
  (when (and (stringp path)
             (not (string-empty-p path))
             (string= "todo.org" (file-name-nondirectory path)))
    (let* ((dir (file-name-directory path))
           (parent (and dir
                        (file-name-nondirectory
                         (directory-file-name dir))))
           (clean (and parent
                       (if (and (> (length parent) 1)
                                (eq ?. (aref parent 0)))
                           (substring parent 1)
                         parent))))
      (and clean (not (string-empty-p clean)) clean))))

(defun cj/--org-set-todo-category ()
  "Set buffer-local `org-category' to the project name for a todo.org buffer.
Runs from `org-mode-hook'.  Only overrides when `org-category' is still
the default-from-filename (\"todo\"), so an explicit `#+CATEGORY:' in
the file keeps precedence."
  (when (and buffer-file-name
             (boundp 'org-category)
             (stringp org-category)
             (string= "todo" org-category))
    (when-let* ((project (cj/--org-todo-category-from-file buffer-file-name)))
      (setq-local org-category project))))

(add-hook 'org-mode-hook #'cj/--org-set-todo-category)

;; ------------------------ Org Agenda File List Cache -------------------------
;; Cache agenda file list to avoid expensive directory scanning on every view.
;; The TTL+building cache lifecycle is provided by `cj-cache.el'.

(defvar cj/--org-agenda-files-cache (cj/cache-make :ttl 3600)
  "Cache state for the agenda files list.  See `cj-cache.el'.")

;; ------------------------ Add Files To Org Agenda List -----------------------
;; Checks immediate subdirectories of DIRECTORY for todo.org files and adds
;; them to org-agenda-files. Does NOT recurse into nested subdirectories.

(defun cj/add-files-to-org-agenda-files-list (directory)
  "Add todo.org files from immediate subdirectories of DIRECTORY.
Only checks DIRECTORY/*/todo.org — does not recurse deeper."
  (interactive "D")
  (if (not (and (file-directory-p directory) (file-readable-p directory)))
      ;; Non-fatal: a missing or unreadable project root shouldn't crash the
      ;; whole agenda build — surface it and carry on with the other files.
      (display-warning
       'org-agenda
       (format "Agenda scan: project directory missing or unreadable, skipped: %s"
               directory)
       :warning)
    (let ((todo-files
           (seq-filter
            #'file-exists-p
            (mapcar (lambda (dir) (expand-file-name "todo.org" dir))
                    (seq-filter #'file-directory-p
                                (directory-files directory t "^[^.]"))))))
      (setq org-agenda-files (append todo-files org-agenda-files)))))

;; ---------------------------- Rebuild Org Agenda ---------------------------
;; builds the org agenda list from all agenda targets with caching.
;; agenda targets are the inbox, the schedule, the synced calendars,
;; and the per-project todo.org files under projects-dir.
(defun cj/--org-agenda-base-files ()
  "Return the existing base files for the agenda: inbox, schedule, and calendars.
The single source of the base list shared by the agenda builders and the chime
initializer, so adding a calendar source is a one-place change.  Per-project
todo.org files are layered on separately.  Files that do not exist are dropped
\(a fresh machine may lack the synced calendars or the inbox) so org-agenda
never prompts to create them -- the interactive-prompt class that once hung the
chime daemon; `org-agenda-skip-unavailable-files' is the backstop."
  (seq-filter #'file-exists-p
              (list inbox-file schedule-file gcal-file pcal-file dcal-file)))

(defun cj/--org-agenda-scan-files ()
  "Scan disk for the agenda files list.  Pure-ish: no caching, no logging.
Returns the list to assign to `org-agenda-files'.  Slow -- walks
`projects-dir' for per-project todo.org files."
  (let ((files (cj/--org-agenda-base-files)))
    ;; cj/add-files-to-org-agenda-files-list mutates org-agenda-files; let-bind
    ;; it for the duration of the helper, then return whatever it produced.
    (let ((org-agenda-files files))
      (cj/add-files-to-org-agenda-files-list projects-dir)
      org-agenda-files)))

(defun cj/build-org-agenda-list (&optional force-rebuild)
  "Build org-agenda-files list with caching.

When FORCE-REBUILD is non-nil, bypass cache and rebuild from scratch.
Otherwise, returns cached list if available and not expired.

This function scans projects-dir for todo.org files, so caching
improves performance from several seconds to instant."
  (interactive "P")
  (when (cj/cache-building-p cj/--org-agenda-files-cache)
    (cj/log-silently "Waiting for background agenda build to complete..."))
  (let* ((start-time (current-time))
         (files
          (cj/cache-value-or-rebuild
           cj/--org-agenda-files-cache
           #'cj/--org-agenda-scan-files
           :force-rebuild force-rebuild
           :on-hit (lambda (v)
                     (cj/log-silently "Using cached agenda files (%d files)"
                                      (length v)))
           :on-build-success
           (lambda (v)
             (cj/log-silently "Built agenda files: %d files in %.3f sec"
                              (length v)
                              (- (float-time) (float-time start-time)))))))
    (setq org-agenda-files files)))

;; Build cache asynchronously after startup to avoid blocking Emacs.
(unless noninteractive
  (run-with-idle-timer
   10  ; Wait 10 seconds after Emacs is idle
   nil ; Don't repeat
   (lambda ()
     (cj/log-silently "Building org-agenda files cache in background...")
     (cj/build-org-agenda-list))))

(defun cj/org-agenda-refresh-files ()
  "Force rebuild of agenda files cache.

Use this after adding new projects or todo.org files.
Bypasses cache and scans directories from scratch."
  (interactive)
  (cj/build-org-agenda-list 'force-rebuild))

(defun cj/todo-list-all-agenda-files ()
  "Displays an \\='org-agenda\\=' todo list.
The contents of the agenda are built from the base files (inbox, schedule, and
the synced calendars) plus the per-project todo.org files under projects-dir."
  (interactive)
  (cj/build-org-agenda-list)
  (org-agenda "a" "t"))
(global-set-key (kbd "s-<f8>") #'cj/todo-list-all-agenda-files)

;; ----------------------- Agenda List Single Project --------------------------
;; an agenda showing the main daily view filtered to a single project.

(defun cj/todo-list-single-project ()
  "Display the main agenda filtered to a single project.
Prompts for a project from ~/projects/ (only those containing todo.org),
then shows the daily agenda (overdue, high-pri, schedule, priority B)
scoped to that project's todo.org plus calendars, schedule, and inbox."
  (interactive)
  (let* ((all-dirs (directory-files projects-dir t "^[^.]"))
         (project-dirs (seq-filter
                        (lambda (dir)
                          (and (file-directory-p dir)
                               (file-exists-p (expand-file-name "todo.org" dir))))
                        all-dirs))
         (project-names (mapcar #'file-name-nondirectory project-dirs))
         (chosen (completing-read "Show agenda for project: " project-names nil t))
         (todo-file (expand-file-name "todo.org"
                                      (expand-file-name chosen projects-dir)))
         (org-agenda-files (cons todo-file (cj/--org-agenda-base-files))))
    (org-agenda "a" "d")))
(global-set-key (kbd "C-<f8>") #'cj/todo-list-single-project)

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

(defvar cj/main-agenda-schedule-title "SCHEDULE"
  "String to announce the schedule section of the main agenda.")

(defvar cj/main-agenda-tasks-title "PRIORITY B"
  "String to announce the schedule section of the main agenda.")

(defvar cj/main-agenda-verify-title "VERIFICATION"
  "String to announce the VERIFY section of the main agenda.
Lists tasks in the VERIFY TODO state waiting on a manual check.
Block sits above the day's schedule.")

(defvar cj/main-agenda-doing-title "IN-PROGRESS"
  "String to announce the DOING section of the main agenda.
Lists tasks in the DOING TODO state -- work actively in flight.
Block sits just under the day's schedule.")

(defvar cj/--main-agenda-prefix-format "  %i %-15:c%?-15t% s"
  "Prefix format string shared by all blocks of the main daily agenda.
Inlined across the overdue / high-priority / VERIFICATION / schedule /
IN-PROGRESS / priority-B blocks of `org-agenda-custom-commands' before
the extraction.  Keep the six blocks pointing here so a format tweak
lands in one place.")

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
		 ((agenda ""
				  ((org-agenda-start-day "0d")
				   (org-agenda-span 8)
				   (org-agenda-start-on-weekday nil)
				   ;; CANCELLED entries with a SCHEDULED date shouldn't appear
				   ;; in the forward-looking schedule -- they're dead weight.
				   (org-agenda-skip-function
					'(org-agenda-skip-entry-if 'todo '("CANCELLED")))
				   (org-agenda-overriding-header cj/main-agenda-schedule-title)
				   (org-agenda-prefix-format cj/--main-agenda-prefix-format)))
		  (tags "PRIORITY=\"A\""
				((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				 (org-agenda-overriding-header cj/main-agenda-hipri-title)
				 (org-agenda-prefix-format cj/--main-agenda-prefix-format)))
		  (todo "VERIFY"
				((org-agenda-skip-function 'cj/org-skip-subtree-if-habit)
				 (org-agenda-overriding-header cj/main-agenda-verify-title)
				 (org-agenda-prefix-format cj/--main-agenda-prefix-format)))
		  (todo "DOING"
				((org-agenda-skip-function 'cj/org-skip-subtree-if-habit)
				 (org-agenda-overriding-header cj/main-agenda-doing-title)
				 (org-agenda-prefix-format cj/--main-agenda-prefix-format)))
		  (alltodo ""
				   ((org-agenda-skip-function '(or (cj/org-skip-subtree-if-habit)
												   (cj/org-skip-subtree-if-priority ?A)
												   (cj/org-skip-subtree-if-priority ?C)
												   (cj/org-skip-subtree-if-priority ?D)
												   (cj/org-skip-subtree-if-keyword '("PROJECT"))
												   (org-agenda-skip-if nil '(scheduled deadline))))
					(org-agenda-overriding-header cj/main-agenda-tasks-title)
					(org-agenda-prefix-format cj/--main-agenda-prefix-format))))
		 ((org-agenda-compact-blocks nil)))))


(defun cj/main-agenda-display ()
  "Display the main daily org-agenda view.
This uses all org-agenda targets and presents three sections:
- All unfinished priority A tasks
- Today's schedule, including habits with consistency graphs
- All priority B and C unscheduled/undeadlined tasks
The agenda is rebuilt from all sources before display, including:
- inbox-file, schedule-file, and the synced calendars
- All todo.org files in immediate subdirectories of projects-dir"
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
  ;; Batch tests load this module without package-initialize, so optional
  ;; notification packages may be installed but not loadable yet.
  :if (or (not noninteractive)
          (require 'alert nil t))
  :config
  (setq alert-fade-time 10) ;; seconds to vanish alert
  (setq alert-default-style 'libnotify)) ;; works well with dunst

(use-package chime
  :vc (:url "git@cjennings.net:chime.git"
       :branch "main"
       :rev :newest)
  ;; :load-path "~/code/chime"  ;; uncomment + comment :vc above for local dev
  :demand t
  :after alert  ; Removed org-agenda - Chime requires it internally
  :init
  ;; Initialize org-agenda-files with base files before chime loads
  ;; The full list will be built asynchronously later
  (setq org-agenda-files (cj/--org-agenda-base-files))

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
  (setq chime-day-wide-alert-times '("09:00"))

  ;; Modeline display: show upcoming events within 6 hours
  (setq chime-modeline-lookahead-minutes (* 6 60))

  ;; Tooltip settings: show up to 20 upcoming events within the next 3 days
  (setq chime-modeline-tooltip-max-events 20)
  (setq chime-tooltip-lookahead-hours (* 3 24))  ;; today, tomorrow, and the next

  ;; Modeline content: show title and countdown only (omit event time)
  (setq chime-notification-text-format "%t %u")

  ;; Time-until format: compact style like " in 10m" or " in 1h 37m"
  (setq chime-time-left-formats
        '((at-event . "now")
          (short    . " in %mm ")     ; Under 1 hour: " in 10m"
          (long     . " in %hh %mm ")))  ; 1 hour+: " in 1h 37m"

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
