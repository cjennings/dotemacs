;;; calendar-sync.el --- Multi-calendar sync via .ics  -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-16

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/S.
;; Load shape: eager only when calendar-sync.local.el configures calendars.
;; Eager reason: daily agenda workflow; timers and network fetches are guarded.
;; Top-level side effects: defines C-; g map; starts sync only when configured.
;; Runtime requires: cl-lib, subr-x, system-lib, cj-org-text-lib, keybindings,
;;   calendar-sync-ics, calendar-sync-recurrence, calendar-sync-org,
;;   calendar-sync-source.
;; Direct test load: yes.
;;
;; One-way calendar synchronization from configured .ics/API sources into Org
;; files. Feed URLs may be inline or resolved from auth-source via :secret-host.
;;
;; This is the public face of the module: it owns configuration, the parse
;; pipeline orchestrator, the sync dispatch, the user commands, the timer, and
;; the C-; g keymap.  The parsing, recurrence expansion, Org rendering, and
;; fetch/worker code live in the calendar-sync-ics / -recurrence / -org /
;; -source layers, which this module requires.  Every public name is unchanged
;; so existing (require 'calendar-sync) callers and tests keep working.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'system-lib)  ;; provides cj/auth-source-secret-value (leaf; no ai-config dep)
(require 'cj-org-text-lib)
(require 'keybindings)  ;; provides cj/custom-keymap
(require 'calendar-sync-ics)
(require 'calendar-sync-recurrence)
(require 'calendar-sync-org)
(require 'calendar-sync-source)

;;; Configuration

(defgroup calendar-sync nil
  "One-way calendar synchronization to Org files."
  :group 'calendar)

(defvar calendar-sync-calendars nil
  "List of calendars to sync.
Each calendar is a plist.  Common keys:
  :name    - Display name for the calendar (used in logs and prompts)
  :file    - Output file path for org format
  :fetcher - Fetch path: \\='ics (default) or \\='api

For the default \\='ics fetcher (Proton, plain .ics feeds), give the feed
URL one of two ways:
  :url         - the feed URL inline (plaintext in this file)
  :secret-host - an auth-source host whose secret holds the feed URL,
                 looked up in ~/.authinfo.gpg (encrypted at rest).  Prefer
                 this: the .ics URL is itself a secret token.  If both are
                 set, :url wins.

For the \\='api fetcher (Google Calendar, sees per-occurrence response
status so OOO auto-declines on recurring events can be filtered):
  :account     - OAuth account nickname (work, personal, ...) matching the
                 token file under ~/.config/calendar-sync/
  :calendar-id - Calendar ID (\"primary\" or a long calendar address)

Example:
  (setq calendar-sync-calendars
        \\='((:name \"google\"
           :fetcher api
           :account \"work\"
           :calendar-id \"primary\"
           :file gcal-file)
          (:name \"proton\"
           :url \"https://calendar.proton.me/api/calendar/v1/url/.../calendar.ics\"
           :file pcal-file)))")

(defcustom calendar-sync-private-config-file
  (expand-file-name "calendar-sync.local.el" user-emacs-directory)
  "Private calendar-sync config file loaded when readable.
This file is the intended place to set `calendar-sync-calendars' with private
calendar feed URLs."
  :type 'file
  :group 'calendar-sync)

(defvar calendar-sync-interval-minutes 60
  "Sync interval in minutes.
Default: 60 minutes (1 hour).")

(defvar calendar-sync-auto-start nil
  "Whether the editor arms its own periodic sync when this module loads.

Off by default: the calendar-sync.timer systemd unit owns the schedule now,
running scripts/calendar-sync-run every ten minutes whether or not Emacs is
up.  Leaving the in-editor timer armed as well would give two owners writing
the same org files and the same state file, with no coordination between
them.

Set to t to hand the schedule back to the editor -- the deferred start at the
end of this file still works, and `calendar-sync-start' and
`calendar-sync-now' remain available on demand either way.

The tradeoff this default accepts: a checkout whose timer has never been
enabled does not sync on its own.  Enabling the unit is a one-time step per
machine, alongside symlinking it into ~/.config/systemd/user.")

(defvar calendar-sync-user-emails
  '("craigmartinjennings@gmail.com" "craig.jennings@deepsat.com" "c@cjennings.net")
  "List of user email addresses for determining acceptance status.
Used by `calendar-sync--find-user-status' to look up the user's
PARTSTAT in event attendee lists.")

(defvar calendar-sync-skip-declined t
  "When non-nil, drop events whose PARTSTAT for the user is \"declined\".
Declined events still arrive in the ICS feed, but they shouldn't show
up on the agenda. Set to nil to keep them (each entry then carries a
:STATUS: declined property drawer).
Note: the ICS feed and the Google Calendar API can disagree — auto-
declines via OOO sometimes write only on the API side, so a few
declined events may still slip through.")

(defvar calendar-sync-past-months 3
  "Number of months in the past to include when expanding recurring events.
Default: 3 months. This keeps recent history visible in org-agenda.")

(defvar calendar-sync-future-months 12
  "Number of months in the future to include when expanding recurring events.
Default: 12 months. This provides a full year of future events.")

(defvar calendar-sync-python-command "python3"
  "Executable used to run the Google Calendar API helper script.
Only the API fetch path (a calendar with `:fetcher' \\='api) uses it; the
default .ics path shells out to curl instead.")

(defvar calendar-sync-fetch-timeout 120
  "Maximum time in seconds for a calendar fetch to complete.
This is the total time allowed for the entire transfer (connect + download).
Large calendars (thousands of events) may need more time on slow connections.
A separate 10-second connect timeout ensures fast failure when a host is
unreachable.")

(defvar calendar-sync--module-file
  (let* ((loaded-file (or load-file-name buffer-file-name))
         (source-file (when loaded-file
                        (concat (file-name-sans-extension loaded-file) ".el"))))
    (if (and source-file (file-readable-p source-file))
        source-file
      loaded-file))
  "Absolute path to this module file.
Used by the batch conversion worker so it can load the same parser code
without loading the user's init file.")

;;; Internal state

(defvar calendar-sync--timer nil
  "Timer object for automatic syncing.")

;;; Parsing orchestration

(defun calendar-sync--parse-ics (ics-content)
  "Parse ICS-CONTENT and return org-formatted string.
Returns nil if parsing fails.
Events are sorted chronologically by start time.
Recurring events are expanded into individual occurrences.
RECURRENCE-ID exceptions are applied to override specific occurrences."
  (condition-case err
      (let* ((range (calendar-sync--get-date-range))
             (events (calendar-sync--split-events ics-content))
             ;; First pass: collect all RECURRENCE-ID exceptions
             (exceptions (calendar-sync--collect-recurrence-exceptions ics-content))
             (parsed-events '())
             (max-events 5000)  ; Safety limit to prevent Emacs from hanging
             (events-generated 0))
        ;; Process each event
        (dolist (event-str events)
          (when (< events-generated max-events)
            (let ((expanded (calendar-sync--expand-recurring-event event-str range)))
              (if expanded
                  ;; Recurring event - add all occurrences with exceptions applied
                  (let ((with-exceptions (calendar-sync--apply-recurrence-exceptions
                                          expanded exceptions)))
                    (setq parsed-events (append parsed-events with-exceptions))
                    (setq events-generated (+ events-generated (length with-exceptions))))
                ;; Non-recurring event - parse normally
                (let ((parsed (calendar-sync--parse-event event-str)))
                  (when (and parsed
                             (calendar-sync--date-in-range-p (plist-get parsed :start) range))
                    (push parsed parsed-events)
                    (setq events-generated (1+ events-generated))))))))
        (when (>= events-generated max-events)
          (calendar-sync--log-silently "calendar-sync: WARNING: Hit max events limit (%d), some events may be missing" max-events))
        (setq parsed-events (calendar-sync--filter-declined parsed-events))
        (calendar-sync--log-silently "calendar-sync: Processing %d events..." (length parsed-events))
        ;; Sort and convert to org format
        (let* ((sorted-events (sort parsed-events
                                    (lambda (a b)
                                      (time-less-p (calendar-sync--event-start-time a)
                                                   (calendar-sync--event-start-time b)))))
               (org-entries (mapcar #'calendar-sync--event-to-org sorted-events)))
          ;; Distinguish a healthy zero-event calendar from garbage: a real
          ;; iCalendar (carries BEGIN:VCALENDAR) with no in-window events
          ;; returns the header alone, so the caller writes an empty calendar
          ;; and reports success.  Non-iCalendar content (an HTML error page, a
          ;; truncated download) has no VCALENDAR and returns nil -- a failure.
          (cond
           (org-entries
            (concat "# Calendar Events\n\n"
                    (string-join org-entries "\n\n")
                    "\n"))
           ((string-match-p "BEGIN:VCALENDAR" ics-content)
            "# Calendar Events\n\n")
           (t nil))))
    (error
     (calendar-sync--log-silently "calendar-sync: Parse error: %s" (error-message-string err))
     nil)))

;;; Sync dispatch

(defun calendar-sync--sync-calendar (calendar)
  "Sync a single CALENDAR asynchronously.
CALENDAR is a plist with :name, :file, and either :url (the default \\='ics
fetcher) or :account + :calendar-id (the \\='api fetcher).  Dispatches on the
:fetcher key, defaulting to the .ics path.
Updates calendar state and saves to disk on completion.
The fetch and conversion run in external processes so parsing and writing large
calendar files do not block the interactive Emacs thread.

Skips a calendar whose previous sync is still in flight, so a timer tick that
fires before a slow fetch finishes does not launch a second overlapping sync for
the same calendar.

A synchronous failure is contained here and recorded against this calendar
alone.  The async callbacks record their own failures, but they never run when
the error lands before a process starts -- resolving a `:secret-host' feed
reads authinfo.gpg, which signals outright on a cold gpg-agent.  Uncontained,
that first error aborted the whole loop and the remaining calendars never
synced."
  (let ((name (plist-get calendar :name)))
    (if (calendar-sync--syncing-p name)
        (calendar-sync--log-silently
         "calendar-sync: [%s] sync already in flight; skipping overlapping tick" name)
      (condition-case err
          (if (eq (plist-get calendar :fetcher) 'api)
              (calendar-sync--sync-calendar-api calendar)
            (calendar-sync--sync-calendar-ics calendar))
        (error
         (let ((reason (error-message-string err)))
           (calendar-sync--log-silently
            "calendar-sync: [%s] Sync error: %s" name reason)
           (calendar-sync--mark-sync-failed name reason)))))))

(defun calendar-sync--require-calendars ()
  "Return non-nil if calendars are configured, else warn and return nil."
  (or calendar-sync-calendars
      (progn (message "calendar-sync: No calendars configured (set calendar-sync-calendars)")
             nil)))

(defun calendar-sync--sync-all-calendars ()
  "Sync all configured calendars asynchronously.
Each calendar syncs in parallel."
  (when (calendar-sync--require-calendars)
    (message "calendar-sync: Syncing %d calendar(s)..." (length calendar-sync-calendars))
    (dolist (calendar calendar-sync-calendars)
      (calendar-sync--sync-calendar calendar))))

(defun calendar-sync--calendar-names ()
  "Return list of configured calendar names."
  (mapcar (lambda (cal) (plist-get cal :name)) calendar-sync-calendars))

(defun calendar-sync--get-calendar-by-name (name)
  "Find calendar plist by NAME, or nil if not found."
  (cl-find-if (lambda (cal) (string= (plist-get cal :name) name))
              calendar-sync-calendars))

;;; Commands

;;;###autoload
(defun calendar-sync-now (&optional calendar-name)
  "Sync calendar(s) now asynchronously.
When called interactively, prompts to select a specific calendar or all.
When called non-interactively with CALENDAR-NAME, syncs that calendar.
When called non-interactively with nil, syncs all calendars."
  (interactive
   (list (when calendar-sync-calendars
           (let ((choices (cons "all" (calendar-sync--calendar-names))))
             (completing-read "Sync calendar: " choices nil t nil nil "all")))))
  (cond
   ((not (calendar-sync--require-calendars)) nil)
   ((or (null calendar-name) (string= calendar-name "all"))
    (calendar-sync--sync-all-calendars))
   (t
    (let ((calendar (calendar-sync--get-calendar-by-name calendar-name)))
      (if calendar
          (calendar-sync--sync-calendar calendar)
        (message "calendar-sync: Calendar '%s' not found" calendar-name))))))

;;;###autoload
(defun calendar-sync-status ()
  "Display sync status for all configured calendars."
  (interactive)
  (when (calendar-sync--require-calendars)
    (let ((status-lines '()))
      (dolist (calendar calendar-sync-calendars)
        (let* ((name (plist-get calendar :name))
               (file (plist-get calendar :file))
               (state (calendar-sync--get-calendar-state name))
               (status (or (plist-get state :status) 'never))
               (last-sync (plist-get state :last-sync))
               (last-error (plist-get state :last-error))
               (status-str
                (pcase status
                  ('ok (format "✓ %s" (if last-sync
                                          (format-time-string "%Y-%m-%d %H:%M" last-sync)
                                        "unknown")))
                  ('error (format "✗ %s" (or last-error "error")))
                  ('syncing "⟳ syncing...")
                  ('never "— never synced"))))
          (push (format "  %s: %s → %s" name status-str (abbreviate-file-name file))
                status-lines)))
      (message "calendar-sync status:\n%s"
               (string-join (nreverse status-lines) "\n")))))

;;; Batch entry point

;; `emacs --batch' exits the moment its top-level form returns, and this
;; pipeline is asynchronous end to end -- curl in one process, the org
;; conversion in a second batch Emacs.  So `calendar-sync-now' is the wrong
;; entry point for a timer: it returns as soon as the fetches are launched,
;; and batch Emacs would exit and kill both children mid-flight, having
;; written nothing and reported success.
;;
;; The batch path therefore starts the sync and then blocks on the same
;; per-calendar state the interactive session keeps.  Nothing new tracks
;; completion -- the pipeline's own record of what finished is the signal.

(defvar calendar-sync--batch-poll-seconds 0.2
  "Seconds `calendar-sync--batch-wait' blocks per iteration.
Short enough that the wait ends promptly once the last child exits, long
enough that the loop is not a spin.  Rebound in tests.")

(defvar calendar-sync-batch-timeout 300
  "Seconds `calendar-sync-batch-run' waits for every calendar to settle.
Covers a `calendar-sync-fetch-timeout' fetch plus the org conversion, with
room to spare: the calendars run in parallel, so this is not a per-calendar
budget.")

(defun calendar-sync--batch-results (names)
  "Return one (NAME . STATUS) pair per calendar in NAMES, in that order.
A calendar with no state entry reads `never' rather than nil, so one that
never started still counts when the failures are tallied."
  (mapcar (lambda (name)
            (cons name
                  (or (plist-get (calendar-sync--get-calendar-state name) :status)
                      'never)))
          names))

(defun calendar-sync--batch-failures (results)
  "Return the rows of RESULTS that did not finish cleanly.
Only `ok' passes.  `error' failed outright, `syncing' means the wait expired
with the fetch still in flight, and `never' means the sync never started --
in all three the org file on disk is not the calendar's current contents,
which is the staleness the timer exists to prevent."
  (seq-remove (lambda (row) (eq (cdr row) 'ok)) results))

(defun calendar-sync--batch-wait (names timeout)
  "Block until no calendar in NAMES is syncing, or TIMEOUT seconds elapse.
Return non-nil when every calendar settled, nil when the timeout expired
first.  `accept-process-output' is also what lets the fetch and conversion
sentinels run, so this loop drives the pipeline as well as waiting on it."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (seq-some #'calendar-sync--syncing-p names)
                (< (float-time) deadline))
      (accept-process-output nil calendar-sync--batch-poll-seconds))
    (not (seq-some #'calendar-sync--syncing-p names))))

;;;###autoload
(defun calendar-sync-batch-run (&optional timeout)
  "Sync every configured calendar, blocking until all of them finish.
Return the (NAME . STATUS) rows.  TIMEOUT defaults to
`calendar-sync-batch-timeout'.

This is the entry point for the systemd timer.  Prefer `calendar-sync-now'
in an interactive session, where returning immediately is the point."
  (unless (calendar-sync--require-calendars)
    (error "calendar-sync: no calendars configured"))
  (let ((names (calendar-sync--calendar-names)))
    (calendar-sync--sync-all-calendars)
    (calendar-sync--batch-wait names (or timeout calendar-sync-batch-timeout))
    (calendar-sync--batch-results names)))

;;;###autoload
(defun calendar-sync-batch-run-and-report ()
  "Run a batch sync, print one line per calendar, and return an exit code.
0 when every calendar synced, 1 otherwise.  Written for
scripts/calendar-sync-run, which turns the code into the process's own exit
status so systemd records a failed sync instead of swallowing it.

A failed row carries its recorded `:last-error'.  The interactive failure
path logs the reason to *Messages', which batch Emacs discards at exit, so
without this the journal shows only `error' — no way to tell a cold
gpg-agent from a revoked feed token or a dead network without re-running the
sync by hand."
  (let* ((results (calendar-sync-batch-run))
         (failures (calendar-sync--batch-failures results)))
    (dolist (row results)
      (let ((reason (unless (eq (cdr row) 'ok)
                      (plist-get (calendar-sync--get-calendar-state (car row))
                                 :last-error))))
        (princ (format "%s: %s%s\n" (car row) (cdr row)
                       (if reason (format " — %s" reason) "")))))
    (if failures 1 0)))

;;; Timer management

(defun calendar-sync--sync-timer-function ()
  "Function called by the hourly sync timer.
Checks for timezone changes and triggers re-sync if detected.

The body is wrapped so a signal — from the timezone check or the sync
fan-out — is caught and logged rather than propagated: this runs from a
`run-at-time' timer, and an unguarded error would repeat on every tick,
once an hour, indefinitely.  The timezone-change notice goes to the silent
log, not the echo area, since an hourly timer must not spam `message'."
  (condition-case err
      (progn
        (when (calendar-sync--timezone-changed-p)
          (let ((old-tz (calendar-sync--format-timezone-offset
                         calendar-sync--last-timezone-offset))
                (new-tz (calendar-sync--format-timezone-offset
                         (calendar-sync--current-timezone-offset))))
            (calendar-sync--log-silently
             "calendar-sync: Timezone change detected (%s → %s), re-syncing..."
             old-tz new-tz)))
        (calendar-sync--sync-all-calendars))
    (error
     (calendar-sync--log-silently
      "calendar-sync: sync timer error: %s" (error-message-string err)))))

;;;###autoload
(defun calendar-sync-start ()
  "Start automatic calendar syncing.
Syncs all calendars immediately, then every `calendar-sync-interval-minutes'."
  (interactive)
  (when calendar-sync--timer
    (cancel-timer calendar-sync--timer))
  (when (calendar-sync--require-calendars)
    ;; Sync immediately
    (calendar-sync--sync-all-calendars)
    ;; Start timer for future syncs (convert minutes to seconds)
    (let ((interval-seconds (* calendar-sync-interval-minutes 60)))
      (setq calendar-sync--timer
            (run-at-time interval-seconds
                         interval-seconds
                         #'calendar-sync--sync-timer-function)))
    (message "calendar-sync: Auto-sync started (every %d minutes, %d calendars)"
             calendar-sync-interval-minutes
             (length calendar-sync-calendars))))

;;;###autoload
(defun calendar-sync-stop ()
  "Stop automatic calendar syncing."
  (interactive)
  (when calendar-sync--timer
    (cancel-timer calendar-sync--timer)
    (setq calendar-sync--timer nil)
    (message "calendar-sync: Auto-sync stopped")))

;;;###autoload
(defun calendar-sync-toggle ()
  "Toggle automatic calendar syncing on/off."
  (interactive)
  (if calendar-sync--timer
      (calendar-sync-stop)
    (calendar-sync-start)))

;;; Keybindings

;; Calendar sync prefix and keymap
(defvar-keymap cj/calendar-map
  :doc "Keymap for calendar synchronization operations"
  "s" #'calendar-sync-now
  "i" #'calendar-sync-status
  "t" #'calendar-sync-toggle
  "S" #'calendar-sync-start
  "x" #'calendar-sync-stop)

(cj/register-prefix-map "g" cj/calendar-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; g" "calendar sync menu"
    "C-; g s" "sync now"
    "C-; g i" "sync status"
    "C-; g t" "toggle auto-sync"
    "C-; g S" "start auto-sync"
    "C-; g x" "stop auto-sync"))

;;; Initialization

(calendar-sync--load-private-config)

;; Load saved state from previous session
(calendar-sync--load-state)

;; Check for timezone change on startup
(when (and calendar-sync-calendars
           (calendar-sync--timezone-changed-p))
  (let ((old-tz (calendar-sync--format-timezone-offset
                 calendar-sync--last-timezone-offset))
        (new-tz (calendar-sync--format-timezone-offset
                 (calendar-sync--current-timezone-offset))))
    (message "calendar-sync: Timezone changed since last session (%s → %s)"
             old-tz new-tz)
    (message "calendar-sync: Will sync on next timer tick")
    ;; Note: We don't auto-sync here to avoid blocking Emacs startup
    ;; User can manually sync or it will happen on next timer tick if auto-sync is enabled
    ))

;; Defer auto-sync until calendar data is first needed.
;;
;; Dormant unless `calendar-sync-auto-start' is turned back on -- the systemd
;; timer owns the schedule now.  Kept because the reasoning below still holds
;; for anyone who hands the schedule back to the editor, and because it is the
;; only safe shape for an in-editor start.
;;
;; The :secret-host feed URLs live in authinfo.gpg, and BOTH the immediate sync
;; and every periodic timer tick resolve them.  Calling `calendar-sync-start' at
;; load (immediate sync + recurring timer) therefore decrypts authinfo.gpg right
;; after startup, prompting for the GPG passphrase on a cold gpg-agent (e.g.
;; after a reboot).  Defer the whole start to the first org-agenda use, so the
;; unlock happens when the user actually asks for calendar data.  A manual
;; `calendar-sync-start' / `calendar-sync-now' still works on demand.
;;
;; That deferral is also what made the timer necessary: hanging the start on
;; `org-agenda-mode-hook' means a session where the agenda is never opened
;; never syncs at all, which after a reboot is every session until the first
;; agenda call.  The batch path has no such trigger to miss.
(defun calendar-sync--auto-start-on-first-agenda ()
  "Start auto-sync on the first org-agenda use, then remove this hook.
One-shot: deferring `calendar-sync-start' until the agenda is first built keeps a
cold gpg-agent from being prompted for the authinfo passphrase at startup.
Removes itself before starting so a `calendar-sync-start' error can't re-fire it."
  (remove-hook 'org-agenda-mode-hook #'calendar-sync--auto-start-on-first-agenda)
  (calendar-sync-start))

;; Arm the deferred start when auto-sync is enabled and calendars are configured.
(when (and calendar-sync-auto-start
           calendar-sync-calendars
           (not noninteractive))
  (add-hook 'org-agenda-mode-hook #'calendar-sync--auto-start-on-first-agenda))


(provide 'calendar-sync)
;;; calendar-sync.el ends here
