;;; ai-term.el --- AI-agent terminals backed by EAT and tmux -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: eager.
;; Eager reason: binds M-SPC and the C-; a AI-agent prefix.
;; Top-level side effects: global M-SPC binding, C-; a prefix map, and the
;;   undead-buffers registration for agent buffers.
;; Runtime requires: seq, system-lib, keybindings, ai-term-sessions,
;;   ai-term-display, ai-term-backend-eat.
;; Direct test load: yes.
;;
;; Opens project-scoped AI agents in EAT buffers backed by tmux sessions. Project
;; candidates come from configured roots that contain .ai/protocols.org.
;;
;; Agent display reuses the host-appropriate side slot when possible, otherwise
;; splits right on desktop frames and below on laptop frames. Attached buffers
;; and detached tmux sessions share the same rotation; selecting a detached
;; agent recreates its EAT buffer and attaches to the live session.
;;
;; This is the public face of the module: it owns the user options, the
;; public commands and their dispatch, the shutdown/wrap-up entry points, and
;; the C-; a keymap.  Project/session discovery, window/display policy, and
;; the EAT terminal backend live in the ai-term-sessions / ai-term-display /
;; ai-term-backend-eat layers, which this module requires.  Every public name
;; is unchanged so existing (require 'ai-term) callers and tests keep working.

;;; Code:

(require 'seq)
(require 'system-lib)   ;; provides cj/completion-table
(require 'keybindings)  ;; provides cj/register-prefix-map (C-; a)
(require 'ai-term-sessions)
(require 'ai-term-display)
(require 'ai-term-backend-eat)

(declare-function cj/make-buffer-pattern-undead "undead-buffers")

(defgroup ai-term nil
  "In-Emacs AI-agent launcher with a vertical-split EAT terminal."
  :group 'tools)

(defcustom cj/ai-term-agent-command
  "claude \"Read .ai/protocols.org and follow all instructions.\""
  "Shell command sent to a fresh AI-term to start the agent.

The default invokes the Claude Code CLI; set it to whatever terminal
agent you run (aider, an open-source LLM TUI, etc.)."
  :type 'string
  :group 'ai-term)

(defcustom cj/ai-term-project-roots
  (list (expand-file-name "~/.emacs.d"))
  "Directories that are themselves AI-agent projects.
Each entry is included as a candidate when it exists and contains
.ai/protocols.org.  Use this for single-project roots like ~/.emacs.d."
  :type '(repeat directory)
  :group 'ai-term)

(defcustom cj/ai-term-container-roots
  (list (expand-file-name "~/code")
        (expand-file-name "~/projects"))
  "Directories whose immediate children are scanned for agent projects.
Each entry's child directories are included as candidates when they
contain .ai/protocols.org.  Use this for container dirs like ~/code."
  :type '(repeat directory)
  :group 'ai-term)

(defcustom cj/ai-term-tmux-session-prefix "aiv-"
  "Prefix prepended to tmux session names AI-term creates.

The session name for a project is this prefix followed by the
project's basename (whitespace collapsed to hyphens).  The prefix
lets `tmux ls' output be filtered down to AI-term's own sessions --
so after an Emacs crash the project picker can match surviving
sessions back to their directories and surface them first.  Pick
something unlikely to collide with hand-rolled tmux sessions; the
default \"aiv-\" is short for \"ai-term\"."
  :type 'string
  :group 'ai-term)

(defcustom cj/ai-term-tmux-window-name "ai"
  "Name given to the first tmux window in an AI-term session.

Passed as `tmux new-session -n', so the window running the AI tool
shows up as this name in `tmux ls' / the status line.  A later
window opened by hand (e.g. a shell) auto-names after its command,
so the two read distinctly instead of both showing up as the
running program."
  :type 'string
  :group 'ai-term)

(defcustom cj/ai-term-desktop-width 0.5
  "Default fraction of frame width for the AI-term window on a desktop.

On a desktop the agent opens as a right-side vertical split (see
`cj/--ai-term-default-direction'), so this fraction is interpreted
as a window width.  Used by `cj/--ai-term-default-size' as the size
fallback when `cj/--ai-term-last-size' is nil (i.e. the user hasn't
yet toggled off an agent window in this session)."
  :type 'number
  :group 'ai-term)

(defcustom cj/ai-term-laptop-height 0.75
  "Default fraction of frame height for the AI-term window on a laptop.

On a laptop the agent opens as a bottom horizontal split (see
`cj/--ai-term-default-direction'), so this fraction is interpreted
as a window height.  Used by `cj/--ai-term-default-size' as the size
fallback when `cj/--ai-term-last-size' is nil."
  :type 'number
  :group 'ai-term)

(defface cj/ai-term-accent
  '((t :foreground "#67809c"))
  "Accent color for agent terminals, defaulting to dupre blue.
Claude Code draws its accent (the bypass-permissions banner, borders,
spinner) with xterm-256 palette colors; agent terminals point those
palette entries at this face (see `cj/ai-term-accent-color-indices'),
so the accent renders in this face's foreground instead of the stock
rose red.  Per-project colors later land as per-buffer overrides of
the same palette entries."
  :group 'ai-term)

(defvar cj/ai-term-accent-color-indices '(211)
  "The xterm-256 palette indices agent terminals repaint with the accent.
211 (#ff87af, a rose pink) is Claude Code's accent as rendered through
the 256-color palette -- confirmed empirically on the bypass-permissions
banner.  Add indices here if other accent elements surface in a
different palette slot.  Applied per terminal by
`cj/--ai-term-apply-accent'; other eat terminals keep the true palette.")

;; Agent buffers ("agent [<project>]") are buried, not killed, by the
;; kill-all sweep (F1 / `cj/dashboard-only').  Register the family pattern so
;; every agent -- however and whenever created -- survives with its session.
(with-eval-after-load 'undead-buffers
  (cj/make-buffer-pattern-undead "\\`agent \\["))

(defun cj/--ai-term-dispatch ()
  "Compute the `cj/ai-term' (C-; a a) action without performing it.

Returns one of:
- (toggle-off . WINDOW)        -- agent is displayed in WINDOW; quit it.
- (redisplay-recent . BUFFER)  -- 1+ alive agent buffers; show MRU.
- (pick-project)               -- zero alive agent buffers; prompt.

When 2+ agent buffers are alive, C-; a a redisplays the most-recently-
selected one rather than opening the project picker.  C-; a s is the
explicit \"start a different project\" surface; C-; a n is the explicit
\"switch among existing agents\" surface.  C-; a a keeps a single, simple
job: toggle whichever agent was last in use.

A pure-decision helper so the dispatch logic is exercisable in tests
without firing real `display-buffer' or `quit-window' calls."
  (let ((win (cj/--ai-term-displayed-agent-window)))
    (cond
     (win (cons 'toggle-off win))
     (t
      (let ((buffers (cj/--ai-term-agent-buffers)))
        (cond
         (buffers
          ;; Reopen the agent the last toggle-off hid (faithful toggle), so
          ;; long as it's still alive and among the live agents.  Otherwise
          ;; fall back to the most-recently-selected agent.
          (cons 'redisplay-recent
                (if (and (buffer-live-p cj/--ai-term-last-hidden-buffer)
                         (memq cj/--ai-term-last-hidden-buffer buffers))
                    cj/--ai-term-last-hidden-buffer
                  (car buffers))))
         (t '(pick-project))))))))

(defun cj/ai-term-pick-project (&optional arg)
  "Pick an AI-agent project and open or reuse its EAT terminal.

The project is picked from a filtered completing-read list of dirs
that contain .ai/protocols.org.  The terminal buffer is named
\"agent [<basename>]\" and is routed to a right-side window via
`display-buffer-alist'.  Multiple projects coexist as separate
buffers; reinvoking on the same project reuses its existing terminal.

With prefix ARG, display the buffer without selecting its window.

Bound to C-; a s -- always shows the project picker, even when an agent
buffer is currently displayed.

EAT renders in terminal frames as well as GUI frames, so this
launches from either."
  (interactive "P")
  (let* ((dir (cj/--ai-term-pick-project))
         (name (cj/--ai-term-buffer-name dir))
         (buf (cj/--ai-term-show-or-create dir name)))
    (unless arg
      (let ((win (get-buffer-window buf)))
        (when win (select-window win))))
    buf))

(defun cj/ai-term (&optional arg)
  "DWIM dispatch for the AI-term launcher.  Bound to C-; a a.

Behavior depends on the current state:

- If an AI-term buffer is currently displayed in this frame, it
  quits its window (toggle off, buffer stays alive).
- Else, if exactly one alive AI-term buffer exists, it re-displays
  it (DWIM -- the obvious next step is to look at it).
- Else (zero or 2+), it falls through to `cj/ai-term-pick-project'.

With prefix ARG, display the buffer without selecting its window
when a buffer is being shown (no effect on the toggle-off branch).

See `cj/ai-term-pick-project' (C-; a s) to force the project picker.
C-; a k closes an agent via `cj/ai-term-close'."
  (interactive "P")
  (pcase (cj/--ai-term-dispatch)
    (`(toggle-off . ,win)
     (cj/--ai-term-toggle-off win))
    (`(redisplay-recent . ,buf)
     (display-buffer buf)
     (unless arg
       (let ((w (get-buffer-window buf)))
         (when w (select-window w))))
     buf)
    (`(pick-project)
     (cj/ai-term-pick-project arg))))

;; ----------------------------- Close an agent --------------------------------

(defun cj/--ai-term-close-buffer (buffer)
  "Gracefully tear down AI-term BUFFER: tmux session, then buffer.

Derives the tmux session name from BUFFER's `default-directory' (the
project dir the terminal was created in) and kills it so the agent
process stops.  When BUFFER is shown, swaps its window to a non-agent
buffer (the working file) rather than deleting the window -- closing an
agent must not collapse the user's window layout; the hide toggle is
what collapses the split.  Then kills BUFFER (suppressing the
process-still-running prompt -- the session is already down).  No-op
when BUFFER isn't an AI-term buffer."
  (when (cj/--ai-term-buffer-p buffer)
    (cj/--ai-term-kill-tmux-session
     (cj/--ai-term-tmux-session-name
      (buffer-local-value 'default-directory buffer)))
    (let ((win (get-buffer-window buffer)))
      (when (window-live-p win)
        (cj/--ai-term-swap-to-working-buffer win)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun cj/--ai-term-close-target ()
  "Return the AI-term buffer `cj/ai-term-close' should act on, or nil.

The current buffer when it is an agent buffer; else the sole live
agent buffer; else a `completing-read' choice among the live agent
buffers; nil when none are alive."
  (cond
   ((cj/--ai-term-buffer-p (current-buffer)) (current-buffer))
   (t (let ((buffers (cj/--ai-term-agent-buffers)))
        (cond
         ((null buffers) nil)
         ((null (cdr buffers)) (car buffers))
         (t (get-buffer
             (completing-read "Close AI terminal: "
                              (cj/completion-table 'buffer (mapcar #'buffer-name buffers))
                              nil t))))))))

(defun cj/ai-term-close ()
  "Gracefully close an AI-term agent: kill its tmux session and buffer.

Targets the current agent buffer, the sole live agent, or prompts when
several are alive (see `cj/--ai-term-close-target').  Asks for
confirmation first -- this kills the running agent process, which can
interrupt work in progress.  Bound to C-; a k."
  (interactive)
  (let ((buffer (cj/--ai-term-close-target)))
    (unless buffer
      (user-error "No AI-term agent buffers to close"))
    (let ((name (buffer-name buffer)))
      (when (y-or-n-p (format "Close agent %s?  This kills its tmux session.  "
                              name))
        (cj/--ai-term-close-buffer buffer)
        (message "Closed agent %s." name)))))

;; ------------------------- Step to the next agent ----------------------------

(defun cj/ai-term-next ()
  "Step to the next open AI-term agent in the queue.

The queue is every active agent ordered by buffer name -- a stable
rotation, unaffected by which agent was most recently selected.  Active
means a live agent buffer (attached) OR a live tmux session with no Emacs
buffer (detached); stepping onto a detached agent attaches it (recreates
its terminal, which reattaches the session).  When an agent window is on
screen, swap it to the next agent (wrapping after the last) and select it.
When no agent is displayed but agents exist, show the first.  When none
are open, open the project picker to launch the first agent rather than
erroring.  When the sole agent is already focused, echo that there are
no other ai-terms to switch to instead of swapping to itself.

Bound to M-SPC.  Unlike C-; a a (toggle the most-recent agent on/off), this
is the \"switch among existing agents\" surface; C-; a s opens the project
picker and C-; a k closes an agent."
  (interactive)
  (let* ((dirs (cj/--ai-term-active-agent-dirs))
         (win (cj/--ai-term-displayed-agent-window))
         (current-name (and win (buffer-name (window-buffer win))))
         (current-dir (and current-name
                           (seq-find (lambda (d)
                                       (equal (cj/--ai-term-buffer-name d) current-name))
                                     dirs)))
         (next-dir (cj/--ai-term-next-agent-dir current-dir dirs)))
    (cond
     ((not next-dir)
      ;; No agents open: launch the first via the project picker instead of
      ;; erroring, so the swap key doubles as a "start an agent" key.
      (cj/ai-term-pick-project))
     ;; Sole agent, already focused: the rotation wraps back to the same
     ;; agent, so a swap would be a silent no-op.  Say there's nowhere to
     ;; go instead.  A sole agent that is displayed but not selected still
     ;; falls through and gets selected.
     ((and current-dir
           (equal next-dir current-dir)
           (eq win (selected-window)))
      (message "No other ai-terms to switch to"))
     (t
      (let* ((name (cj/--ai-term-buffer-name next-dir))
             (existing (get-buffer name)))
        ;; Live agent and an agent window is up: swap it into that window in
        ;; place (faithful to the prior buffer-only behavior).  Detached, or no
        ;; window yet: show-or-create attaches the tmux session / displays it.
        ;; No "Agent: <name>" echo after the swap: the modeline already
        ;; announces the agent (buffer name + eat state), and the duplicate
        ;; message was echo-area clutter (roam inbox, 2026-07-02).
        (if (and win existing (cj/--ai-term-process-live-p existing))
            (progn (set-window-buffer win existing) (select-window win))
          (cj/--ai-term-show-or-create next-dir name)
          (let ((w (get-buffer-window name)))
            (when w (select-window w)))))))))

;; ai-term lives under the C-; a prefix (vacated when gptel was archived).
;; The frequent "swap to the next agent" also gets M-SPC for a fast chord.
(defvar-keymap cj/ai-term-keymap
  :doc "Keymap for ai-term agent commands (C-; a)."
  "a" #'cj/ai-term               ;; toggle the most-recent agent on/off
  "s" #'cj/ai-term-pick-project  ;; select / launch via the project picker
  "n" #'cj/ai-term-next          ;; swap to the next open agent
  "k" #'cj/ai-term-close)        ;; kill the current agent
(cj/register-prefix-map "a" cj/ai-term-keymap "ai-term")
(keymap-global-set "M-SPC" #'cj/ai-term-next)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; a"   "ai-term menu"
    "C-; a a" "toggle agent"
    "C-; a s" "select / launch"
    "C-; a n" "next agent"
    "C-; a k" "kill agent"
    "M-SPC"   "ai-term: next agent"))

;; ------------------- Wrap-it-up teardown + shutdown -------------------------
;;
;; Headless entry points the rulesets wrap-it-up workflow calls via
;; `emacsclient -e' (its Stop hook ~/.claude/hooks/ai-wrap-teardown.sh).  All
;; three must work with no interactive frame guaranteed.  rulesets owns the
;; workflow + hook that call these; this module owns the aiv- session naming,
;; the agent buffer, and the geometry restore, so the functions live here.
;; See docs/design/2026-06-23-wrap-teardown-shutdown-proposal.org (rulesets).

(defcustom cj/ai-term-shutdown-command "sudo shutdown now"
  "Shell command run when the shutdown countdown completes uncancelled.
A defcustom so development and tests can stub it instead of powering off
\(sudo is NOPASSWD on Craig's machines, so the default really shuts down)."
  :type 'string
  :group 'cj)

(defun cj/ai-term-quit (&optional project)
  "Tear down PROJECT's AI-term: kill its tmux session, buffer, and restore layout.
PROJECT is a project basename (as the rulesets Stop hook passes) or a directory;
nil means the current project (`default-directory').  Kills the `aiv-<name>'
tmux session (taking the agent process with it), then, when the agent buffer is
live, swaps its window back to the working buffer and kills it.  Idempotent and
safe headless: a session or buffer already gone is a no-op, not an error."
  (let* ((key (or project default-directory))
         (session (cj/--ai-term-tmux-session-name key))
         (buffer (get-buffer (cj/--ai-term-buffer-name key))))
    (cj/--ai-term-kill-tmux-session session)
    (when (cj/--ai-term-buffer-p buffer)
      (let ((win (get-buffer-window buffer)))
        (when (window-live-p win)
          (cj/--ai-term-swap-to-working-buffer win)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    session))

(defun cj/ai-term-live-count ()
  "Return the integer count of live AI-term (aiv-*) tmux sessions.
0 when tmux has no server or no AI-term sessions.  The shutdown safety gate:
`emacsclient -e (cj/ai-term-live-count)' prints the integer for the hook."
  (length (cj/--ai-term-live-tmux-sessions)))

(defvar cj/--ai-term-shutdown-timer nil
  "The active shutdown-countdown repeating timer, or nil when none is running.")

(defun cj/--ai-term-shutdown-clear-timer ()
  "Cancel and forget the shutdown-countdown timer, if any."
  (when (timerp cj/--ai-term-shutdown-timer)
    (cancel-timer cj/--ai-term-shutdown-timer))
  (setq cj/--ai-term-shutdown-timer nil))

(defun cj/ai-term-shutdown-cancel ()
  "Cancel an in-progress AI-term shutdown countdown."
  (interactive)
  (when cj/--ai-term-shutdown-timer
    (cj/--ai-term-shutdown-clear-timer)
    (message "Shutdown cancelled.")))

(defun cj/ai-term-shutdown-countdown (&optional seconds)
  "Count down SECONDS (default 10) in the echo area, then shut the machine down.
Re-checks the safety gate first (a TOCTOU guard against the workflow's earlier
check): aborts with a message when more than one `aiv-*' session is live.  The
countdown is an abort-able `run-at-time' timer -- `C-g' (while the countdown
owns the keymap) or \\[cj/ai-term-shutdown-cancel] stops it.  On reaching zero
uncancelled it runs `cj/ai-term-shutdown-command'.  Returns immediately so the
Stop hook does not block; the daemon ticks the timer asynchronously."
  (if (> (cj/ai-term-live-count) 1)
      (progn
        (message "Shutdown aborted: %d AI-term sessions still live."
                 (cj/ai-term-live-count))
        nil)
    (cj/--ai-term-shutdown-clear-timer)
    (let ((remaining (or seconds 10)))
      (set-transient-map
       (let ((m (make-sparse-keymap)))
         (define-key m (kbd "C-g") #'cj/ai-term-shutdown-cancel)
         m)
       (lambda () (and cj/--ai-term-shutdown-timer t)))
      (setq cj/--ai-term-shutdown-timer
            (run-at-time
             0 1
             (lambda ()
               (if (<= remaining 0)
                   (progn
                     (cj/--ai-term-shutdown-clear-timer)
                     (shell-command cj/ai-term-shutdown-command))
                 (message "Shutting down in %d…  (C-g to cancel)" remaining)
                 (setq remaining (1- remaining))))))
      nil)))

(provide 'ai-term)
;;; ai-term.el ends here
