;;; ai-term.el --- In-Emacs AI-agent launcher with vertical-split terminal -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: eager.
;; Eager reason: registers four global keys for the AI-agent terminal launcher; a
;;   command-loaded deferral candidate.
;; Top-level side effects: four global key bindings.
;; Runtime requires: cl-lib, seq, cj-window-geometry-lib, cj-window-toggle-lib,
;;   host-environment.
;; Direct test load: yes.
;;
;; Picks an AI-agent project (a dir under ~/.emacs.d, ~/code/*, or
;; ~/projects/* containing .ai/protocols.org), opens or reuses a terminal
;; buffer named "agent [<basename>]", sends the agent's startup
;; instruction to it, and routes the buffer to a side window via
;; display-buffer-alist.  When the frame already has a window forming the
;; half the agent would occupy (a right column on a desktop, a bottom row
;; on a laptop), the agent reuses that slot rather than splitting a third
;; window in; toggling off restores the displaced buffer to the slot.
;; Otherwise placement is a host-aware split: a right-side split at 50%
;; width on a desktop, a bottom split at 75% height on a laptop (see
;; `cj/--ai-term-default-direction').  Multiple
;; projects produce multiple coexisting buffers that share the same
;; slot; switching among them is a buffer-switch, not a
;; kill-and-recreate.
;;
;; Each project's agent runs inside a tmux session named
;; "<cj/ai-term-tmux-session-prefix><basename>" (default prefix "aiv-").
;; The prefix lets `tmux ls' be filtered to AI-term's own sessions, so
;; after an Emacs crash the project picker can match surviving sessions
;; back to their directories: matched projects sort to the top of the
;; picker (flagged "[detached]" -- session alive, no Emacs buffer -- or
;; "[running]" when a live terminal buffer exists), the rest follow in
;; alphabetical order.
;;
;; Four F-key entry points:
;;
;; - F9     `cj/ai-term' -- DWIM dispatch.  If an agent buffer is
;;          currently displayed in this frame, F9 toggles it off: when it
;;          took over an existing window (a reused slot) the buffer it
;;          displaced returns to that slot, when it was split into its own
;;          window that window is removed, and when it fills the frame it
;;          is buried.  Otherwise, if exactly one agent buffer is alive,
;;          F9 re-displays it; if zero or two-plus are alive, F9 falls
;;          through to the project picker.
;; - C-F9   `cj/ai-term-pick-project' -- always show the project
;;          picker, even when an agent buffer is currently displayed.
;;          Used when the user wants to start a new project session
;;          instead of toggling the current one.
;; - s-F9   `cj/ai-term-next' -- step to the next active agent in the
;;          queue.  The queue is every active agent in buffer-name order
;;          (a stable rotation): attached agents (a live buffer) and
;;          detached ones (a live tmux session with no Emacs buffer).
;;          Stepping onto a detached agent attaches it.  When an agent
;;          window is on screen, swap it to the next agent and focus it,
;;          wrapping after the last; when none is shown but agents exist,
;;          show the first.  This is the "switch among existing agents"
;;          surface F9 deliberately doesn't provide.
;; - M-F9   `cj/ai-term-close' -- gracefully close an agent: kill its
;;          tmux session (stopping the agent process), then its terminal
;;          buffer.  Its window stays in the layout (swapped to the
;;          working buffer), so closing never collapses a split.  Confirms
;;          first.  Targets the current agent, the sole live agent, or
;;          prompts among several.
;;
;; Existing windmove (Shift-arrows) handles code <-> agent focus
;; toggling.  Buffer-move (C-M-arrows) handles side-swap.  Neither
;; needs anything new from this module.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'cj-window-geometry-lib)
(require 'cj-window-toggle-lib)
(require 'host-environment)
(require 'keybindings)  ;; provides cj/register-prefix-map (C-; a)

(declare-function eat "eat" (&optional program arg))
(defvar eat-buffer-name)
(defvar eat-semi-char-mode-map)

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

(defconst cj/--ai-term-name-prefix "agent ["
  "Buffer-name prefix shared by all AI-term buffers.

Single source of truth for both buffer construction in
`cj/--ai-term-buffer-name' and detection in
`cj/--ai-term-buffer-p'.  The display-buffer-alist rule keys on the
escaped form \"\\\\`agent \\\\[\" -- they must stay in sync.")

(defun cj/--ai-term-buffer-name (dir)
  "Return the AI-term buffer name for project directory DIR.

The name pattern is \"agent [<basename>]\".  The display-buffer-alist
rule keys on the literal prefix \"agent [\", so changing the format
breaks routing to the right-side window."
  (format "%s%s]"
          cj/--ai-term-name-prefix
          (file-name-nondirectory (directory-file-name dir))))

(defun cj/--ai-term-buffer-p (buffer)
  "Return non-nil when BUFFER is an AI-term buffer.

A buffer qualifies when its name starts with the literal prefix in
`cj/--ai-term-name-prefix' (\"agent [\").  The check is anchored at
the start so names like \"foo agent [bar]\" do not match."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (string-prefix-p cj/--ai-term-name-prefix (buffer-name buffer))))

(defun cj/--ai-term-agent-buffers ()
  "Return the live AI-term buffers in `buffer-list' order.

Order matches `buffer-list' on the selected frame, which is most-
recently-selected first.  Non-AI-term buffers are filtered out via
`cj/--ai-term-buffer-p'."
  (seq-filter #'cj/--ai-term-buffer-p (buffer-list)))

(defun cj/--ai-term-next-agent-dir (current dirs)
  "Return the project dir after CURRENT in DIRS, wrapping to the first.

DIRS is an ordered list of active-agent project dirs.  When CURRENT is
the last element, wrap to the first.  When CURRENT is nil or not a member
of DIRS, return the first dir.  Returns nil when DIRS is empty.  Matches
with `member' (string equality) since dirs are paths.

Pure decision helper (no buffer or window side effects) so the cycle
order driving `cj/ai-term-next' is exercisable in tests."
  (when dirs
    (if (member current dirs)
        (or (cadr (member current dirs))
            (car dirs))
      (car dirs))))

(defun cj/--ai-term-active-agent-dirs ()
  "Return project dirs that have a live agent buffer or a live tmux session.

Sorted by the agent buffer name, so the rotation is stable and matches
what the picker shows.  This is the queue `cj/ai-term-next' steps through:
it includes detached sessions (alive in tmux but with no Emacs buffer),
which the step materializes by attaching."
  (let* ((sessions (cj/--ai-term-live-tmux-sessions))
         (live-names (mapcar #'buffer-name (cj/--ai-term-agent-buffers))))
    (sort
     (seq-filter
      (lambda (dir)
        (or (member (cj/--ai-term-buffer-name dir) live-names)
            (cj/--ai-term-session-active-p dir sessions)))
      (cj/--ai-term-candidates))
     (lambda (a b)
       (string< (cj/--ai-term-buffer-name a) (cj/--ai-term-buffer-name b))))))

(defun cj/--ai-term-most-recent-non-agent-buffer ()
  "Return the most-recently-selected live non-agent buffer, or nil.

Walks `buffer-list' (most-recently-selected first) and returns the
first buffer that is not an AI-term agent buffer (per
`cj/--ai-term-buffer-p') and is not an internal buffer (name starting
with a space).  Used by the single-window F9 toggle-off so dismissing a
full-frame agent returns to the file the user was working in (e.g.
todo.org) rather than swapping in another agent."
  (seq-find (lambda (b)
              (and (buffer-live-p b)
                   (not (cj/--ai-term-buffer-p b))
                   (not (string-prefix-p " " (buffer-name b)))))
            (buffer-list)))

(defun cj/--ai-term-displayed-agent-window (&optional frame)
  "Return a window in FRAME currently displaying an AI-term buffer, or nil.

FRAME defaults to the selected frame.  When more than one window in
the frame shows an agent buffer, the first one in `window-list' order
is returned.  The minibuffer is excluded from the search."
  (seq-find (lambda (w)
              (cj/--ai-term-buffer-p (window-buffer w)))
            (window-list (or frame (selected-frame)) 'never)))

(defun cj/--ai-term-tmux-session-name (dir)
  "Return the tmux session name for project directory DIR.

`cj/ai-term-tmux-session-prefix' followed by DIR's basename, sanitized
to a form tmux won't re-mangle: runs of whitespace become a single
hyphen, and `.' / `:' become `_'.  tmux disallows `.' and `:' in
session names and silently rewrites them to `_', so a project like
`.emacs.d' really runs in session `aiv-_emacs_d', not `aiv-.emacs.d' --
sanitizing up front keeps the computed name matching the live one (and
keeps `cj/--ai-term-session-active-p' and the crash-recovery picker
from missing such projects).  The prefix lets `tmux ls' output be
filtered to AI-term's own sessions (see
`cj/--ai-term-live-tmux-sessions')."
  (concat cj/ai-term-tmux-session-prefix
          (replace-regexp-in-string
           "[.:]" "_"
           (replace-regexp-in-string
            "[[:space:]]+" "-"
            (file-name-nondirectory (directory-file-name dir))))))

(defun cj/--ai-term-live-tmux-sessions ()
  "Return live tmux session names that carry the AI-term prefix.

Runs `tmux list-sessions'.  Returns the names beginning with
`cj/ai-term-tmux-session-prefix', or nil when tmux is not installed,
no server is running, or the command exits non-zero -- the picker
treats nil as \"no sessions to surface\" and falls back to a plain
alphabetical list."
  (let* ((prefix cj/ai-term-tmux-session-prefix)
         (exit nil)
         (output (with-temp-buffer
                   (setq exit (condition-case nil
                                  (process-file "tmux" nil '(t nil) nil
                                                "list-sessions" "-F"
                                                "#{session_name}")
                                (error nil)))
                   (buffer-string))))
    (when (and (integerp exit) (zerop exit))
      (seq-filter (lambda (name) (string-prefix-p prefix name))
                  (split-string output "\n" t)))))

(defun cj/--ai-term-session-active-p (dir sessions)
  "Return non-nil when DIR's tmux session name is in SESSIONS.

SESSIONS is the list from `cj/--ai-term-live-tmux-sessions' (or nil).
The match is forward: DIR's expected session name is computed and
looked up in SESSIONS, so the lossy whitespace->hyphen transform in
`cj/--ai-term-tmux-session-name' never needs reversing."
  (and (member (cj/--ai-term-tmux-session-name dir) sessions) t))

(defun cj/--ai-term-launch-command (dir)
  "Return the shell command line that runs the AI tool in a project tmux session.

Uses `tmux new-session -A' so a second F9 on the same project reattaches
to the running session instead of spawning a new one.  The session name
comes from `cj/--ai-term-tmux-session-name'; the first window is named
`cj/ai-term-tmux-window-name' (default \"ai\") so a later hand-opened
window auto-names after its command and the two read distinctly.

The shell command run on first creation is
  <cj/ai-term-agent-command>; exec bash
so the tmux window survives the AI command exiting -- the session stays
alive with a bare bash prompt for recovery, and reattach works the same way."
  (let ((session (cj/--ai-term-tmux-session-name dir))
        (start-dir (expand-file-name dir)))
    ;; Pass the inner shell-command-string through `shell-quote-argument'
    ;; so any single quotes embedded in a user-customized
    ;; `cj/ai-term-agent-command' don't break the literal single-quote
    ;; wrap below.  The default value carries embedded double quotes
    ;; (\"Read .ai/protocols.org and follow all instructions.\") which
    ;; was safe in the prior shape but a single-quoted custom value
    ;; silently broke the shell parse.
    (format "tmux new-session -A -s %s -n %s -c %s %s"
            (shell-quote-argument session)
            (shell-quote-argument cj/ai-term-tmux-window-name)
            (shell-quote-argument start-dir)
            (shell-quote-argument
             (concat cj/ai-term-agent-command "; exec bash")))))

(defun cj/--ai-term-has-marker-p (dir)
  "Return non-nil when DIR contains .ai/protocols.org."
  (file-exists-p (expand-file-name ".ai/protocols.org" dir)))

(defun cj/--ai-term-candidates ()
  "Return the list of AI-agent project paths.

Each entry of `cj/ai-term-project-roots' contributes itself when it
exists and contains .ai/protocols.org.  Each entry of
`cj/ai-term-container-roots' contributes its immediate child
directories that contain .ai/protocols.org.

Returns absolute paths.  Nonexistent roots are skipped silently."
  (let (result)
    (dolist (root cj/ai-term-project-roots)
      (let ((expanded (expand-file-name root)))
        (when (and (file-directory-p expanded)
                   (cj/--ai-term-has-marker-p expanded))
          (push expanded result))))
    (dolist (root cj/ai-term-container-roots)
      (let ((expanded (expand-file-name root)))
        (when (file-directory-p expanded)
          (dolist (child (directory-files
                          expanded t directory-files-no-dot-files-regexp t))
            (when (and (file-directory-p child)
                       (cj/--ai-term-has-marker-p child))
              (push child result))))))
    (nreverse result)))

(defvar cj/--ai-term-mru nil
  "Project dirs opened via the AI-term launcher this session, newest first.

Maintained by `cj/--ai-term-record-mru' (called from
`cj/--ai-term-show-or-create') and consumed by
`cj/--ai-term-sort-candidates' so the project picker puts
recently-opened projects at the top of the active-sessions group.
In-memory only -- not persisted across Emacs restarts.")

(defun cj/--ai-term-record-mru (dir)
  "Move DIR to the front of `cj/--ai-term-mru'.

DIR is normalized with `expand-file-name' + `directory-file-name' so a
trailing slash or `~' form doesn't create a duplicate entry; any prior
occurrence is removed first, keeping the list a true MRU order."
  (let ((d (directory-file-name (expand-file-name dir))))
    (setq cj/--ai-term-mru (cons d (delete d cj/--ai-term-mru)))))

(defun cj/--ai-term-mru-rank (dir)
  "Return DIR's index in `cj/--ai-term-mru', or nil when it isn't there.

DIR is normalized the same way `cj/--ai-term-record-mru' stores
entries, so a trailing slash doesn't defeat the lookup."
  (seq-position cj/--ai-term-mru
                (directory-file-name (expand-file-name dir))))

(defun cj/--ai-term-sort-candidates (dirs sessions)
  "Order DIRS for the project picker.

DIRS with a live tmux session in SESSIONS (per
`cj/--ai-term-session-active-p') come first, ordered most-recently-
opened first (per `cj/--ai-term-mru'); active dirs not opened yet this
session fall after them, alphabetical by abbreviated path.  DIRS with no
session follow, always alphabetical.  SESSIONS nil means nothing is
active, so the result is a plain alphabetical list; an empty MRU makes
the active group alphabetical too."
  (let* ((alpha (lambda (a b)
                  (string< (abbreviate-file-name a) (abbreviate-file-name b))))
         (mru-then-alpha
          (lambda (a b)
            (let ((ra (cj/--ai-term-mru-rank a))
                  (rb (cj/--ai-term-mru-rank b)))
              (cond ((and ra rb) (< ra rb))
                    (ra t)
                    (rb nil)
                    (t (funcall alpha a b))))))
         (active-p (lambda (d) (cj/--ai-term-session-active-p d sessions)))
         (active (seq-filter active-p dirs))
         (inactive (seq-remove active-p dirs)))
    (append (sort active mru-then-alpha) (sort inactive alpha))))

(defun cj/--ai-term-process-live-p (buffer)
  "Return non-nil when BUFFER has a live process attached."
  (let ((proc (get-buffer-process buffer)))
    (and proc (process-live-p proc))))

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

(defun cj/--ai-term-default-direction (&optional frame)
  "Return the default split direction for the agent window.

Chosen at display time from FRAME's column width (FRAME defaults to the
selected frame): `right' when a side-by-side split would leave both the
agent and the main window at least `cj/window-dock-min-columns' wide,
`below' otherwise.  The agent's share of the width is
`cj/ai-term-desktop-width'.  See `cj/preferred-dock-direction'."
  (let ((frame (or frame (selected-frame))))
    (cj/preferred-dock-direction (frame-width frame)
                                 cj/ai-term-desktop-width)))

(defun cj/--ai-term-default-size ()
  "Return the default size fraction paired with the chosen direction.

`cj/ai-term-desktop-width' (a width fraction) when the default direction is
`right', `cj/ai-term-laptop-height' (a height fraction) when it is `below'."
  (if (eq (cj/--ai-term-default-direction) 'right)
      cj/ai-term-desktop-width
    cj/ai-term-laptop-height))

(defvar cj/--ai-term-last-direction nil
  "Last user-chosen direction for the AI-term display.

Symbol: right, below, or left.  `above' is never stored -- the agent
window must not be remembered at the top of the frame, so a top
placement falls back to the host default at capture time.  nil means no
agent window has been toggled off yet this session, so the default
direction applies.  Captured at toggle-off by
`cj/--ai-term-capture-state' and consumed by
`cj/--ai-term-display-saved'.")

(defvar cj/--ai-term-last-was-bury nil
  "Non-nil when the last F9 toggle-off used `bury-buffer'.

Set by `cj/ai-term' in its `toggle-off' branch: t when the agent
window was the only window in the frame (so toggle-off buried
without deleting), nil when the window was deleted.  Consumed by
`cj/--ai-term-display-saved' to decide between restoring the
buried agent in the current window (the only one) or splitting per
the saved direction.")

(defvar cj/--ai-term-last-toggle-deleted-split nil
  "Non-nil when the last F9 toggle-off deleted the agent's own split window.

Set t by `cj/--ai-term-toggle-off' only when it actually `delete-window's
the agent (a multi-window layout where the agent had its own window);
nil for a bury or a degenerate swap.  Consumed by
`cj/--ai-term-reuse-edge-window': when set, the next toggle-on re-splits a
fresh agent window instead of reusing a window at the edge.  Without this,
toggling the agent off and on in a 3+ window layout would reuse the user's
working window at the edge, displacing its buffer and collapsing the layout
-- the toggle must be reversible (off then on returns the same windows).")

(defvar cj/--ai-term-last-hidden-buffer nil
  "The agent buffer hidden by the most recent F9 toggle-off.

Captured in `cj/ai-term' just before an agent window is torn down, and
consumed by `cj/--ai-term-dispatch' so the next toggle-on reopens the
SAME agent that was on screen rather than whichever agent happens to be
most-recent in `buffer-list'.  Without this, hiding one agent and
reopening could surface a different one when several agents are alive --
the \"the displayed buffer changes\" bug.  Falls back to the buffer-list
MRU when nil or when the remembered buffer has been killed.")

(defvar cj/--ai-term-last-size nil
  "Last user-chosen size for the AI-term display.

Positive integer: body-columns when `cj/--ai-term-last-direction'
is right or left, total-lines when below or above.  nil means use
the host-aware default from `cj/--ai-term-default-size' (a float
fraction).  See `cj/window-replay-size' for the per-axis capture.

The axis choice is asymmetric.  Width captures body-width, not
total-width: total-width includes the right-edge divider when the
window has a right sibling but excludes it at the frame edge, so
capturing total-width from a rightmost agent (no divider) and
replaying into a middle position (with divider) leaves the body 1
column short.  Body-width is divider-independent.

Height captures total-height, not body-height: every window has
exactly one mode line regardless of position, so total-height has
no divider-position problem, and total-height is the same whether
the window is active or inactive.  Body-height would subtract the
mode line's pixel height, which differs between an active and an
inactive (theme-shrunk) mode line -- capturing body-height active
and replaying it inactive then re-measuring active drifts the
window down by ~1 line per toggle (the F9 shrink bug, 2026-06-20).

Absolute values rather than fractions because
`display-buffer-in-direction' interprets a float `window-width' /
`window-height' as a fraction of the new window's parent in the
window tree.  In a 3+ window layout the parent may be a sub-tree,
and a fraction-of-frame produces the wrong size on replay
(squeezes the other windows).  An integer is unambiguous, at the
cost of not auto-scaling if the frame itself resizes.")

(defun cj/--ai-term-capture-state (window)
  "Capture WINDOW's direction and size into module-level state.

Sets `cj/--ai-term-last-direction' and `cj/--ai-term-last-size'
so a subsequent F9 display can restore the user's chosen orientation
and size.  Called at toggle-off (just before the window is torn
down).  The default direction is host-aware via
`cj/--ai-term-default-direction' (used only when WINDOW fills its
frame and no direction can be inferred).  Does nothing when WINDOW
is not live."
  (cj/window-toggle-capture-state
   window (cj/--ai-term-default-direction)
   'cj/--ai-term-last-direction
   'cj/--ai-term-last-size
   '(right below left)))

(defun cj/--ai-term-reuse-existing-agent (buffer _alist)
  "Display-buffer action: reuse any window in this frame already showing
an agent buffer.

Looks up `cj/--ai-term-displayed-agent-window' on the selected
frame.  When an agent window exists, replaces its buffer with BUFFER
and returns the window.  When none exists, returns nil so the next
action in the chain runs.

This is more specific than `display-buffer-use-some-window', which
would happily steal any non-selected window (e.g. a code window
above the agent split) when the user is focused in agent and
swaps projects via C-F9.  The selective lookup here keeps non-agent
windows undisturbed and preserves the user's split geometry across
project changes."
  (let ((win (cj/--ai-term-displayed-agent-window)))
    (when win
      (set-window-buffer win buffer)
      win)))

(defun cj/--ai-term-reuse-edge-window (buffer _alist)
  "Display-buffer action: reuse the existing window forming the target half.

When the frame already holds a window forming the half the agent would
occupy -- the right column on a desktop, the bottom row on a laptop, per
the saved or default direction -- swap BUFFER into it with
`set-window-buffer' and return that window, rather than splitting a third
window in.  The target half is found by `cj/window-at-edge'.

Returns nil when there is no such half to reuse (a single-window frame,
or a layout split on the other axis), so the chain falls through to
`cj/--ai-term-display-saved', which splits a fresh half.  Also returns
nil when the edge window is dedicated -- those are not ours to replace.

Records the displaced buffer through `display-buffer-record-window'
\(type `reuse') before swapping, so the native `quit-restore-window'
called at toggle-off puts that buffer back into the slot instead of
deleting the window -- toggling swaps the slot's buffer between the
displaced buffer and the agent, never changing the window count.

Runs after `cj/--ai-term-reuse-existing-agent', so an agent already on
screen has been handled already; the window reused here always holds a
non-agent buffer, which is replaced (it stays alive, just unshown).

Skipped entirely when the prior toggle-off deleted the agent's own split
window (`cj/--ai-term-last-toggle-deleted-split'): re-showing then reuses a
working window at the edge and collapses the layout.  Consume the flag and
return nil so `cj/--ai-term-display-saved' re-splits a fresh agent window,
keeping the toggle reversible."
  (if cj/--ai-term-last-toggle-deleted-split
      (progn (setq cj/--ai-term-last-toggle-deleted-split nil) nil)
    (let* ((direction (or cj/--ai-term-last-direction
                          (cj/--ai-term-default-direction)))
           (win (cj/window-at-edge direction)))
      (when (and win (not (window-dedicated-p win)))
        (display-buffer-record-window 'reuse win buffer)
        (set-window-buffer win buffer)
        win))))

(defun cj/--ai-term-display-saved (buffer alist)
  "Display-buffer action: split per saved direction and size.

When the prior toggle-off was a bury (single-window state, flagged
via `cj/--ai-term-last-was-bury') and the frame is still single-
window, restore the agent into the selected window in place rather
than splitting -- preserves the user's lone-window layout across
F9 toggles.

Otherwise delegates to `cj/window-toggle-display-saved' against the
F9 state vars, falling back to the host-aware defaults from
`cj/--ai-term-default-direction' and `cj/--ai-term-default-size'."
  (cond
   ((and cj/--ai-term-last-was-bury (one-window-p))
    (setq cj/--ai-term-last-was-bury nil)
    (let ((win (selected-window)))
      (set-window-buffer win buffer)
      win))
   (t
    (setq cj/--ai-term-last-was-bury nil)
    (cj/window-toggle-display-saved
     buffer alist
     'cj/--ai-term-last-direction (cj/--ai-term-default-direction)
     'cj/--ai-term-last-size (cj/--ai-term-default-size)))))

(defun cj/--ai-term-display-rule-list ()
  "Return the `display-buffer-alist' entry list installed by this module.

The single rule routes any buffer whose name starts with \"agent [\"
through four actions in order:

1. `display-buffer-reuse-window' -- if the same buffer is already
   visible in any window, focus that one.
2. `cj/--ai-term-reuse-existing-agent' -- otherwise, if any
   window in this frame already shows an agent-prefixed buffer,
   swap its buffer for the new one (preserves geometry across
   project changes via C-F9).
3. `cj/--ai-term-reuse-edge-window' -- otherwise, if the frame
   already has a window forming the half the agent would occupy
   (the right column on a desktop, the bottom row on a laptop),
   reuse it instead of splitting a third window in.
4. `cj/--ai-term-display-saved' -- otherwise (single-window frame,
   or a layout split on the other axis), split per the saved
   direction + size from the last toggle-off (or defaults when no
   capture has happened this session).

`display-buffer-in-side-window' is avoided deliberately.  Side
windows enforce dedication, which breaks `buffer-move' (C-M-arrows)
and `switch-to-buffer' replacement.  The chain above keeps the
resulting window an ordinary window so all standard window commands
work.

`display-buffer-use-some-window' is also avoided -- it would happily
steal any non-selected window (e.g. a code window above an agent
split) when the user is focused in agent and switches projects."
  '(("\\`agent \\["
     (display-buffer-reuse-window
      cj/--ai-term-reuse-existing-agent
      cj/--ai-term-reuse-edge-window
      cj/--ai-term-display-saved)
     (inhibit-same-window . t))))

(dolist (entry (cj/--ai-term-display-rule-list))
  (add-to-list 'display-buffer-alist entry))

(defun cj/--ai-term-send-string (buffer string)
  "Send STRING to BUFFER's terminal process (the agent's shell).
Sends to the pty directly so the launch command reaches the shell EAT runs."
  (let ((proc (get-buffer-process buffer)))
    (when (process-live-p proc)
      (process-send-string proc string))))

(defun cj/--ai-term-show-or-create (dir name)
  "Show or create the AI-term buffer for project DIR with buffer NAME.

If a buffer named NAME exists with a live process, display it.  If
the buffer exists but its process is dead, kill it and recreate.  If
no such buffer exists, create a new EAT terminal in DIR and send
the project's tmux launch command (see `cj/--ai-term-launch-command') so
the same project basename reattaches across Emacs restarts.

EAT runs a plain shell with no auto-tmux hook, so the named
`tmux new-session -A' launch command is the only thing that starts the
session -- the spike confirmed EAT + tmux detach and reattach exactly
like ghostel + tmux did.

Records DIR in `cj/--ai-term-mru' (whichever branch runs) so the
project picker can list recently-opened projects first.  Returns the
buffer."
  (cj/--ai-term-record-mru dir)
  (let ((existing (get-buffer name)))
    (cond
     ((and existing (cj/--ai-term-process-live-p existing))
      (display-buffer existing)
      existing)
     (t
      (when existing
        (kill-buffer existing))
      ;; `eat' switches to its buffer in the selected window before our
      ;; display-buffer-alist rule can route it; `save-window-excursion'
      ;; reverts that, and the explicit display-buffer below routes the buffer
      ;; through the alist into the agent slot.  `eat-buffer-name' is bound to
      ;; NAME so the terminal is created under the agent name; EAT (unlike
      ;; ghostel) does not rename the buffer from the terminal's OSC title, so
      ;; the "agent [" prefix that buffer detection and the display rule key on
      ;; stays put.
      (save-window-excursion
        (let ((default-directory dir)
              (eat-buffer-name name))
          (eat)))
      (let ((buf (get-buffer name)))
        (with-current-buffer buf
          (cj/--ai-term-send-string
           buf (concat (cj/--ai-term-launch-command dir) "\n")))
        (display-buffer buf)
        buf)))))

(defun cj/--ai-term-format-candidate (path &optional sessions)
  "Return the display name for PATH in the AI-term project picker.

Appends \" [running]\" when the project's agent buffer exists with
a live process; otherwise \" [detached]\" when PATH's tmux session
name is in SESSIONS (a session that survived an Emacs crash, no
buffer yet); otherwise just the abbreviated path.  Path is
abbreviated via `abbreviate-file-name' so it reads as ~/code/foo
rather than the full home-dir form."
  (let* ((name (cj/--ai-term-buffer-name path))
         (buf (get-buffer name))
         (running (and buf (cj/--ai-term-process-live-p buf)))
         (detached (and (not running)
                        (cj/--ai-term-session-active-p path sessions)))
         (display-path (abbreviate-file-name path)))
    (cond
     (running  (format "%s [running]" display-path))
     (detached (format "%s [detached]" display-path))
     (t        display-path))))

(defun cj/--ai-term-completion-table (alist)
  "Return a `completing-read' table over ALIST that pins candidate order.

`completing-read' over a bare alist lets the front-end (Vertico)
re-sort candidates by recency / length / alpha, which would defeat
the picker's active-sessions-first grouping.  Returning
`display-sort-function' and `cycle-sort-function' of `identity' in
the metadata keeps the order ALIST was built in."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action alist string predicate))))

(defun cj/--ai-term-pick-project ()
  "Prompt for an AI-agent project; return its absolute path.

Candidates come from `cj/--ai-term-candidates', ordered by
`cj/--ai-term-sort-candidates' so projects with a live tmux session
appear first (then alphabetical by abbreviated path).  Display uses
`cj/--ai-term-format-candidate', which abbreviates the path and
flags a live session via \" [running]\" (an Emacs terminal buffer is
alive) or \" [detached]\" (the tmux session survived, no buffer).
Signals `user-error' when no candidates exist."
  (let ((candidates (cj/--ai-term-candidates)))
    (unless candidates
      (user-error "No AI-agent projects found under %s"
                  (mapconcat #'identity
                             (append cj/ai-term-project-roots
                                     cj/ai-term-container-roots)
                             ", ")))
    (let* ((sessions (cj/--ai-term-live-tmux-sessions))
           (sorted (cj/--ai-term-sort-candidates candidates sessions))
           (display-alist
            (mapcar (lambda (p)
                      (cons (cj/--ai-term-format-candidate p sessions) p))
                    sorted))
           (chosen (completing-read
                    "AI terminal project: "
                    (cj/--ai-term-completion-table display-alist)
                    nil t)))
      (or (cdr (assoc chosen display-alist))
          (expand-file-name chosen)))))

(defun cj/--ai-term-dispatch ()
  "Compute the F9 (`cj/ai-term') action without performing it.

Returns one of:
- (toggle-off . WINDOW)        -- agent is displayed in WINDOW; quit it.
- (redisplay-recent . BUFFER)  -- 1+ alive agent buffers; show MRU.
- (pick-project)               -- zero alive agent buffers; prompt.

When 2+ agent buffers are alive, F9 redisplays the most-recently-
selected one rather than opening the project picker.  C-F9 is the
explicit \"start a different project\" surface; M-F9 is the explicit
\"switch among existing agents\" surface.  F9 keeps a single, simple
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

Bound to C-F9 -- always shows the project picker, even when an agent
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

(defun cj/--ai-term-swap-to-working-buffer (win)
  "In WIN, switch to the most-recent non-agent buffer (a working file).
Falls back to `other-buffer' (excluding WIN's current agent buffer) when no
non-agent buffer is on record.  Used at toggle-off and close so dismissing an
agent surfaces the file the user was working on rather than another agent or
the agent itself."
  (with-selected-window win
    (switch-to-buffer
     (or (cj/--ai-term-most-recent-non-agent-buffer)
         (other-buffer (window-buffer win) t)))))

(defun cj/--ai-term-toggle-off (win)
  "Hide the agent shown in WIN for an F9 toggle-off.  Always returns nil.

Two cases, by window count:

- Lone fullscreen agent (e.g. after `C-x 1' inside it): there is no prior
  layout for the native undo to restore and deleting would leave the frame
  empty.  Bury and flag, so the next toggle-on (`cj/--ai-term-display-saved')
  restores the agent in place at full frame rather than splitting.  Capture
  geometry for that restore.  `bury-buffer' can no-op when the window's
  prev-buffer history holds only the agent (common right after `C-x 1'), so
  force a swap to a non-agent buffer to keep the toggle observable.

- Multi-window: collapse the agent split outright by deleting its window, so
  the working buffer (e.g. todo.org) reclaims the space.  F9 is a pure
  show/hide toggle of THE agent split -- it must never surface a different
  agent.  `quit-restore-window' can't guarantee that here: switching among
  several agents reuses the one slot via `set-window-buffer' (see
  `cj/--ai-term-reuse-existing-agent'), which leaves the window's
  `quit-restore' parameter pointing at the FIRST agent shown.  Once it's
  stale, `quit-restore-window' falls back to `switch-to-prev-buffer' and
  surfaces another agent instead of removing the window -- exactly the \"F9
  shows another agent\" bug.  `delete-window' is unconditional and
  slot-history-independent.  Capture geometry first so the next toggle-on
  splits at the same size (the user's chosen split width is preserved)."
  ;; Remember which agent we're hiding so the next toggle-on reopens this
  ;; same one, not whichever agent is most-recent in `buffer-list'.
  (setq cj/--ai-term-last-hidden-buffer (window-buffer win))
  (cond
   ((one-window-p)
    (cj/--ai-term-capture-state win)
    (setq cj/--ai-term-last-was-bury t)
    (setq cj/--ai-term-last-toggle-deleted-split nil)
    (bury-buffer (window-buffer win))
    (when (and (window-live-p win)
               (cj/--ai-term-buffer-p (window-buffer win)))
      (cj/--ai-term-swap-to-working-buffer win)))
   (t
    (cj/--ai-term-capture-state win)
    (setq cj/--ai-term-last-was-bury nil)
    (if (and (window-live-p win)
             (> (length (window-list (window-frame win) 'never)) 1))
        (progn
          (delete-window win)
          ;; The agent had its own window in a multi-window layout, now gone:
          ;; the next toggle-on must re-split it rather than reuse a working
          ;; window at the edge (see `cj/--ai-term-reuse-edge-window').
          (setq cj/--ai-term-last-toggle-deleted-split t))
      ;; Degenerate fallback (window became sole between dispatch and
      ;; here): swap to a non-agent buffer rather than leave the agent up.
      (setq cj/--ai-term-last-toggle-deleted-split nil)
      (when (window-live-p win)
        (cj/--ai-term-swap-to-working-buffer win)))))
  nil)

(defun cj/ai-term (&optional arg)
  "Smart F9 dispatch for the AI-term launcher.

Behavior depends on the current state:

- If an AI-term buffer is currently displayed in this frame, F9
  quits its window (toggle off, buffer stays alive).
- Else, if exactly one alive AI-term buffer exists, F9 re-displays
  it (DWIM -- the obvious next step is to look at it).
- Else (zero or 2+), F9 falls through to `cj/ai-term-pick-project'.

With prefix ARG, display the buffer without selecting its window
when a buffer is being shown (no effect on the toggle-off branch).

See `cj/ai-term-pick-project' (C-F9) to force the project picker.
M-F9 closes an agent via `cj/ai-term-close'."
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

(defun cj/--ai-term-kill-tmux-session (session)
  "Kill the tmux SESSION via `tmux kill-session -t SESSION'.

Returns the process exit status (0 on success), or nil when tmux is
unavailable or already gone -- a session that no longer exists is not
an error worth surfacing, since the goal is just to make sure it's
down."
  (condition-case nil
      (process-file "tmux" nil nil nil "kill-session" "-t" session)
    (error nil)))

(defun cj/--ai-term-close-buffer (buffer)
  "Gracefully tear down AI-term BUFFER: tmux session, then buffer.

Derives the tmux session name from BUFFER's `default-directory' (the
project dir the terminal was created in) and kills it so the agent
process stops.  When BUFFER is shown, swaps its window to a non-agent
buffer (the working file) rather than deleting the window -- closing an
agent must not collapse the user's window layout; the F9 hide toggle is
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

(require 'system-lib)

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
interrupt work in progress.  Bound to M-<f9>."
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
erroring.

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
    (if (not next-dir)
        ;; No agents open: launch the first via the project picker instead of
        ;; erroring, so the swap key doubles as a "start an agent" key.
        (cj/ai-term-pick-project)
      (let* ((name (cj/--ai-term-buffer-name next-dir))
             (existing (get-buffer name)))
        ;; Live agent and an agent window is up: swap it into that window in
        ;; place (faithful to the prior buffer-only behavior).  Detached, or no
        ;; window yet: show-or-create attaches the tmux session / displays it.
        (if (and win existing (cj/--ai-term-process-live-p existing))
            (progn (set-window-buffer win existing) (select-window win))
          (cj/--ai-term-show-or-create next-dir name)
          (let ((w (get-buffer-window name)))
            (when w (select-window w))))
        (message "Agent: %s" name)))))

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

;; In EAT's semi-char mode, keys not bound in `eat-semi-char-mode-map' are
;; forwarded to the pty.  M-SPC (swap to the next agent) must reach Emacs from
;; inside an agent buffer, so bind it in that map -- no exception-list or rebuild
;; dance like ghostel needed.  C-; is already bound there (eat-config), so the
;; C-; a family resolves through the global prefix without extra wiring.
(with-eval-after-load 'eat
  (keymap-set eat-semi-char-mode-map "M-SPC" #'cj/ai-term-next))

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

;; ---------- emacsclient: keep opened files off the agent terminal ----------
;;
;; `server-start' (in system-defaults.el) leaves `server-window' nil, so
;; `server-switch-buffer' opens an `emacsclient -n' file in the *selected*
;; window.  When the user is typing in the agent terminal, that's the agent
;; window -- so "tell the agent to open X" would replace the agent buffer
;; with X.  The function below, wired as `server-window', routes such files
;; into a non-agent window instead (splitting one off the agent when the
;; agent is the only window).  emacsclient invocations from anywhere else
;; fall through to `pop-to-buffer' and behave as before.

(defun cj/--ai-term-non-agent-window (&optional exclude)
  "Return a window in the selected frame fit to show a non-agent buffer.

Skips the minibuffer, the EXCLUDE window, dedicated windows, and any
window already showing an AI-term agent buffer.  Returns nil when no
such window exists."
  (seq-find (lambda (w)
              (and (not (eq w exclude))
                   (not (window-dedicated-p w))
                   (not (cj/--ai-term-buffer-p (window-buffer w)))))
            (window-list (selected-frame) 'never)))

(defun cj/--ai-term-server-display (buffer)
  "Display BUFFER for `server-window', keeping it off the agent terminal.

When the selected window shows an AI-term agent buffer, put BUFFER in
a non-agent window (`cj/--ai-term-non-agent-window'), splitting a
left-side window off the agent when the agent is the only window, then
select that window.  Otherwise hand off to `pop-to-buffer'.  Returns
the window BUFFER ends up in -- the value `server-switch-buffer'
expects from a `server-window' function."
  (if (cj/--ai-term-buffer-p (window-buffer (selected-window)))
      (let* ((agent-win (selected-window))
             (target (or (cj/--ai-term-non-agent-window agent-win)
                         (split-window agent-win nil 'left))))
        (set-window-buffer target buffer)
        (select-window target))
    (pop-to-buffer buffer)
    (selected-window)))

(defvar server-window)
(with-eval-after-load 'server
  (setq server-window #'cj/--ai-term-server-display))

(provide 'ai-term)
;;; ai-term.el ends here
