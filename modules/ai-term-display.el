;;; ai-term-display.el --- AI-term window and display policy -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: library.
;; Top-level side effects: installs the agent display-buffer-alist rule, adds
;;   the geometry tracker to window-configuration-change-hook, and wires
;;   `server-window' after server loads.
;; Runtime requires: seq, cj-window-geometry-lib, cj-window-toggle-lib,
;;   ai-term-sessions.
;; Direct test load: yes.
;;
;; Display/window layer of ai-term: the display-buffer action chain and its
;; alist rule, the toggle capture/restore state and geometry tracking, the
;; toggle-off teardown, the working-buffer swap, and the server-window
;; routing that keeps emacsclient-opened files off the agent terminal.
;; No EAT and no tmux -- window behavior here is exercisable with plain
;; buffers and stubbed session/backend functions.
;;
;; The size defaults this layer reads (`cj/ai-term-desktop-width',
;; `cj/ai-term-laptop-height') are owned by ai-term.el, the public face,
;; which requires this module before defining them; they are
;; forward-declared here so this module compiles and reads them without a
;; cycle.

;;; Code:

(require 'seq)
(require 'cj-window-geometry-lib)
(require 'cj-window-toggle-lib)
(require 'ai-term-sessions)

;; Owned by ai-term.el (the public face's defcustoms); forward-declared so
;; this module compiles and reads them without a cycle.
(defvar cj/ai-term-desktop-width)
(defvar cj/ai-term-laptop-height)

(defun cj/--ai-term-displayed-agent-window (&optional frame)
  "Return a window in FRAME currently displaying an AI-term buffer, or nil.

FRAME defaults to the selected frame.  When more than one window in
the frame shows an agent buffer, the first one in `window-list' order
is returned.  The minibuffer is excluded from the search."
  (seq-find (lambda (w)
              (cj/--ai-term-buffer-p (window-buffer w)))
            (window-list (or frame (selected-frame)) 'never)))

(defun cj/--ai-term-most-recent-non-agent-buffer ()
  "Return the most-recently-selected live non-agent buffer, or nil.

Walks `buffer-list' (most-recently-selected first) and returns the
first buffer that is not an AI-term agent buffer (per
`cj/--ai-term-buffer-p') and is not an internal buffer (name starting
with a space).  Used by the single-window toggle-off so dismissing a
full-frame agent returns to the file the user was working in (e.g.
todo.org) rather than swapping in another agent."
  (seq-find (lambda (b)
              (and (buffer-live-p b)
                   (not (cj/--ai-term-buffer-p b))
                   (not (string-prefix-p " " (buffer-name b)))))
            (buffer-list)))

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
  "Non-nil when the last toggle-off used `bury-buffer'.

Set by `cj/ai-term' in its `toggle-off' branch: t when the agent
window was the only window in the frame (so toggle-off buried
without deleting), nil when the window was deleted.  Consumed by
`cj/--ai-term-display-saved' to decide between restoring the
buried agent in the current window (the only one) or splitting per
the saved direction.")

(defvar cj/--ai-term-last-toggle-deleted-split nil
  "Non-nil when the last toggle-off deleted the agent's own split window.

Set t by `cj/--ai-term-toggle-off' only when it actually `delete-window's
the agent (a multi-window layout where the agent had its own window);
nil for a bury or a degenerate swap.  Consumed by
`cj/--ai-term-reuse-edge-window': when set, the next toggle-on re-splits a
fresh agent window instead of reusing a window at the edge.  Without this,
toggling the agent off and on in a 3+ window layout would reuse the user's
working window at the edge, displacing its buffer and collapsing the layout
-- the toggle must be reversible (off then on returns the same windows).")

(defvar cj/--ai-term-last-hidden-buffer nil
  "The agent buffer hidden by the most recent toggle-off.

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

(defvar cj/--ai-term-last-fullscreen nil
  "Non-nil when the agent window was last seen filling its frame.

Maintained by `cj/--ai-term-track-geometry' on
`window-configuration-change-hook': set t whenever a live agent window is
the sole window in its frame, cleared when the agent is shown as a split
\(its dock direction and size are captured then instead).  Consulted by
`cj/--ai-term-display-saved' so a summon into a single-window frame
restores the agent fullscreen rather than docking it -- the sole-window
state isn't a representable dock size, so this flag is how it round-trips.
Unlike `cj/--ai-term-last-was-bury' it does not depend on a toggle-off, so
it also covers leaving the agent by switching buffers or `C-x 1'.")

(defun cj/--ai-term-capture-state (window)
  "Capture WINDOW's direction and size into module-level state.

Sets `cj/--ai-term-last-direction' and `cj/--ai-term-last-size'
so a subsequent display can restore the user's chosen orientation
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

(defun cj/--ai-term-window-sole-p (window)
  "Return non-nil when WINDOW is the only live window in its frame.
A frame's sole window is its root window; once split, the root is an
internal window and no live window equals it."
  (and (window-live-p window)
       (eq window (frame-root-window (window-frame window)))))

(defun cj/--ai-term-track-geometry (&rest _)
  "Track whether the displayed agent window is fullscreen.

Run from `window-configuration-change-hook'.  Sets
`cj/--ai-term-last-fullscreen' to whether a live agent window is the sole
window in its frame, and leaves it untouched when no agent window is
displayed -- that retained value is the just-left state a later summon
replays.  Dock direction and size stay owned by the toggle-off capture
\(`cj/--ai-term-capture-state'); this hook must not re-capture them, or the
repeated capture/replay drifts the dock height a couple rows per cycle."
  (let ((win (cj/--ai-term-displayed-agent-window)))
    (when (window-live-p win)
      (setq cj/--ai-term-last-fullscreen (cj/--ai-term-window-sole-p win)))))

(add-hook 'window-configuration-change-hook #'cj/--ai-term-track-geometry)

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
swaps projects via C-; a s.  The selective lookup here keeps non-agent
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
  "Display-buffer action: restore fullscreen in a single-window frame,
otherwise split per saved direction and size.

When the frame is a single window and the agent was last fullscreen
\(`cj/--ai-term-last-fullscreen', tracked by `cj/--ai-term-track-geometry')
or the prior toggle-off was a single-window bury
\(`cj/--ai-term-last-was-bury'), restore the agent into the selected window
in place rather than splitting.  This round-trips a fullscreen agent --
left by toggle-off, `C-x 1', or switching buffers -- since the sole-window
state isn't a representable dock size.

Otherwise delegates to `cj/window-toggle-display-saved' against the
toggle state vars, falling back to the host-aware defaults from
`cj/--ai-term-default-direction' and `cj/--ai-term-default-size'."
  (cond
   ;; NOMINI t: don't count an active minibuffer as a second window.  A summon
   ;; can run with a picker prompt up, and a bare `one-window-p' then returns
   ;; nil on a structurally single-window frame, misfiring the fullscreen
   ;; restore into a dock -- which clears the fullscreen flag and cascades.
   ((and (or cj/--ai-term-last-fullscreen cj/--ai-term-last-was-bury)
         (one-window-p t))
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
   project changes via C-; a s).
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
  "Hide the agent shown in WIN for a toggle-off.  Always returns nil.

Two cases, by window count:

- Lone fullscreen agent (e.g. after `C-x 1' inside it): there is no prior
  layout for the native undo to restore and deleting would leave the frame
  empty.  Bury and flag, so the next toggle-on (`cj/--ai-term-display-saved')
  restores the agent in place at full frame rather than splitting.  Capture
  geometry for that restore.  `bury-buffer' can no-op when the window's
  prev-buffer history holds only the agent (common right after `C-x 1'), so
  force a swap to a non-agent buffer to keep the toggle observable.

- Multi-window: collapse the agent split outright by deleting its window, so
  the working buffer (e.g. todo.org) reclaims the space.  The toggle is a pure
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

(provide 'ai-term-display)
;;; ai-term-display.el ends here
