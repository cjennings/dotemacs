;;; org-agenda-frame.el --- Dedicated agenda frame -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D.
;; Load shape: eager (binds keys in Phase 2; Phase 1 defines helpers only).
;; Top-level side effects: none yet (Phase 1 is private helpers).
;; Runtime requires: none.
;; Direct test load: yes.
;;
;; A dedicated Emacs frame of the running daemon that shows a today-anchored
;; seven-day org-agenda, refreshing itself, kept read-only and focus-locked.
;; A normal (non-fullscreen) frame, so a tiling WM places it side by side with
;; the working frame.  Spawned/raised/closed by one key.  See the spec:
;; docs/specs/2026-07-17-org-agenda-fullscreen-frame-spec.org.
;;
;; Phase 1 (this pass) builds non-interactive helpers only: frame lookup,
;; spawn/raise/delete, the dedicated view, the default-deny read-only policy,
;; and working-frame routing.  No interactive command or key is bound until
;; Phase 2, so nothing user-visible changes yet.

;;; Code:

(require 'seq)

;; Declared, not required: `org-agenda-config' pulls in vc packages that don't
;; load under `make test' (no package-initialize).  The frame module references
;; org-agenda symbols through these declarations and does its real wiring inside
;; `with-eval-after-load' so a batch test-load runs no org-agenda side effects.
(defvar org-agenda-custom-commands)
(defvar org-agenda-finalize-hook)
(defvar org-agenda-sticky)
(defvar org-agenda-window-setup)
(declare-function cj/build-org-agenda-list "org-agenda-config" (&optional force-rebuild))
(declare-function cj/org-agenda-refresh-files "org-agenda-config" ())
(declare-function org-agenda-redo "org-agenda" (&optional all))
(declare-function org-agenda-next-line "org-agenda" ())
(declare-function org-agenda-previous-line "org-agenda" ())
(declare-function org-agenda-next-item "org-agenda" (n))
(declare-function org-agenda-previous-item "org-agenda" (n))
(declare-function org-agenda-open-link "org-agenda" (&optional arg))
(declare-function org-get-at-bol "org" (property))
(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-agenda "org-agenda" (&optional arg org-keys restriction))
;; Forward references: defined later in this file (Phase 2 for -safe-redo,
;; the lifecycle block for -delete).
(declare-function cj/--agenda-frame-safe-redo "org-agenda-frame" ())
(declare-function cj/--agenda-frame-delete "org-agenda-frame" ())

(defconst cj/--agenda-frame-parameter 'cj/agenda-frame
  "Frame parameter marking the dedicated agenda frame.
Its presence (non-nil) is how `cj/--agenda-frame' locates the frame among
all of the daemon's frames.")

(defvar cj/--agenda-frame-launch-frame nil
  "The frame selected when the agenda frame was last spawned.
Preferred routing target for source files opened from the agenda (see
`cj/--agenda-frame-working-frame'); ignored once it is dead or is itself
the agenda frame.")

(defun cj/--agenda-frame-p (frame)
  "Return non-nil when FRAME is a live agenda frame.
FRAME is an agenda frame when it is live and carries the
`cj/--agenda-frame-parameter' marker; a dead frame is never one."
  (and (frame-live-p frame)
       (frame-parameter frame cj/--agenda-frame-parameter)))

(defun cj/--agenda-frame ()
  "Return the live agenda frame, or nil.
The frame is identified by the `cj/--agenda-frame-parameter' marker; a
dead frame is never returned even if it still carries the marker."
  (seq-find #'cj/--agenda-frame-p (frame-list)))

(defun cj/--agenda-frame-working-frame ()
  "Return a live non-agenda frame to route source files into, or nil.
Prefer `cj/--agenda-frame-launch-frame' when it is still live and not the
agenda frame; otherwise the first live non-agenda frame among all frames.
Return nil when the agenda frame is the only live frame -- the caller then
creates a normal frame."
  (or (and (frame-live-p cj/--agenda-frame-launch-frame)
           (not (cj/--agenda-frame-p cj/--agenda-frame-launch-frame))
           cj/--agenda-frame-launch-frame)
      (seq-find (lambda (frame)
                  (and (frame-live-p frame)
                       (not (cj/--agenda-frame-p frame))))
                (frame-list))))

;;; The dedicated seven-day view (org-agenda-custom-commands key F)

(defconst cj/--agenda-frame-command-key "F"
  "The `org-agenda-custom-commands' key for the agenda-frame view.
The existing top-level key is `d' (org-agenda-config.el:344), so `F' is
collision-free.  The sticky buffer derives its name from this key
\(*Org Agenda(F)*).")

(defun cj/--agenda-frame-command ()
  "Return the `org-agenda-custom-commands' entry for the agenda frame.
A one-block agenda: a seven-day span anchored to today rather than
Monday (`org-agenda-list' otherwise anchors any seven-day span to the
week start in Org 9.7.11), rendered in the frame's sole window
\(`current-window', so Org's default `reorganize-frame' can't split it),
as its own sticky *Org Agenda(F)* buffer, with follow-mode forced off so
a non-nil global default can't open a second window at build time."
  `(,cj/--agenda-frame-command-key "Agenda frame: 7-day today-anchored"
    ((agenda ""
             ((org-agenda-span 7)
              (org-agenda-start-day "0d")
              (org-agenda-start-on-weekday nil)
              (org-agenda-start-with-follow-mode nil)
              ;; Narrow category column: the global agenda format pads the
              ;; category to 25 chars, leaving a wide blank gutter between
              ;; the source name (todo:, dcal:) and the item.
              (org-agenda-prefix-format "  %i %-10:c%?-12t% s"))))
    ;; No `org-agenda-sticky' here, deliberately: these settings are baked
    ;; into the buffer's series-redo-cmd and re-applied by every redo, and a
    ;; sticky t mid-redo makes `org-agenda-use-sticky-p' true while the
    ;; buffer exists -- `org-agenda-prepare' then throws \\='exit ("use `r'
    ;; to refresh") with no catch, failing every refresh tick.  Stickiness
    ;; is bound in the spawn wrapper instead, where it names the buffer.
    ((org-agenda-window-setup 'current-window))))

(defun cj/--agenda-frame-register-command ()
  "Register the agenda-frame view in `org-agenda-custom-commands'.
Idempotent: any existing entry for `cj/--agenda-frame-command-key' is
replaced, so a module reload never accumulates duplicate keys."
  (setq org-agenda-custom-commands
        (cons (cj/--agenda-frame-command)
              (assoc-delete-all cj/--agenda-frame-command-key
                                org-agenda-custom-commands))))

;;; Engage routing — open the item's source outside the agenda frame

(defun cj/--agenda-frame-item-marker ()
  "Return the source marker for the agenda item at point, or nil.
Prefers the item's own marker, falling back to the heading marker.  Reads
the text property directly (like `org-get-at-bol') so point restoration
is exercisable without loading org."
  (or (get-text-property (line-beginning-position) 'org-marker)
      (get-text-property (line-beginning-position) 'org-hd-marker)))

(defun cj/--agenda-frame-target-frame ()
  "Return the frame to open agenda source in, creating one when needed.
The engage action never opens into the agenda frame: it targets the
working frame (`cj/--agenda-frame-working-frame'), and when the agenda
frame is the only live frame it creates a normal, non-fullscreen frame."
  (or (cj/--agenda-frame-working-frame)
      (make-frame)))

(defun cj/--agenda-frame-engage-open ()
  "Open the source of the agenda item at point in the working frame.
Routes to the MRU non-agenda frame (or a new normal frame when the agenda
frame is the only one), so the agenda frame keeps showing the agenda.
Signals a `user-error' when point is not on an agenda item."
  (interactive)
  (let ((marker (cj/--agenda-frame-item-marker)))
    (unless (and marker (marker-buffer marker))
      (user-error "No agenda item on this line"))
    (let ((buffer (marker-buffer marker))
          (pos (marker-position marker))
          (frame (cj/--agenda-frame-target-frame)))
      (select-frame-set-input-focus frame)
      (pop-to-buffer-same-window buffer)
      (widen)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
        (org-fold-show-context 'agenda))
      (beginning-of-line))))

(defun cj/--agenda-frame-engage-mouse (event)
  "Open the agenda item clicked by EVENT in the working frame."
  (interactive "e")
  (mouse-set-point event)
  (cj/--agenda-frame-engage-open))

(defun cj/--agenda-frame-open-link ()
  "Follow the link in the agenda item at point, in the working frame."
  (interactive)
  (select-frame-set-input-focus (cj/--agenda-frame-target-frame))
  (org-agenda-open-link))

(defun cj/--agenda-frame-close ()
  "Close the agenda frame from within it.
Bound to q, Q, and x so Org's own quit keys delete the whole frame
\(and cancel its timer) rather than leaving a sole-window agenda
frame stranded on a non-agenda buffer."
  (interactive)
  (cj/--agenda-frame-delete))

;;; Default-deny read-only policy

(defconst cj/--agenda-frame-readonly-message
  "Agenda frame is read-only — press RET to edit in your working frame"
  "Shown when a mutating or buffer-opening command is denied in the frame.")

(defconst cj/--agenda-frame-fixed-view-message
  "Agenda frame is fixed to the 7-day view"
  "Shown when a view-changing command is denied in the frame.")

(defun cj/--agenda-frame-denied-readonly ()
  "Deny a mutating or buffer-opening command in the agenda frame.
The default binding for every key not on the allowlist."
  (interactive)
  (message "%s" cj/--agenda-frame-readonly-message))

(defun cj/--agenda-frame-denied-fixed-view ()
  "Deny a view-changing command that would break the today-anchored span."
  (interactive)
  (message "%s" cj/--agenda-frame-fixed-view-message))

(defvar cj/agenda-frame-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Default-deny: every key/mouse event not rebound below funnels through
    ;; this one catch-all, so mutations (present and future) are read-only.
    (define-key map [t] #'cj/--agenda-frame-denied-readonly)
    ;; Hide the Org Agenda menu-bar entry so there is no menu path to a mutation.
    (define-key map [menu-bar org-agenda] #'undefined)
    ;; (a) Navigation — allowlisted to their org-agenda commands.
    (define-key map (kbd "n") #'org-agenda-next-line)
    (define-key map (kbd "p") #'org-agenda-previous-line)
    (define-key map (kbd "<down>") #'org-agenda-next-line)
    (define-key map (kbd "<up>") #'org-agenda-previous-line)
    (define-key map (kbd "C-n") #'org-agenda-next-line)
    (define-key map (kbd "C-p") #'org-agenda-previous-line)
    (define-key map (kbd "N") #'org-agenda-next-item)
    (define-key map (kbd "P") #'org-agenda-previous-item)
    (define-key map (kbd "C-v") #'scroll-up-command)
    (define-key map (kbd "M-v") #'scroll-down-command)
    (define-key map (kbd "M-<") #'beginning-of-buffer)
    (define-key map (kbd "M->") #'end-of-buffer)
    ;; Read-only point motion and search within the agenda.
    (define-key map (kbd "C-a") #'move-beginning-of-line)
    (define-key map (kbd "C-e") #'move-end-of-line)
    (define-key map (kbd "C-f") #'forward-char)
    (define-key map (kbd "C-b") #'backward-char)
    (define-key map (kbd "C-s") #'isearch-forward)
    (define-key map (kbd "C-r") #'isearch-backward)
    (define-key map (kbd "C-g") #'keyboard-quit)
    ;; (b) Engage / open — routed to the working frame, never the agenda frame.
    ;; Bind the GUI function-key events ([return]/[tab]) as well as the ASCII
    ;; forms: the [t] catch-all otherwise gives `return'/`tab' a binding, which
    ;; suppresses their function-key translation to RET/TAB, so a bare RET would
    ;; hit the deny handler instead of engaging in a graphical frame.
    (define-key map (kbd "RET") #'cj/--agenda-frame-engage-open)
    (define-key map (kbd "TAB") #'cj/--agenda-frame-engage-open)
    (define-key map [return] #'cj/--agenda-frame-engage-open)
    (define-key map [tab] #'cj/--agenda-frame-engage-open)
    (define-key map (kbd "<mouse-2>") #'cj/--agenda-frame-engage-mouse)
    (define-key map (kbd "C-c C-o") #'cj/--agenda-frame-open-link)
    ;; (c) The frame's own controls.
    (define-key map (kbd "q") #'cj/--agenda-frame-close)
    (define-key map (kbd "Q") #'cj/--agenda-frame-close)
    (define-key map (kbd "x") #'cj/--agenda-frame-close)
    (define-key map (kbd "r") #'cj/--agenda-frame-safe-redo)
    (define-key map (kbd "S-<f8>") #'cj/agenda-frame-toggle)
    (define-key map (kbd "C-M-<f8>") #'cj/org-agenda-refresh-files)
    ;; C-x C-c means "close this frame" here.  The global
    ;; `save-buffers-kill-terminal' must never run in this frame: it was made
    ;; by `make-frame', not emacsclient, so with no client to close it falls
    ;; back to killing the daemon itself.
    (define-key map (kbd "C-x C-c") #'cj/--agenda-frame-close)
    ;; (d) Input machinery punched through the catch-all.  An explicit nil
    ;; shadows the [t] default in this map, so these fall through to their
    ;; global bindings.  Without the punches, every frame-focus change
    ;; (switch-frame), every wheel scroll, and every mouse click hits the
    ;; deny handler -- message spam and broken frame switching.
    (dolist (key (list [switch-frame]
                       [wheel-up] [wheel-down] [wheel-left] [wheel-right]
                       [double-wheel-up] [double-wheel-down]
                       [triple-wheel-up] [triple-wheel-down]
                       [mouse-1] [down-mouse-1] [drag-mouse-1]
                       (kbd "C-h")))
      (define-key map key nil))
    ;; View-changers get the distinct fixed-view message, not the read-only one.
    (dolist (key '("w" "d" "y" "f" "b" "j" "g"))
      (define-key map (kbd key) #'cj/--agenda-frame-denied-fixed-view))
    map)
  "Keymap for `cj/agenda-frame-mode'.
Shadows `org-agenda-mode-map' by default-deny: the `[t]' catch-all denies
every key that is not explicitly allowlisted here, so a future Org binding
is denied by default and there is nothing to keep in sync.")

(define-minor-mode cj/agenda-frame-mode
  "Read-only, focus-locked policy for the dedicated agenda frame.
Only the allowlist in `cj/agenda-frame-mode-map' is permitted: navigation,
the engage/open keys (routed to the working frame), and the frame's own
controls.  Every other key/mouse command is denied.  The enforcement
boundary is keys and mouse; a direct \\[execute-extended-command] is out
of contract."
  :init-value nil
  :lighter " AgendaFrame"
  :keymap cj/agenda-frame-mode-map)

(defun cj/--agenda-frame-maybe-enable-mode ()
  "Re-enable `cj/agenda-frame-mode' after an agenda build in the agenda frame.
Added to `org-agenda-finalize-hook'.  `org-agenda-redo' rebuilds through
`org-agenda-mode', whose `kill-all-local-variables' strips the buffer-local
minor mode; this reinstates it whenever the just-built buffer is displayed
in the frame carrying the `cj/agenda-frame' marker (a frame parameter, which
survives the buffer reset).  Ordinary agenda builds in working frames are
left untouched.

The same reset also strips the buffer-local `kill-buffer-hook' installed at
spawn, so it is re-added here too -- otherwise, after the first refresh
tick, killing the buffer would no longer delete the frame."
  (let ((frame (cj/--agenda-frame)))
    (when (and frame (get-buffer-window (current-buffer) frame))
      (cj/agenda-frame-mode 1)
      (add-hook 'kill-buffer-hook #'cj/--agenda-frame-on-kill-buffer nil t))))

;;; Frame lifecycle — spawn, raise, delete, toggle, cleanup

(defconst cj/--agenda-frame-timer-parameter 'cj/agenda-frame-timer
  "Frame parameter holding the agenda frame's refresh timer (set in Phase 2).")

(defvar cj/--agenda-frame-tearing-down nil
  "Non-nil while the agenda frame is being torn down.
Breaks the `delete-frame' / `kill-buffer-hook' re-entrancy loop: deleting
the frame kills its buffer and killing the buffer deletes the frame, so
whichever fires first sets this to skip the other.")

(defun cj/--agenda-frame-sticky-buffer ()
  "Return the dedicated *Org Agenda(F)* sticky buffer, or nil if none."
  (get-buffer (format "*Org Agenda(%s)*" cj/--agenda-frame-command-key)))

(defun cj/--agenda-frame-cancel-timer (&optional frame)
  "Cancel and clear the refresh timer on FRAME (default: the agenda frame).
Safe when no timer is set or FRAME is dead.  Returns nil."
  (let* ((frame (or frame (cj/--agenda-frame)))
         (timer (and (frame-live-p frame)
                     (frame-parameter frame cj/--agenda-frame-timer-parameter))))
    (when (timerp timer)
      (cancel-timer timer))
    (when (frame-live-p frame)
      (set-frame-parameter frame cj/--agenda-frame-timer-parameter nil))
    nil))

(defun cj/--agenda-frame-on-delete-frame (frame)
  "Clean up when the agenda FRAME dies by any path.
Registered on `delete-frame-functions': cancels the refresh timer and
kills the dedicated sticky buffer, so the next spawn regenerates fresh
rather than reusing stale sticky content.  A non-agenda frame is ignored."
  (when (cj/--agenda-frame-p frame)
    (cj/--agenda-frame-cancel-timer frame)
    (let ((buffer (cj/--agenda-frame-sticky-buffer))
          (cj/--agenda-frame-tearing-down t))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun cj/--agenda-frame-on-kill-buffer ()
  "Delete the agenda frame when its dedicated buffer is killed.
A buffer-local `kill-buffer-hook' on the sticky buffer, so killing it from
anywhere takes the frame with it.  Guarded against re-entry during a
frame-initiated teardown."
  (unless cj/--agenda-frame-tearing-down
    (let ((frame (cj/--agenda-frame)))
      (when (frame-live-p frame)
        (delete-frame frame)))))

(defun cj/--agenda-frame-delete ()
  "Delete the agenda frame; a no-op when none exists.
`delete-frame' fires `cj/--agenda-frame-on-delete-frame', which cancels
the timer and kills the sticky buffer."
  (let ((frame (cj/--agenda-frame)))
    (when (frame-live-p frame)
      (delete-frame frame))))

(defun cj/--agenda-frame-raise (frame)
  "Raise FRAME and give it input focus.  Returns FRAME."
  (select-frame-set-input-focus frame)
  frame)

(defun cj/--agenda-frame-make-parameters ()
  "Return the frame parameters for the dedicated agenda frame.
A normal frame -- not fullscreen -- so a tiling window manager places it
side by side with the working frame rather than covering the whole output.
It carries the `cj/agenda-frame' marker and a distinct, noticeable name
\(\"Full Agenda\") so the frame is recognizable at a glance and
window-manager rules can target it."
  `((,cj/--agenda-frame-parameter . t)
    (name . "Full Agenda")))

(defun cj/--agenda-frame-spawn ()
  "Create, display, and focus the dedicated agenda frame.
Transactional: on any failure after `make-frame', delete the partial
frame (which cleans up its buffer and timer via the delete hook), restore
focus to the launching frame, and signal a `user-error' naming the cause.
Returns the new agenda frame on success."
  (let ((launch (selected-frame))
        (frame nil))
    (condition-case err
        (progn
          (setq cj/--agenda-frame-launch-frame launch)
          (setq frame (make-frame (cj/--agenda-frame-make-parameters)))
          (select-frame-set-input-focus frame)
          ;; Cached, non-forced: a frame spawned early after daemon startup
          ;; still shows the full project agenda, not the base-files-only view.
          (cj/build-org-agenda-list)
          ;; Bind sticky + current-window dynamically around the render.  The
          ;; custom command's own settings apply too late to name the buffer;
          ;; without these the buffer is plain *Org Agenda*, which matches the
          ;; 0.75 below-selected display rule in org-agenda-config.el -- the
          ;; new frame gets split with the launch buffer left in the top 25%.
          ;; Sticky names it *Org Agenda(F)*, which no display rule matches.
          (let ((org-agenda-sticky t)
                (org-agenda-window-setup 'current-window))
            (org-agenda "a" cj/--agenda-frame-command-key))
          ;; Belt: whatever a display rule did, the frame is one agenda window.
          (delete-other-windows)
          (let ((buffer (cj/--agenda-frame-sticky-buffer)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (add-hook 'kill-buffer-hook
                          #'cj/--agenda-frame-on-kill-buffer nil t))))
          (cj/--agenda-frame-start-timer frame)
          frame)
      (error
       (when (frame-live-p frame)
         (delete-frame frame))
       (when (frame-live-p launch)
         (select-frame-set-input-focus launch))
       (user-error "Agenda frame: spawn failed: %s"
                   (error-message-string err))))))

(defun cj/--agenda-frame-toggle ()
  "Spawn, raise, or delete the dedicated agenda frame.
Spawn when none exists, delete when the agenda frame is the selected
frame, raise and focus it otherwise.

Non-interactive by design in Phase 1: reachable only from ERT, never from
\\[execute-extended-command] or a key.  Phase 2 wraps this in the public
`cj/agenda-frame-toggle' and binds it to S-<f8>."
  (let ((frame (cj/--agenda-frame)))
    (cond
     ((null frame) (cj/--agenda-frame-spawn))
     ((eq frame (selected-frame)) (cj/--agenda-frame-delete) nil)
     (t (cj/--agenda-frame-raise frame)))))

;;; Phase 2 — refresh timer, snapshot restore, and the public command

(defconst cj/--agenda-frame-refresh-seconds 300
  "Refresh cadence for the agenda frame, in seconds (five minutes).")

(defconst cj/--agenda-frame-fail-count-parameter 'cj/agenda-frame-fail-count
  "Frame parameter holding the consecutive-failure count for the refresh timer.")

(defconst cj/--agenda-frame-overlay-property 'cj/agenda-frame-failure
  "Overlay property tagging the refresh-failed banner.
The banner is found by scanning for this property, never held in a
buffer-local variable: `org-agenda-redo' runs `kill-all-local-variables',
which would wipe the variable while the overlay object survives
`erase-buffer' -- leaving a banner nothing could ever remove.")

(defun cj/--agenda-frame-seconds-to-next-mark (time period)
  "Return seconds from TIME to the next wall-clock multiple of PERIOD.
TIME is any Emacs time value, PERIOD is seconds (300 gives the :00/:05
marks).  A TIME exactly on a mark returns a full PERIOD, so the timer
never fires twice back-to-back."
  (let ((rem (mod (floor (float-time time)) period)))
    (if (zerop rem) period (- period rem))))

;; -- Point restoration -------------------------------------------------------

(defun cj/--agenda-frame-goto-first-item ()
  "Move point to the first agenda item, or `point-min' when the view is empty."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (get-text-property (line-beginning-position) 'org-marker)
          (setq found t)
        (forward-line 1)))
    (unless found (goto-char (point-min)))))

(defun cj/--agenda-frame-restore-point (old-marker old-line)
  "Restore point in the rebuilt agenda buffer after a redo.
Prefer the line whose org-marker points at the same source location as
OLD-MARKER, choosing the occurrence nearest OLD-LINE when a source line
appears twice.  When the marker is gone, clamp OLD-LINE into range; if
that lands on a header (no item), move to the first item; an item-less
view leaves point at buffer start."
  (let ((max-line (line-number-at-pos (point-max)))
        (targets '()))
    (when (and (markerp old-marker) (marker-buffer old-marker))
      (let ((src-buf (marker-buffer old-marker))
            (src-pos (marker-position old-marker)))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((m (get-text-property (line-beginning-position) 'org-marker)))
              (when (and (markerp m)
                         (eq (marker-buffer m) src-buf)
                         (eql (marker-position m) src-pos))
                (push (line-number-at-pos) targets)))
            (forward-line 1)))))
    (cond
     (targets
      (let ((best (car (sort targets
                             (lambda (a b)
                               (< (abs (- a old-line)) (abs (- b old-line))))))))
        (goto-char (point-min))
        (forward-line (1- best))))
     (t
      (let ((line (max 1 (min old-line max-line))))
        (goto-char (point-min))
        (forward-line (1- line))
        (unless (get-text-property (line-beginning-position) 'org-marker)
          (cj/--agenda-frame-goto-first-item)))))))

;; -- Snapshot with cloned markers --------------------------------------------

(defun cj/--agenda-frame-snapshot-markers (buffer)
  "Return a list of (POSITION . CLONE) for every org-marker in BUFFER.
CLONE is an independent `copy-marker' into the same source location, so
it survives `org-agenda-reset-markers' nulling BUFFER's own markers on a
rebuild."
  (with-current-buffer buffer
    (let ((clones '())
          (pos (point-min)))
      (while (< pos (point-max))
        (let ((m (get-text-property pos 'org-marker)))
          (when (and (markerp m) (marker-buffer m))
            (push (cons pos (copy-marker m)) clones)))
        (setq pos (or (next-single-property-change pos 'org-marker buffer)
                      (point-max))))
      (nreverse clones))))

(defun cj/--agenda-frame-reinstall-markers (buffer clones)
  "Reapply CLONES (from `cj/--agenda-frame-snapshot-markers') to BUFFER.
Restores each cloned marker as the org-marker text property at its
recorded position, so RET/TAB resolve to the right source line after a
snapshot restore."
  (with-current-buffer buffer
    (dolist (entry clones)
      (let ((pos (car entry)))
        (when (and (>= pos (point-min)) (< pos (point-max)))
          (put-text-property pos (1+ pos) 'org-marker (cdr entry)))))))

(defun cj/--agenda-frame-snapshot (buffer window)
  "Capture BUFFER's last-good state for restore after a failed redo.
Returns a plist of the propertized :text (carrying org-redo-cmd/org-lprops),
:point, :window-start, and :markers (cloned, source-owned)."
  (with-current-buffer buffer
    (list :text (buffer-substring (point-min) (point-max))
          :point (point)
          :window-start (and (window-live-p window) (window-start window))
          :markers (cj/--agenda-frame-snapshot-markers buffer))))

(defun cj/--agenda-frame-restore-snapshot (buffer snapshot window)
  "Restore SNAPSHOT verbatim into BUFFER, reinstating cloned markers.
Sets point and, when WINDOW is live, window-start from the snapshot."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (plist-get snapshot :text))
      (cj/--agenda-frame-reinstall-markers buffer (plist-get snapshot :markers))
      (goto-char (min (plist-get snapshot :point) (point-max))))
    (when (and (window-live-p window) (plist-get snapshot :window-start))
      (set-window-start window (min (plist-get snapshot :window-start)
                                    (point-max))))))

(defun cj/--agenda-frame-release-snapshot (snapshot)
  "Release SNAPSHOT's cloned markers so repeated redoes don't leak markers.
Called on a successful redo (the snapshot is discarded); never on the
error path, where the clones become the buffer's live org-markers."
  (dolist (entry (plist-get snapshot :markers))
    (when (markerp (cdr entry))
      (set-marker (cdr entry) nil))))

;; -- Failure latch and overlay -----------------------------------------------

(defun cj/--agenda-frame-record-failure (frame)
  "Increment FRAME's consecutive-failure count; return non-nil to report.
Reports only on the first failure of a run (the 0 -> 1 transition)."
  (let ((n (1+ (or (frame-parameter frame cj/--agenda-frame-fail-count-parameter)
                   0))))
    (set-frame-parameter frame cj/--agenda-frame-fail-count-parameter n)
    (= n 1)))

(defun cj/--agenda-frame-clear-failure (frame)
  "Reset FRAME's consecutive-failure count (the next tick reports again)."
  (set-frame-parameter frame cj/--agenda-frame-fail-count-parameter 0))

(defun cj/--agenda-frame-failure-overlays (buffer)
  "Return the refresh-failed banner overlays in BUFFER (normally 0 or 1)."
  (with-current-buffer buffer
    (seq-filter (lambda (o) (overlay-get o cj/--agenda-frame-overlay-property))
                (overlays-in (point-min) (point-max)))))

(defun cj/--agenda-frame-show-failure-overlay (buffer)
  "Show the refresh-failed notice as an overlay at the top of BUFFER.
Idempotent: an existing banner is reused, so consecutive failures never
stack a second one."
  (with-current-buffer buffer
    (let ((overlay (or (car (cj/--agenda-frame-failure-overlays buffer))
                       (make-overlay (point-min) (point-min)))))
      (overlay-put overlay cj/--agenda-frame-overlay-property t)
      (overlay-put overlay 'before-string
                   (propertize "Agenda frame: refresh failed (C-M-<f8> to force-rescan)\n"
                               'face 'warning)))))

(defun cj/--agenda-frame-remove-overlay (buffer)
  "Remove the refresh-failed banner from BUFFER, if present."
  (when (buffer-live-p buffer)
    (mapc #'delete-overlay (cj/--agenda-frame-failure-overlays buffer))))

;; -- The refresh itself ------------------------------------------------------

(defun cj/--agenda-frame-do-redo (frame buffer window)
  "Redo the agenda in BUFFER, degrading to the last-good snapshot on failure.
On success: drop the failure overlay, restore point, clear the failure
latch, and release the pre-redo snapshot.  On error: restore the snapshot
verbatim, re-enable the policy (the finalize hook runs only on success),
show the failure overlay, and report once per consecutive-failure run.
Either way the frame is never blank, unrestricted, or non-retryable."
  (with-current-buffer buffer
    ;; Clone the point marker: `org-agenda-redo' calls `org-agenda-reset-markers'
    ;; which nulls the buffer's own org-markers, so the raw marker would be dead
    ;; by the time `cj/--agenda-frame-restore-point' runs -- collapsing the
    ;; "follow the same source item" restoration to the line-number clamp on
    ;; every normal tick.  An independent clone survives the reset.
    (let ((old-marker (let ((m (cj/--agenda-frame-item-marker)))
                        (and (markerp m) (marker-buffer m) (copy-marker m))))
          (old-line (line-number-at-pos))
          (snapshot (cj/--agenda-frame-snapshot buffer window)))
      (unwind-protect
          (condition-case nil
              ;; Never bind sticky here: `org-agenda-redo' handles the
              ;; in-place rebuild itself (binds sticky nil, redirects the
              ;; buffer name).  A sticky t reaching `org-agenda-prepare'
              ;; mid-redo makes it throw \\='exit with no catch, failing
              ;; every tick.  current-window is bound as a belt so a rule
              ;; can't split the frame during the rebuild.
              (let ((inhibit-message t)
                    (org-agenda-window-setup 'current-window))
                (org-agenda-redo)
                (cj/--agenda-frame-remove-overlay buffer)
                (cj/--agenda-frame-restore-point old-marker old-line)
                (cj/--agenda-frame-clear-failure frame)
                (cj/--agenda-frame-release-snapshot snapshot))
            (error
             (cj/--agenda-frame-restore-snapshot buffer snapshot window)
             (cj/agenda-frame-mode 1)
             (cj/--agenda-frame-show-failure-overlay buffer)
             (when (cj/--agenda-frame-record-failure frame)
               (message "Agenda frame: refresh failed (C-M-<f8> to force-rescan)"))))
        (when (markerp old-marker)
          (set-marker old-marker nil))))))

(defun cj/--agenda-frame-safe-redo (&optional frame)
  "Refresh the agenda buffer in FRAME safely (the timer tick and manual `r').
Runs with the dedicated window selected for the redo's dynamic extent and
restores the prior window afterward, never calling an input-focus
function, so a tick while another frame is active neither errors on an
out-of-range window-start nor steals focus."
  (interactive)
  (let* ((frame (or frame (cj/--agenda-frame)))
         (buffer (cj/--agenda-frame-sticky-buffer))
         (window (and (frame-live-p frame) (buffer-live-p buffer)
                      (get-buffer-window buffer frame))))
    (when (and (window-live-p window)
               ;; Skip the tick while a minibuffer is active anywhere --
               ;; reselecting windows under an active minibuffer session can
               ;; break it, and the next tick catches up.
               (not (active-minibuffer-window)))
      (let ((prev-window (selected-window))
            ;; The rebuild takes visible time, and for its duration the
            ;; agenda window is the selected window.  Without inhibiting
            ;; redisplay the user's cursor visibly goes hollow for the whole
            ;; rebuild every tick -- indistinguishable from focus theft.
            ;; The rebuild blocks Emacs either way (it is synchronous), so
            ;; this hides the selection flicker at no extra cost; redisplay
            ;; resumes after the selection is restored.
            (inhibit-redisplay t))
        (unwind-protect
            (progn
              (select-window window t)
              (cj/--agenda-frame-do-redo frame buffer window))
          (when (window-live-p prev-window)
            (select-window prev-window t)))))))

(defun cj/--agenda-frame-start-timer (frame)
  "Start FRAME's five-minute wall-clock refresh timer, unless one exists.
Idempotent: a frame already carrying a live timer keeps it (no duplicate).
Returns the timer."
  (unless (timerp (frame-parameter frame cj/--agenda-frame-timer-parameter))
    (let* ((period cj/--agenda-frame-refresh-seconds)
           (delay (cj/--agenda-frame-seconds-to-next-mark (current-time) period))
           (timer (run-at-time delay period #'cj/--agenda-frame-safe-redo frame)))
      (set-frame-parameter frame cj/--agenda-frame-timer-parameter timer)
      timer)))

;; -- Public command and key install ------------------------------------------

(defun cj/agenda-frame-toggle ()
  "Toggle the dedicated agenda frame.
Spawn it when none exists, raise and focus it when it exists but is
unfocused, and close it when it is the selected frame."
  (interactive)
  (cj/--agenda-frame-toggle))

(defun cj/--agenda-frame-install-keys (&optional map)
  "Bind the F8-family keys for the agenda frame in MAP (default: the global map).
S-<f8> toggles the agenda frame; the force-rescan
\(`cj/org-agenda-refresh-files') moves to C-M-<f8>, keeping the whole
force-refresh idea in the F8 family."
  (let ((map (or map (current-global-map))))
    (define-key map (kbd "S-<f8>") #'cj/agenda-frame-toggle)
    (define-key map (kbd "C-M-<f8>") #'cj/org-agenda-refresh-files)))

;;; Wiring — registered once org-agenda is loaded (no batch side effects)

(with-eval-after-load 'org-agenda
  (cj/--agenda-frame-register-command)
  (add-hook 'org-agenda-finalize-hook #'cj/--agenda-frame-maybe-enable-mode)
  (add-hook 'delete-frame-functions #'cj/--agenda-frame-on-delete-frame))

;; The public gesture appears only now that the feature is complete and live.
(cj/--agenda-frame-install-keys)

(provide 'org-agenda-frame)
;;; org-agenda-frame.el ends here
