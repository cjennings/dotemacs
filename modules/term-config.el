;;; term-config.el --- Settings for ghostel and the F12 toggle -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: registers terminal keymaps and the F12 toggle.
;; Top-level side effects: defines two keymaps (one under cj/custom-keymap), one
;;   global key, two add-hook, package config.
;; Runtime requires: keybindings, seq, subr-x, cj-window-geometry-lib,
;;   cj-window-toggle-lib.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; GHOSTEL
;; ghostel is a native Emacs terminal emulator over libghostty-vt (the Ghostty
;; engine).  Like a real terminal, in its default semi-char mode most keys are
;; sent to the running program; `ghostel-keymap-exceptions' lists the keys that
;; reach Emacs instead.  We add C-; so the personal prefix keymap works inside
;; ghostel buffers.
;;
;; The module degrades gracefully when ghostel is unavailable (D6 of the
;; migration spec): the package installs via use-package, the native module
;; auto-downloads on first use, and ghostel emits its own warning if the module
;; cannot load.  A machine without a prebuilt binary needs Zig to build it; the
;; terminal commands stay defined either way.
;;
;; Two ways to lift text out of a terminal, both with the same key story:
;;   - C-; x c  enters copy-mode via `cj/term-copy-mode-dwim'.  When a tmux
;;     client is attached (typical -- `cj/term-launch-tmux' auto-starts tmux),
;;     sends tmux's prefix C-b [ then C-a, so the user lands in tmux's own
;;     copy-mode with the full pane history and the cursor at column 0 (so
;;     scrolling up runs up the left, not the right).  Without tmux, falls back to
;;     `ghostel-copy-mode' (read-only standard-Emacs navigation over the
;;     scrollback; M-w copies and stays, q / C-g exit) and moves point to the
;;     start of the line for the same column-0 reason.
;;   - C-; x h  captures the current tmux pane's full history into a temporary
;;     Emacs buffer.
;; In both copy surfaces, M-w copies the active region and stays open so several
;; pieces can be grabbed in a row; C-g / q leave without copying.

;;; Code:

(require 'keybindings)
(require 'seq)
(require 'subr-x)
(require 'cj-window-geometry-lib)
(require 'cj-window-toggle-lib)

(declare-function ghostel "ghostel" (&optional directory))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-copy-mode "ghostel" ())
(declare-function ghostel-clear-scrollback "ghostel" ())
(declare-function ghostel-next-prompt "ghostel" (&optional n))
(declare-function ghostel-previous-prompt "ghostel" (&optional n))
(declare-function ghostel-send-next-key "ghostel" ())
(declare-function ghostel--rebuild-semi-char-keymap "ghostel" ())
(defvar ghostel-mode-map)
(defvar ghostel-keymap-exceptions)
(defvar ghostel-buffer-name)

(defvar-keymap cj/term-map
  :doc "Personal terminal command map.")
;; Lowercase x picked over T for fewer Shift presses; t is the toggle leaf.
(cj/register-prefix-map "x" cj/term-map)

;; ----------------------------- tmux history ----------------------------------

(defvar-local cj/term-tmux-history--origin-buffer nil
  "Buffer active before opening the tmux history buffer.")

(defvar-local cj/term-tmux-history--origin-window nil
  "Window active before opening the tmux history buffer.")

(defvar-local cj/term-tmux-history--origin-point nil
  "Point in the origin buffer before opening the tmux history buffer.")

(defun cj/term--tmux-output (&rest args)
  "Run tmux with ARGS and return its stdout.
Signal `user-error' when tmux exits with a non-zero status."
  (with-temp-buffer
    (let ((exit-code (apply #'process-file "tmux" nil t nil args)))
      (unless (zerop exit-code)
        (user-error "tmux failed: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun cj/term--tmux-pane-id-for-tty (tty)
  "Return the tmux pane id for client TTY."
  (let* ((output (cj/term--tmux-output
                  "list-clients" "-F" "#{client_tty}\t#{pane_id}"))
         (lines (split-string output "\n" t))
         (match (seq-find
                 (lambda (line)
                   (let ((fields (split-string line "\t")))
                     (equal (car fields) tty)))
                 lines)))
    (unless match
      (user-error "No tmux client found for terminal tty %s" tty))
    (cadr (split-string match "\t"))))

(defun cj/term--tmux-capture-pane (pane-id)
  "Return full joined tmux history for PANE-ID."
  (cj/term--tmux-output
   "capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" pane-id))

(defun cj/term--current-tmux-pane-id ()
  "Return the tmux pane id for the current ghostel buffer."
  (unless (eq major-mode 'ghostel-mode)
    (user-error "Current buffer is not a ghostel buffer"))
  (let* ((proc (get-buffer-process (current-buffer)))
         (tty (and proc (process-tty-name proc))))
    (unless (and tty (not (string-empty-p tty)))
      (user-error "Could not determine terminal tty"))
    (cj/term--tmux-pane-id-for-tty tty)))

(defvar-keymap cj/term-tmux-history-mode-map
  :doc "Keymap for `cj/term-tmux-history-mode'.
M-w copies the active region without leaving the buffer; C-g, <escape>, or q
returns to the terminal without copying.  RET is left unbound."
  "M-w" #'kill-ring-save
  "C-g" #'cj/term-tmux-history-quit
  "<escape>" #'cj/term-tmux-history-quit
  "q" #'cj/term-tmux-history-quit)

(define-derived-mode cj/term-tmux-history-mode special-mode "Tmux History"
  "Mode for copying captured tmux pane history with normal Emacs keys."
  (setq-local truncate-lines t)
  (goto-address-mode 1))

(defun cj/term-tmux-history-quit ()
  "Quit tmux history and return to its origin buffer."
  (interactive)
  (let ((history-buffer (current-buffer))
        (origin-buffer cj/term-tmux-history--origin-buffer)
        (origin-window cj/term-tmux-history--origin-window)
        (origin-point cj/term-tmux-history--origin-point))
    (when (buffer-live-p origin-buffer)
      (if (window-live-p origin-window)
          (progn
            (set-window-buffer origin-window origin-buffer)
            (select-window origin-window))
        (pop-to-buffer origin-buffer))
      (with-current-buffer origin-buffer
        (when (integer-or-marker-p origin-point)
          (goto-char origin-point))))
    (when (buffer-live-p history-buffer)
      (kill-buffer history-buffer))))

(defun cj/term-tmux-history ()
  "Open full tmux pane history in a temporary Emacs buffer.

The history buffer uses normal Emacs navigation and selection.  `M-w'
copies the active region and stays open, so several pieces can be
copied in a row; `q', `<escape>', or `C-g' returns point to the
terminal buffer that launched it.

The history view replaces the origin terminal buffer in the same window
\(via `switch-to-buffer'), not a split or a popped-up window."
  (interactive)
  (let* ((origin-buffer (current-buffer))
         (origin-window (selected-window))
         (origin-point (point))
         (pane-id (cj/term--current-tmux-pane-id))
         (history (cj/term--tmux-capture-pane pane-id))
         (buffer (get-buffer-create
                  (format "*terminal tmux history: %s*" (buffer-name origin-buffer)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert history))
      (cj/term-tmux-history-mode)
      (setq-local cj/term-tmux-history--origin-buffer origin-buffer)
      (setq-local cj/term-tmux-history--origin-window origin-window)
      (setq-local cj/term-tmux-history--origin-point origin-point)
      (goto-char (point-max)))
    (switch-to-buffer buffer)))

;; ----------------------------- copy mode -------------------------------------

(defun cj/term--in-tmux-p ()
  "Return non-nil when the current ghostel buffer has a tmux client attached.
Errors from the pane-id lookup (not in ghostel-mode, no tty, no matching
client, tmux not installed) are treated as nil so callers can use this as a
cheap boolean predicate."
  (and (eq major-mode 'ghostel-mode)
       (condition-case _
           (and (cj/term--current-tmux-pane-id) t)
         (error nil))))

(defun cj/term-copy-mode-dwim ()
  "Enter copy-mode using the engine appropriate to this terminal.

When tmux is attached, write tmux's default prefix sequence (C-b [) into the
pty so the user lands in tmux's copy-mode with the full pane history, then
C-a to land the cursor at the start of the line.  Without the trailing C-a
the copy cursor inherits the live column (far right after a prompt) and
scrolling up runs up the right edge; tmux's emacs copy-mode binds C-a to
start-of-line, so column 0 makes it run up the left.  Without tmux, falls
through to `ghostel-copy-mode' (a read-only standard-Emacs view of the
scrollback; M-w copies and stays, q / C-g exit), then moves point to the
start of the line for the same column-0 reason."
  (interactive)
  (if (cj/term--in-tmux-p)
      (ghostel-send-string "\C-b[\C-a")
    (ghostel-copy-mode)
    (beginning-of-line)))

;; ----------------------------- ghostel package -------------------------------

(defun cj/turn-off-chrome-for-term ()
  "Turn off line numbers and hl-line in a terminal buffer."
  (hl-line-mode -1)
  (display-line-numbers-mode -1))

(defun cj/term-launch-tmux ()
  "Auto-launch tmux in a ghostel buffer unless already inside tmux.

Skipped when `cj/--ai-term-suppress-tmux' is non-nil so the AI-agent flow can
run its own project-named tmux session instead of a bare, auto-named one.
`bound-and-true-p' keeps this safe whether or not ai-term.el is loaded."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and proc
               (not (getenv "TMUX"))
               (not (bound-and-true-p cj/--ai-term-suppress-tmux)))
      (ghostel-send-string "tmux\n"))))

(use-package ghostel
  :ensure t
  :commands (ghostel)
  :init
  ;; These keys must reach Emacs (not the terminal program) inside ghostel
  ;; buffers.  In semi-char mode ghostel forwards every key NOT in
  ;; `ghostel-keymap-exceptions' to the pty, and `ghostel-semi-char-mode-map'
  ;; is rebuilt from that list by `ghostel--rebuild-semi-char-keymap' --
  ;; `add-to-list' alone updates the list but not the already-built map, so the
  ;; rebuild is what actually lets the key through to `ghostel-mode-map' / the
  ;; global map.  C-; and F12 are the prefix + toggle; the modified arrows are
  ;; windmove (S-arrows, focus) and buffer-move (C-M-arrows, swap), which the
  ;; ai-term workflow expects to work from inside an agent buffer.  F8, F10 and
  ;; C-F10 are global bindings (org agenda, music-playlist toggle, server
  ;; shutdown) that reach Emacs by falling through to the global map once the
  ;; semi-char map stops forwarding them.
  (with-eval-after-load 'ghostel
    (dolist (key '("C-;" "<f8>" "<f12>" "<f10>" "C-<f10>"
                   "S-<up>" "S-<down>" "S-<left>" "S-<right>"
                   "C-M-<up>" "C-M-<down>" "C-M-<left>" "C-M-<right>"))
      (add-to-list 'ghostel-keymap-exceptions key))
    (ghostel--rebuild-semi-char-keymap))
  :hook
  ((ghostel-mode . cj/turn-off-chrome-for-term)
   (ghostel-mode . cj/term-launch-tmux))
  :custom
  (ghostel-kill-buffer-on-exit t)
  ;; Byte analog of the prior 100000-line vterm setting (~100 bytes/line) -- D7.
  (ghostel-max-scrollback (* 10 1024 1024)))

;; ----------------------- F12 toggle (custom) -----------------------
;;
;; Mirrors the geometry-preservation pattern shared with ai-term.el: capture
;; direction + body size at toggle-off, replay them via a custom display action
;; using frame-edge directions and body-relative sizes so the result is
;; divider-independent and layout-stable.  Excludes agent-prefixed buffers,
;; which ai-term.el owns via F9.

(defcustom cj/term-toggle-window-height 0.7
  "Default fraction of frame height for the F12 terminal window."
  :type 'number
  :group 'term)

(defvar cj/--term-toggle-last-direction nil
  "Last user-chosen direction for the F12 terminal display.
Symbol: right, left, or below.  `above' is never stored.  nil means use the
default `below' for F12's traditional bottom split.")

(defvar cj/--term-toggle-last-size nil
  "Last user-chosen body size for the F12 terminal display.
Positive integer: body-cols (right/left) or body-lines (below/above).
nil means fall back to `cj/term-toggle-window-height' as a fraction.")

(defun cj/--term-toggle-buffer-p (buffer)
  "Return non-nil when BUFFER is a terminal buffer F12 should manage.

Qualifies when BUFFER is alive and has `ghostel-mode' (or its name starts with
the ghostel buffer-name prefix), AND its name does NOT start with the agent
prefix used by ai-term.el."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (or (eq major-mode 'ghostel-mode)
                  (string-prefix-p (or (bound-and-true-p ghostel-buffer-name)
                                       "*ghostel*")
                                   (buffer-name buffer)))
              (not (string-prefix-p "agent [" (buffer-name buffer)))))))

(defun cj/--term-toggle-buffers ()
  "Return live F12-managed terminal buffers in `buffer-list' (MRU) order."
  (seq-filter #'cj/--term-toggle-buffer-p (buffer-list)))

(defun cj/--term-toggle-displayed-window (&optional frame)
  "Return a window in FRAME currently displaying an F12 terminal buffer, or nil.
FRAME defaults to the selected frame.  Minibuffer is excluded."
  (seq-find (lambda (w)
              (cj/--term-toggle-buffer-p (window-buffer w)))
            (window-list (or frame (selected-frame)) 'never)))

(defun cj/--term-toggle-capture-state (window)
  "Capture WINDOW's direction + body size into module-level state.
Default direction is `below' to match F12's traditional bottom split."
  (cj/window-toggle-capture-state
   window 'below
   'cj/--term-toggle-last-direction
   'cj/--term-toggle-last-size
   '(right below left)))

(defun cj/--term-toggle-display-saved (buffer alist)
  "Display-buffer action: split per saved direction and body size.
Delegates to `cj/window-toggle-display-saved' against the F12 state vars,
falling back to `below' and `cj/term-toggle-window-height'."
  (cj/window-toggle-display-saved
   buffer alist
   'cj/--term-toggle-last-direction 'below
   'cj/--term-toggle-last-size cj/term-toggle-window-height))

(defun cj/--term-toggle-display-rule-list ()
  "Return the `display-buffer-alist' entry list installed by F12.
Routes any terminal buffer satisfying `cj/--term-toggle-buffer-p' through
reuse-window then the saved-geometry action.  Excludes agent buffers."
  '(((lambda (buffer-or-name _)
       (cj/--term-toggle-buffer-p (get-buffer buffer-or-name)))
     (display-buffer-reuse-window
      cj/--term-toggle-display-saved)
     (inhibit-same-window . t))))

(dolist (entry (cj/--term-toggle-display-rule-list))
  (add-to-list 'display-buffer-alist entry))

(defun cj/--term-toggle-dispatch ()
  "Compute the F12 (`cj/term-toggle') action without performing it.

Returns one of:
- (toggle-off . WINDOW)        -- terminal displayed in WINDOW; hide it.
- (show-recent . BUFFER)       -- terminal alive but not shown; redisplay.
- (create-new)                 -- no terminal buffer alive; create one."
  (let ((win (cj/--term-toggle-displayed-window)))
    (cond
     (win (cons 'toggle-off win))
     (t
      (let ((buffers (cj/--term-toggle-buffers)))
        (cond
         (buffers (cons 'show-recent (car buffers)))
         (t '(create-new))))))))

(defun cj/term-toggle ()
  "Toggle a normal (non-agent) ghostel terminal buffer.

- If an F12-managed terminal is displayed in this frame, capture its geometry
  and delete its window (toggle off).  Falls back to burying when it is the
  only window in the frame.
- Otherwise, if any F12-managed terminal buffer is alive, display the most
  recent one via the saved-geometry action.
- Otherwise, create a new terminal via `(ghostel)' which routes through the
  same display action.

Excludes agent-prefixed buffers; those have their own F9 dispatch via
`cj/ai-term'."
  (interactive)
  (pcase (cj/--term-toggle-dispatch)
    (`(toggle-off . ,win)
     (cj/--term-toggle-capture-state win)
     (if (one-window-p)
         (bury-buffer (window-buffer win))
       (delete-window win))
     nil)
    (`(show-recent . ,buf)
     (display-buffer buf)
     (let ((w (get-buffer-window buf)))
       (when w (select-window w)))
     buf)
    (`(create-new)
     (ghostel))))

(keymap-global-set "<f12>" #'cj/term-toggle)

;; ----------------------------- prefix menu -----------------------------------

(keymap-set cj/term-map "c" #'cj/term-copy-mode-dwim)
(keymap-set cj/term-map "h" #'cj/term-tmux-history)
(keymap-set cj/term-map "l" #'ghostel-clear-scrollback)
(keymap-set cj/term-map "N" #'ghostel)
(keymap-set cj/term-map "n" #'ghostel-next-prompt)
(keymap-set cj/term-map "p" #'ghostel-previous-prompt)
(keymap-set cj/term-map "q" #'ghostel-send-next-key)
(keymap-set cj/term-map "t" #'cj/term-toggle)

(defun cj/term-send-C-SPC ()
  "Forward C-SPC (NUL) to the terminal instead of setting an Emacs mark.

ghostel forwards the `C-@' event but not the distinct `C-SPC' event GUI
Emacs produces, so a bare C-SPC in a ghostel buffer falls through to the
global `set-mark-command'.  That sets an Emacs region in the terminal buffer
that follows point as output streams (a stuck \"selection\" C-g / Escape
can't clear) and, worse, never reaches tmux -- so tmux copy-mode's
begin-selection (C-Space) never starts and M-w then copies nothing.
Forwarding NUL makes C-Space behave like a terminal key."
  (interactive)
  (ghostel-send-string "\C-@"))

(defun cj/term-install-keys ()
  "Make `C-;' resolve as the personal keymap inside ghostel buffers, bind the
F12 toggle, and forward C-SPC so it reaches the terminal (see
`cj/term-send-C-SPC')."
  (when (boundp 'ghostel-mode-map)
    (keymap-set ghostel-mode-map "C-;" cj/custom-keymap)
    (keymap-set ghostel-mode-map "<f12>" #'cj/term-toggle)
    (keymap-set ghostel-mode-map "C-SPC" #'cj/term-send-C-SPC)))

(cj/term-install-keys)
(with-eval-after-load 'ghostel
  (cj/term-install-keys))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; x" "terminal menu"
    "C-; x c" "copy mode (tmux/ghostel)"
    "C-; x h" "tmux scrollback history"
    "C-; x l" "clear scrollback"
    "C-; x N" "new terminal"
    "C-; x n" "next prompt"
    "C-; x p" "previous prompt"
    "C-; x q" "send next key to terminal"
    "C-; x t" "toggle terminal"))

(provide 'term-config)
;;; term-config.el ends here.
