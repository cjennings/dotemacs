;;; eat-config.el --- EAT terminal emulator and the F12 eshell toggle -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;;
;; EAT (Emulate A Terminal, pure elisp) is the terminal emulator.  Because EAT
;; renders entirely in elisp, its whole palette is real Emacs faces, so it themes
;; from the theme.  This module owns the eat package configuration, the keymap
;; wiring that lets F12 and C-; reach Emacs from inside a terminal, and the F12
;; dock-and-remember toggle.
;;
;; F12 opens eshell, which runs through EAT (eat-eshell-mode, set up in
;; eshell-config.el): the shell is eshell -- elisp functions as commands, TRAMP
;; transparency -- and EAT renders its visual commands.  eshell-config.el holds
;; the shell itself; this module holds the emulator and the toggle.
;;
;; The toggle reuses the geometry-preservation pattern from cj-window-toggle-lib:
;; capture direction + body size at toggle-off, replay them via a custom display
;; action using frame-edge directions and body-relative sizes, so the docked
;; terminal returns at the same size and the result is divider-independent.

;;; Code:

(require 'keybindings)
(require 'cj-window-geometry-lib)
(require 'cj-window-toggle-lib)

(declare-function eat "eat" (&optional program arg))
(declare-function eshell "eshell" (&optional arg))
(defvar eat-mode-map)
(defvar eat-semi-char-mode-map)
(defvar eshell-buffer-name)
(defvar cj/custom-keymap)

(defun cj/turn-off-chrome-for-term ()
  "Turn off line numbers and hl-line in a terminal buffer."
  (hl-line-mode -1)
  (display-line-numbers-mode -1))

;; ------------------------------- eat package ---------------------------------

(use-package eat
  :ensure t
  :commands (eat)
  :hook (eat-mode . cj/turn-off-chrome-for-term)
  :custom
  ;; Close the EAT buffer when its shell exits.
  (eat-kill-buffer-on-exit t)
  ;; Shell-integration UX.  These are EAT defaults, set explicitly to document
  ;; intent and survive default changes.  They only light up once the shell
  ;; sources EAT's integration script -- see the EAT block in the zsh rc.
  (eat-enable-directory-tracking t)        ; Emacs follows the terminal's cwd
  (eat-enable-shell-prompt-annotation t)   ; the success/running/failure prompt glyphs
  (eat-enable-shell-command-history t)     ; terminal history into EAT line-mode isearch
  ;; Interaction.
  (eat-enable-mouse t)                      ; mouse clicks + selection in TUIs (default)
  (eat-enable-kill-from-terminal t)         ; terminal selection -> Emacs kill-ring (default)
  (eat-enable-yank-to-terminal t)           ; Emacs kill-ring -> the terminal (off by default)
  ;; Fidelity.
  (eat-enable-alternative-display t)        ; alt-screen so TUIs restore scrollback on exit (default)
  (eat-term-scrollback-size (* 10 1024 1024)) ; ~10MB of scrollback, matching the old ghostel
  ;; Truecolor is already on: eat-term-name auto-selects the compiled eat-truecolor terminfo.
  ;; Niceties.
  (eat-sixel-render-formats '(xpm svg half-block background none)) ; inline images (on by default)
  (eat-query-before-killing-running-terminal 'auto) ; confirm before killing a terminal with a live process
  :config
  ;; F12 and C-; must reach Emacs from inside EAT.  In semi-char mode (EAT's
  ;; default) EAT forwards unbound keys to the terminal -- a letter runs
  ;; `eat-self-input' -- so bind these explicitly or they never reach Emacs:
  ;; F12 toggles the terminal window, C-; opens the global prefix map.
  (keymap-set eat-semi-char-mode-map "<f12>" #'cj/term-toggle)
  (keymap-set eat-semi-char-mode-map "C-;" cj/custom-keymap)
  (keymap-set eat-mode-map "<f12>" #'cj/term-toggle)
  (keymap-set eat-mode-map "C-;" cj/custom-keymap))

;; ----------------------- F12 toggle (custom) -----------------------
;;
;; Mirrors the geometry-preservation pattern shared with ai-term.el: capture
;; direction + body size at toggle-off, replay them via a custom display action
;; using frame-edge directions and body-relative sizes so the result is
;; divider-independent and layout-stable.  Manages the EAT terminal only;
;; ai-term.el's agent buffers are separate (M-SPC).

(defcustom cj/term-toggle-window-height 0.7
  "Default fraction of frame height for the F12 terminal window.
Used as the size fallback when F12 docks the terminal as a bottom split."
  :type 'number
  :group 'term)

(defcustom cj/term-toggle-window-width 0.5
  "Default fraction of frame width for the F12 terminal window.
Used as the size fallback when F12 docks the terminal as a right-side
column (see `cj/--term-toggle-default-direction')."
  :type 'number
  :group 'term)

(defun cj/--term-toggle-default-direction ()
  "Return the default dock direction for the F12 terminal: `right' or `below'.
Docks as a right-side column only when a side-by-side split would leave
both panes at least `cj/window-dock-min-columns' wide (the terminal's
share is `cj/term-toggle-window-width'); otherwise stacks below.  See
`cj/preferred-dock-direction'."
  (cj/preferred-dock-direction (frame-width) cj/term-toggle-window-width))

(defun cj/--term-toggle-default-size (direction)
  "Return the default size fraction paired with DIRECTION for the F12 terminal.
`cj/term-toggle-window-width' for `right', `cj/term-toggle-window-height'
otherwise."
  (if (eq direction 'right)
      cj/term-toggle-window-width
    cj/term-toggle-window-height))

(defvar cj/--term-toggle-last-direction nil
  "Last user-chosen direction for the F12 terminal display.
Symbol: right, left, or below.  `above' is never stored.  nil means use the
default `below' for F12's traditional bottom split.")

(defvar cj/--term-toggle-last-size nil
  "Last user-chosen size for the F12 terminal display.
Positive integer: body-cols (right/left) or total-lines (below/above) -- see
`cj/window-replay-size' for why the vertical axis uses total, not body.
nil means fall back to `cj/term-toggle-window-height' as a fraction.")

(defun cj/--term-toggle-buffer-p (buffer)
  "Return non-nil when BUFFER is an eshell terminal F12 should manage.

F12 opens eshell, which runs through EAT via eat-eshell-mode.  ai-term's
agent buffers are managed separately via M-SPC, not F12."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'eshell-mode))))

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
The default direction (used when WINDOW fills its frame) is the
column-rule choice from `cj/--term-toggle-default-direction'."
  (cj/window-toggle-capture-state
   window (cj/--term-toggle-default-direction)
   'cj/--term-toggle-last-direction
   'cj/--term-toggle-last-size
   '(right below left)))

(defun cj/--term-toggle-display-saved (buffer alist)
  "Display-buffer action: split per saved direction and body size.
Delegates to `cj/window-toggle-display-saved' against the F12 state vars,
falling back to the column-rule default direction
\(`cj/--term-toggle-default-direction') and its paired size."
  (let ((dir (cj/--term-toggle-default-direction)))
    (cj/window-toggle-display-saved
     buffer alist
     'cj/--term-toggle-last-direction dir
     'cj/--term-toggle-last-size (cj/--term-toggle-default-size dir))))

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
  "Toggle the F12 eshell terminal (the primary `*eshell*', run through EAT).

- If it is displayed in this frame, capture its geometry and delete its window
  (toggle off).  Falls back to burying when it is the only window in the frame.
- Otherwise, if it is alive, display it via the saved-geometry action.
- Otherwise, open eshell, displaying it through the same saved-geometry action.

eshell runs through EAT via eat-eshell-mode, so visual commands render in a real
terminal.  ai-term's agent buffers are managed separately via M-SPC."
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
     ;; Open the primary eshell without stealing the layout, then display it
     ;; through the saved-geometry dock rule (same path as show-recent).
     (save-window-excursion (eshell))
     (let ((buf (get-buffer (or (bound-and-true-p eshell-buffer-name) "*eshell*"))))
       (when buf
         (display-buffer buf)
         (let ((w (get-buffer-window buf)))
           (when w (select-window w))))
       buf))))

(keymap-global-set "<f12>" #'cj/term-toggle)

;; ------------------- terminal copy mode + tmux history -----------------------
;; Carried over from the ghostel era for the EAT agent terminals (ai-term).
;; Agents run EAT over tmux, so copy-mode is tmux's own copy-mode -- the same UX
;; ghostel-over-tmux had.  C-<up> enters it and scrolls up in one stroke; C-; x c
;; enters it via the menu, and C-; x h grabs the whole pane history into a buffer.

(declare-function cj/register-prefix-map "keybindings")
(declare-function eat-emacs-mode "eat")
(defvar eat--semi-char-mode)
(defvar eat--char-mode)
(defvar eat--line-mode)

(defun cj/--term-send-string (string)
  "Send STRING to the current terminal buffer's process (the pty)."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (process-send-string proc string))))

(defun cj/term-send-escape ()
  "Send ESC to the terminal.
In tmux copy-mode this cancels it (tmux binds Escape to cancel); in a TUI like
vim it forwards ESC normally.  EAT's semi-char mode leaves the bare escape key
unbound and treats `ESC' only as the Meta prefix, so without this the key never
reaches the pty -- which is why C-<up>'s tmux copy-mode could not be exited with
Escape."
  (interactive)
  (cj/--term-send-string "\e"))

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
  "Return the tmux pane id for the current EAT terminal buffer."
  (unless (derived-mode-p 'eat-mode)
    (user-error "Current buffer is not an EAT terminal"))
  (let* ((proc (get-buffer-process (current-buffer)))
         (tty (and proc (process-tty-name proc))))
    (unless (and tty (not (string-empty-p tty)))
      (user-error "Could not determine terminal tty"))
    (cj/term--tmux-pane-id-for-tty tty)))

(defvar-local cj/term-tmux-history--origin-buffer nil
  "Buffer active before opening the tmux history buffer.")
(defvar-local cj/term-tmux-history--origin-window nil
  "Window active before opening the tmux history buffer.")
(defvar-local cj/term-tmux-history--origin-point nil
  "Point in the origin buffer before opening the tmux history buffer.")

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

(defun cj/term-tmux-history ()
  "Open full tmux pane history in a temporary Emacs buffer.

The history buffer uses normal Emacs navigation and selection.  `M-w' copies
the active region and stays open, so several pieces can be copied in a row;
`q', `<escape>', or `C-g' returns point to the terminal buffer that launched
it.  The history view replaces the origin terminal buffer in the same window."
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

(defun cj/term--in-tmux-p ()
  "Return non-nil when the current EAT buffer has a tmux client attached.
Lookup errors (not eat-mode, no tty, no client, tmux absent) are treated as
nil so callers can use this as a cheap boolean predicate."
  (and (derived-mode-p 'eat-mode)
       (condition-case _
           (and (cj/term--current-tmux-pane-id) t)
         (error nil))))

(defun cj/--term-in-emacs-mode-p ()
  "Return non-nil when the current EAT buffer is in emacs (navigation) mode.
EAT has no dedicated emacs-mode flag; emacs mode is the absence of the
semi-char, char, and line input modes."
  (and (derived-mode-p 'eat-mode)
       (not (or (bound-and-true-p eat--semi-char-mode)
                (bound-and-true-p eat--char-mode)
                (bound-and-true-p eat--line-mode)))))

(defun cj/term-copy-mode-dwim ()
  "Enter copy-mode using the engine appropriate to this terminal.

When tmux is attached (an agent terminal), write tmux's prefix sequence (C-b [)
into the pty so the user lands in tmux's copy-mode with the full pane history,
then C-a to land the cursor at column 0 so scrolling up runs up the left edge.
Without tmux, falls through to EAT's emacs mode (a navigable view of the
scrollback) and moves point to the start of the line."
  (interactive)
  (if (cj/term--in-tmux-p)
      (cj/--term-send-string "\C-b[\C-a")
    (eat-emacs-mode)
    (beginning-of-line)))

(defun cj/term--tmux-pane-in-copy-mode-p (pane-id)
  "Return non-nil when tmux PANE-ID is currently displaying a mode.
tmux's `pane_in_mode' is 1 while a pane is in any mode; copy-mode is the only
mode this config enters.  tmux failures are treated as nil."
  (condition-case nil
      (equal "1" (string-trim
                  (cj/term--tmux-output
                   "display-message" "-p" "-t" pane-id "#{pane_in_mode}")))
    (error nil)))

(defun cj/term-copy-mode-up ()
  "Enter copy-mode if needed, then scroll up one line.
A single C-<up> lands in the terminal's copy-mode already moving up.  Pressed
again while already in copy-mode it just moves up another line, so it never
re-enters and resets the cursor.  In tmux, writes the up-arrow escape into the
pty; without tmux, moves point up in EAT's emacs-mode buffer."
  (interactive)
  (let ((pane (ignore-errors (cj/term--current-tmux-pane-id))))
    (cond
     (pane
      (unless (cj/term--tmux-pane-in-copy-mode-p pane)
        (cj/term-copy-mode-dwim))
      (cj/--term-send-string "\e[A"))
     (t
      (unless (cj/--term-in-emacs-mode-p)
        (cj/term-copy-mode-dwim))
      (forward-line -1)))))

;; The C-; x terminal prefix (copy-mode, tmux history, the F12 toggle).  C-<up>
;; enters copy-mode + scrolls in one stroke; bound in EAT's semi-char map so it
;; reaches Emacs from inside an agent terminal.
(defvar-keymap cj/term-map
  :doc "Personal terminal command map.")
(cj/register-prefix-map "x" cj/term-map)
(keymap-set cj/term-map "c" #'cj/term-copy-mode-dwim)
(keymap-set cj/term-map "h" #'cj/term-tmux-history)
(keymap-set cj/term-map "t" #'cj/term-toggle)

(defvar eat-mode-map)
(declare-function eat-semi-char-mode "eat")
(with-eval-after-load 'eat
  (keymap-set eat-semi-char-mode-map "C-<up>" #'cj/term-copy-mode-up)
  ;; Escape forwards ESC to the pty, so it cancels tmux copy-mode (tmux binds
  ;; Escape to cancel) and works in TUIs; in EAT's own emacs/char mode it returns
  ;; to semi-char.  One key gets out of either copy view.
  (keymap-set eat-semi-char-mode-map "<escape>" #'cj/term-send-escape)
  (keymap-set eat-mode-map "<escape>" #'eat-semi-char-mode))

(provide 'eat-config)
;;; eat-config.el ends here
