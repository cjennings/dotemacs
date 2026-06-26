;;; eat-config.el --- EAT terminal and the F12 toggle -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;;
;; EAT (Emulate A Terminal, pure elisp) is the F12 terminal.  Because EAT renders
;; entirely in elisp, its whole palette is real Emacs faces, so it themes from
;; the theme.  This module owns the eat package configuration, the keymap wiring
;; that lets F12 and C-; reach Emacs from inside a terminal, and the F12
;; dock-and-remember toggle.
;;
;; The toggle reuses the geometry-preservation pattern from cj-window-toggle-lib:
;; capture direction + body size at toggle-off, replay them via a custom display
;; action using frame-edge directions and body-relative sizes, so the docked
;; terminal returns at the same size and the result is divider-independent.

;;; Code:

(require 'cj-window-geometry-lib)
(require 'cj-window-toggle-lib)

(declare-function eat "eat" (&optional program arg))
(defvar eat-buffer-name)
(defvar eat-mode-map)
(defvar eat-semi-char-mode-map)
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
;; ai-term.el's ghostel agent buffers are separate (M-SPC).

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
  "Return non-nil when BUFFER is the EAT terminal F12 should manage.

Qualifies when BUFFER is alive and has `eat-mode' (or its name starts with the
EAT buffer-name prefix).  ai-term's ghostel agent buffers never match -- they
are managed separately via M-SPC, not F12."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (or (eq major-mode 'eat-mode)
             (string-prefix-p (or (bound-and-true-p eat-buffer-name)
                                  "*eat*")
                              (buffer-name buffer))))))

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
  "Toggle the EAT terminal buffer.

- If the EAT terminal is displayed in this frame, capture its geometry and
  delete its window (toggle off).  Falls back to burying when it is the only
  window in the frame.
- Otherwise, if the EAT terminal buffer is alive, display it via the
  saved-geometry action.
- Otherwise, create a new EAT terminal, displaying it through the same
  saved-geometry action.

ai-term's ghostel agent buffers are managed separately via M-SPC, not F12."
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
     ;; Create the EAT buffer without stealing the layout, then display it
     ;; through the saved-geometry dock rule (same path as show-recent).
     (save-window-excursion (eat))
     (let ((buf (get-buffer (or (bound-and-true-p eat-buffer-name) "*eat*"))))
       (when buf
         (display-buffer buf)
         (let ((w (get-buffer-window buf)))
           (when w (select-window w))))
       buf))))

(keymap-global-set "<f12>" #'cj/term-toggle)

(provide 'eat-config)
;;; eat-config.el ends here
