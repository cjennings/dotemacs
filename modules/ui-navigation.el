;;; ui-navigation --- Managing Cursor Placement, Buffers, and Windows -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P.
;; Load shape: eager.
;; Eager reason: window-navigation keybindings and winner/buffer-move setup for
;;   the first session.
;; Top-level side effects: defines a navigation keymap, binds five global keys,
;;   configures packages via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;; Window Navigation

;; This section handles situations where we're navigating or arranging windows

;; Shift + arrow keys           =  move the cursors around the windows/buffers
;; Control + Shift + arrow keys = resize the windows
;; Meta + Shift + arrow keys    =  move the windows around

;; M-H - split windows, creating a new window horizontally to the right
;; M-V - split windows, creating a new window vertically to the bottom
;; M-T - toggle the orientation of the split between horizontal and vertical
;; M-S - swap window positions
;; M-C - kill the current window
;; M-O - kill the other window
;; M-Z - undo kill buffer
;; M-U - winner undo (revert to the previous layout)

;; Adjusting Window Sizes
;; Note: C-s is pressing Control + Super keys
;; C-s-<left>  move window left
;; C-s-<right> move window right
;; C-s-<up>    move window up
;; C-s-<down>  move window down

;;; Code:

(defvar recentf-list)
(defvar recentf-mode)
(declare-function recentf-mode "recentf")
(declare-function consult-buffer "consult")

;; ------------------------------ Window Placement -----------------------------

(use-package windmove
  :config
  (windmove-default-keybindings))  ; move cursor around with shift+arrows

;; ------------------------------ Window Resizing ------------------------------

;; windsize moves the divider between the active window and a neighbor in the
;; arrow's direction (preferring the right/bottom border).  Its commands have
;; no repeat mechanism; `cj/window-resize-sticky' below adds one.  windsize was
;; on C-s-<arrow> (Ctrl+Super), which a tiling WM eats, so the keys live under
;; C-; b instead (bound there in custom-buffer-file.el).
(use-package windsize
  :commands (windsize-left windsize-right windsize-up windsize-down)
  :custom
  (windsize-cols 2)               ; default 8 is too jumpy for a held nudge loop
  (windsize-rows 2))              ; default 4, same reason

;; M-shift = to balance multiple split windows
(keymap-global-set "M-+" #'balance-windows)

(defvar-keymap cj/window-resize-map
  :doc "Bare arrows that keep resizing the split after a `C-; b <arrow>'
resize -- each moves the active window's divider in the arrow's direction
(via `windsize').  Any other key (or `C-g' / `<escape>') ends the loop."
  "<left>"  #'windsize-left
  "<right>" #'windsize-right
  "<up>"    #'windsize-up
  "<down>"  #'windsize-down)

(defun cj/window-pull-side (key)
  "Map a `C-; b' arrow KEY to the side the revealed window opens on.
The arrow names the edge the current window shrinks toward, so the new
window opens on the *opposite* side and the current window keeps the
arrow's edge: <down> -> above, <up> -> below, <left> -> right,
<right> -> left.  Returns nil for anything else."
  (pcase key
    ("<down>"  'above)
    ("<up>"    'below)
    ("<left>"  'right)
    ("<right>" 'left)
    (_ nil)))

(defun cj/window--pull-away (side)
  "Split the sole window so the previous buffer opens on SIDE.
SIDE is one of above/below/left/right -- opposite the pressed arrow, so
the current window keeps the arrow's edge.  The new window is minimized
to a sliver (the current window keeps almost the whole frame) and shows
`other-buffer'; focus stays on the current window so the sticky arrows
then shrink it step by step via `windsize', exactly as resizing an
existing split does.  No-op when SIDE is nil."
  (when side
    (let ((new (split-window (selected-window) nil side)))
      (set-window-buffer new (other-buffer (current-buffer) t))
      ;; Shrink the reveal to the smallest window Emacs allows (~2 lines, the
      ;; mode line) so the current window keeps almost the whole frame; the
      ;; sticky `windsize' arrows grow the reveal from there.  `minimize-window'
      ;; floors at `window-min-height' (4 by default), so bind it down to 1.
      (let ((window-min-height 1))
        (minimize-window new))
      new)))

(defun cj/window-resize-sticky ()
  "Resize the active window's divider in the just-pressed arrow's direction
\(via `windsize'), then keep `cj/window-resize-map' active so bare arrows keep
nudging until any other key.  Bound to `C-; b <left>/<right>/<up>/<down>'.

When the selected window is the sole window in the frame there is no
divider to move, so the first arrow instead splits a sliver away on the
side opposite the arrow (`cj/window--pull-away'), revealing the previous
buffer; the current window keeps almost the whole frame and the following
arrows shrink it via `windsize', so it reads the same as resizing an
existing split."
  (interactive)
  (let ((key (key-description (vector last-command-event))))
    (if (one-window-p)
        (cj/window--pull-away (cj/window-pull-side key))
      (let ((cmd (keymap-lookup cj/window-resize-map key)))
        (when cmd (call-interactively cmd)))))
  (set-transient-map cj/window-resize-map t))

;; ------------------------------ Window Splitting -----------------------------

(defun cj/split-and-follow-right ()
  "Split window horizontally and select a buffer to display."
  (interactive)
  (split-window-right)
  (other-window 1)
  (consult-buffer))
(keymap-global-set "M-S-v" #'cj/split-and-follow-right)  ;; was M-V, overrides scroll-down

(defun cj/split-and-follow-below ()
  "Split window vertically and select a buffer to display."
  (interactive)
  (split-window-below)
  (other-window 1)
  (consult-buffer))
(keymap-global-set "M-S-h" #'cj/split-and-follow-below)  ;; was M-H

(defun cj/--dashboard-buffer ()
  "Return the *dashboard* buffer, creating it if needed, without changing windows."
  (or (get-buffer "*dashboard*")
      (save-window-excursion
        (when (fboundp 'dashboard-open) (dashboard-open))
        (get-buffer "*dashboard*"))))

(defun cj/--split-show-buffer (split-fn buffer)
  "Split with SPLIT-FN, show BUFFER in the new window, keep point in the current
window.  Return the new window."
  (let ((new (funcall split-fn)))
    (when (and (window-live-p new) buffer)
      (set-window-buffer new buffer))
    new))

(defun cj/--split-from-dashboard-p (buffer-name)
  "Return non-nil when BUFFER-NAME is the dashboard.
Splitting from the dashboard shows *scratch* in the new window instead of
the dashboard again."
  (equal buffer-name "*dashboard*"))

(defun cj/--split-companion-buffer ()
  "Buffer to show in the new window after a C-x 2 / C-x 3 split.
The dashboard, or the *scratch* buffer when splitting from the dashboard."
  (if (cj/--split-from-dashboard-p (buffer-name))
      (get-scratch-buffer-create)
    (cj/--dashboard-buffer)))

(defun cj/split-below-with-dashboard ()
  "Split below and show the companion buffer in the new window; stay in this one.
The companion is the dashboard, or *scratch* when splitting from the dashboard."
  (interactive)
  (cj/--split-show-buffer #'split-window-below (cj/--split-companion-buffer)))

(defun cj/split-right-with-dashboard ()
  "Split right and show the companion buffer in the new window; stay in this one.
The companion is the dashboard, or *scratch* when splitting from the dashboard."
  (interactive)
  (cj/--split-show-buffer #'split-window-right (cj/--split-companion-buffer)))

(keymap-global-set "C-x 2" #'cj/split-below-with-dashboard)
(keymap-global-set "C-x 3" #'cj/split-right-with-dashboard)

;; ------------------------- Split Window Reorientation ------------------------

(defun toggle-window-split ()
  "Toggle the orientation of the current window split.

If the window is split horizontally, change the split to vertical.
If it's vertical, change the split to horizontal.
Clears dedicated state on both windows so the toggle still works when
one is strongly dedicated (e.g. =*Org Agenda*=).  Without this, the
internal `set-window-buffer' call refuses to place a non-matching
buffer in a dedicated window and both panes end up showing the
dedicated buffer.
This function won't work with more than one split window."
  (interactive)
  (when (= (count-windows) 2)
	;; Clear dedicated up front: `set-window-buffer' rejects buffer swaps
	;; on strongly-dedicated windows.  The user explicitly invoked a
	;; layout change, so don't try to preserve dedicated through it.
	(set-window-dedicated-p (selected-window) nil)
	(set-window-dedicated-p (next-window) nil)
	(let* ((this-win-buffer (window-buffer))
		   (next-win-buffer (window-buffer (next-window)))
		   (this-win-edges (window-edges (selected-window)))
		   (next-win-edges (window-edges (next-window)))
		   (this-win-2nd (not (and (<= (car this-win-edges)
									   (car next-win-edges))
								   (<= (cadr this-win-edges)
									   (cadr next-win-edges)))))
		   (splitter
			(if (= (car this-win-edges)
				   (car (window-edges (next-window))))
				'split-window-horizontally
			  'split-window-vertically)))
	  (delete-other-windows)
	  (let ((first-win (selected-window)))
		(funcall splitter)
		(if this-win-2nd (other-window 1))
		(set-window-buffer (selected-window) this-win-buffer)
		(set-window-buffer (next-window) next-win-buffer)
		(select-window first-win)
		(if this-win-2nd (other-window 1))))))
(keymap-global-set "M-S-t" #'toggle-window-split)  ;; was M-T, overrides transpose-words

;; ---------------------------- Buffer Manipulation ----------------------------

;; MOVE BUFFER
;; allows changing buffer positions in a layout.
(use-package buffer-move
  :bind
  ("C-M-<down>"  . buf-move-down)
  ("C-M-<up>"    . buf-move-up)
  ("C-M-<left>"  . buf-move-left)
  ("C-M-<right>" . buf-move-right))


;; UNDO KILL BUFFER
(defun cj/undo-kill-buffer (arg)
  "Re-open the last buffer killed.
With numeric prefix ARG, re-open the ARGth most-recently-killed file
\(1-based, so no prefix re-opens the most recent)."
  (interactive "p")
  (require 'recentf)
  (unless recentf-mode
	(recentf-mode 1))
  (let ((recently-killed-list (copy-sequence recentf-list))
		(buffer-files-list
		 (delq nil (mapcar (lambda (buf)
							 (when (buffer-file-name buf)
							   (expand-file-name (buffer-file-name buf))))
						   (buffer-list)))))
	(mapc
	 (lambda (buf-file)
	   ;; delete (equal), not delq (eq): buf-file is a fresh string from
	   ;; expand-file-name and never eq to the recentf-list entries, so the
	   ;; skip-open-files logic was dead.
	   (setq recently-killed-list
			 (delete buf-file recently-killed-list)))
	 buffer-files-list)
	(when recently-killed-list
	  (let ((file (nth (1- arg) recently-killed-list)))
		(if file
			(find-file file)
		  (user-error "Only %d killed file(s) to choose from"
					  (length recently-killed-list)))))))
(keymap-global-set "M-S-z" #'cj/undo-kill-buffer)  ;; was M-Z, overrides zap-to-char

;; ---------------------------- Undo Layout Changes ----------------------------
;; allows you to restore your window setup with C-c left-arrow
;; or redo a window change with C-c right-arrow if you change your mind

(use-package winner
  :ensure nil ;; built-in
  :bind ("M-S-u" . winner-undo)  ;; was M-U, overrides upcase-word
  :config
  (winner-mode 1))

;; ------------------------------- Cursor Jump (avy) ---------------------------
;; Jump anywhere visible by typing a few of the target's characters, then the
;; decision-tree key avy overlays.  Fills the in-buffer motion gap that windmove
;; (windows) and isearch (text) leave.

(use-package avy
  :bind (("C-:"   . avy-goto-char-timer)   ;; type chars, pause, jump to a match
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

(provide 'ui-navigation)
;;; ui-navigation.el ends here
