;;; ui-navigation --- Managing Cursor Placement, Buffers, and Windows -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

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

;; ------------------------------ Window Placement -----------------------------

(use-package windmove
  :defer .5
  :config
  (windmove-default-keybindings))  ; move cursor around with shift+arrows

;; ------------------------------ Window Resizing ------------------------------

(use-package windsize
  :defer .5
  :bind
  ("C-s-<left>"  . windsize-left)
  ("C-s-<right>" . windsize-right)
  ("C-s-<up>"    . windsize-up)
  ("C-s-<down>"  . windsize-down))

;; M-shift = to balance multiple split windows
(global-set-key (kbd "M-+") 'balance-windows)

;; ------------------------------ Window Splitting -----------------------------

(defun cj/split-and-follow-right ()
  "Like =ivy-switch-buffer-other-window' but always split horizontally."
  (interactive)
  (if (fboundp 'ivy-switch-buffer-other-window)
	  (let ((split-height-threshold nil)  ; Disable vertical splitting
			(split-width-threshold 0))    ; Always prefer horizontal splitting
		(ivy-switch-buffer-other-window))
	;; Fallback if ivy is not available
	(split-window-right)
	(other-window 1)
	(switch-to-buffer (other-buffer))))
(global-set-key (kbd "M-V") 'cj/split-and-follow-right)

(defun cj/split-and-follow-below ()
  "Like =ivy-switch-buffer-other-window' but always split vertically."
  (interactive)
  (if (fboundp 'ivy-switch-buffer-other-window)
	  (let ((split-height-threshold 0)  ; Always prefer vertical splitting
			(split-width-threshold nil)) ; Disable horizontal splitting
		(ivy-switch-buffer-other-window))
	;; Fallback if ivy is not available
	(split-window-below)
	(other-window 1)
	(switch-to-buffer (other-buffer))))
(global-set-key (kbd "M-H") 'cj/split-and-follow-below)

;; (defun cj/split-and-follow-right ()
;;   "Open a new window horizontally to the right of the current window.
;; Open ibuffer in the other window for easy buffer selection."
;;   (interactive)
;;   (split-window-right)
;;   (other-window 1)
;;   (ibuffer))

;; (defun cj/split-and-follow-below ()
;;   "Open a new window vertically below the current window.
;; Open ibuffer in the other window for easy buffer selection."
;;   (interactive)
;;   (split-window-below)
;;   (other-window 1)
;;   (ibuffer))

;; ------------------------- Split Window Reorientation ------------------------

(defun toggle-window-split ()
  "Toggle the orientation fo the window split.
If the window is split horizontally, change the split to be vertical.
If it's vertical, change the split to be to horizontal.
This function won't work with more than one split window."
  (interactive)
  (if (= (count-windows) 2)
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
(global-set-key (kbd "M-T") 'toggle-window-split)

;; SWAP WINDOW POSITIONS
(global-set-key (kbd "M-S") 'window-swap-states)

;; ---------------------------- Buffer Manipulation ----------------------------

;; MOVE BUFFER
;; allows you to move between buffers
(use-package buffer-move
  ;; :straight (buffer-move :type git :host github :repo "lukhas/buffer-move"
  ;;                        :fork (:host github :repo "cjennings/buffer-move"))
  :bind
  ("M-S-<down>"  . buf-move-down)
  ("M-S-<up>"    . buf-move-up)
  ("M-S-<left>"  . buf-move-left)
  ("M-S-<right>" . buf-move-right))


;; UNDO KILL BUFFER
(defun cj/undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
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
	   (setq recently-killed-list
			 (delq buf-file recently-killed-list)))
	 buffer-files-list)
	(when recently-killed-list
	  (find-file
	   (if arg (nth arg recently-killed-list)
		 (car recently-killed-list))))))
(global-set-key (kbd "M-Z") 'cj/undo-kill-buffer)

;; ---------------------------- Undo Layout Changes ----------------------------
;; allows you to restore your window setup with C-c left-arrow
;; or redo a window change with C-c right-arrow if you change your mind

(use-package winner
  :ensure nil ;; built-in
  :defer .5
  :bind ("M-U" . winner-undo)
  :config
  (winner-mode 1))

(provide 'ui-navigation)
;;; ui-navigation.el ends here
