;;; undead-buffers.el --- Bury Rather Than Kill These Buffers -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;;
;; This library allows for "burying" selected buffers instead of killing them.
;; Since they won't be killed, I'm calling them "undead buffers".
;; The main function cj/kill-buffer-or-bury-alive replaces kill-buffer.
;;
;; Additional helper commands and key bindings:
;;  - C-; b k (=cj/kill-buffer-and-window=): delete this window and bury/kill its buffer.
;;  - M-O (=cj/kill-other-window=): delete the next window and bury/kill its buffer.
;;  - M-M (=cj/kill-all-other-buffers-and-windows=): kill or bury all buffers except
;;    the current one and delete all other windows.
;;
;; Add to the list of "undead buffers" by adding to the cj/buffer-bury-alive-list
;; variable.
;;
;;; Code:

(defvar cj/undead-buffer-list
  '("*scratch*" "*EMMS-Playlist*" "*Messages*" "*ert*"
    "*AI-Assistant*")
  "Buffers to bury instead of killing.")

(defun cj/make-buffer-undead (name)
  "Append NAME to `cj/undead-buffer-list' if not present.
Signal an error if NAME is not a non-empty string. Return the updated list."
  (unless (and (stringp name) (> (length name) 0))
    (error "cj/bury-alive-add: NAME must be a non-empty string"))
  (add-to-list 'cj/undead-buffer-list name t))

(defun cj/kill-buffer-or-bury-alive (buffer)
  "Kill BUFFER or bury it if it's in `cj/undead-buffer-list'."
  (interactive "bBuffer to kill or bury: ")
  (with-current-buffer buffer
	(if current-prefix-arg
		(progn
          (add-to-list 'cj/undead-buffer-list (buffer-name))
		  (message "Added %s to bury-alive-list" (buffer-name)))
      (if (member (buffer-name) cj/undead-buffer-list)
		  (bury-buffer)
		(kill-buffer)))))
(keymap-global-set "<remap> <kill-buffer>" #'cj/kill-buffer-or-bury-alive)

(defun cj/undead-buffer-p ()
  "Replacement for `save-some-buffers' skips undead-buffers.
Undead-buffers are buffers in `cj/undead-buffer-list'."
  (let* ((buf (current-buffer))
		 (name (buffer-name buf)))
	(and
     (not (member name cj/undead-buffer-list))
	 (buffer-file-name buf)
	 (buffer-modified-p buf))))

(defun cj/save-some-buffers (&optional arg)
  "Save some buffers, omitting those in `cj/undead-buffer-list'.
ARG is passed to `save-some-buffers'."
  (interactive "P")
  (save-some-buffers arg #'cj/undead-buffer-p))

(defun cj/kill-buffer-and-window ()
  "Delete window and kill or bury its buffer."
  (interactive)
  (let ((buf (current-buffer)))
	(unless (one-window-p)
	  (delete-window))
	(cj/kill-buffer-or-bury-alive buf)))
;; Keybinding moved to custom-buffer-file.el (C-; b k)

(defun cj/kill-other-window ()
  "Delete the next window and kill or bury its buffer."
  (interactive)
  (other-window 1)
  (let ((buf (current-buffer)))
	(unless (one-window-p)
	  (delete-window))
	(cj/kill-buffer-or-bury-alive buf)))
(keymap-global-set "M-O" #'cj/kill-other-window)

(defun cj/kill-all-other-buffers-and-windows ()
  "Kill or bury all other buffers, then delete other windows."
  (interactive)
  (cj/save-some-buffers)
  (delete-other-windows)
  (mapc #'cj/kill-buffer-or-bury-alive
		(delq (current-buffer) (buffer-list))))
(keymap-global-set "M-M" #'cj/kill-all-other-buffers-and-windows)

(provide 'undead-buffers)
;;; undead-buffers.el ends here
