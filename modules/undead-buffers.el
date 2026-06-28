;;; undead-buffers.el --- Bury Rather Than Kill These Buffers -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C.
;; Load shape: eager.
;; Eager reason: global kill-buffer remap and window-kill bindings wanted from
;;   the first session.
;; Top-level side effects: three keymap-global-set (remaps kill-buffer; binds
;;   M-S-o, M-S-m).
;; Runtime requires: none.
;; Direct test load: yes.
;;
;; This library allows for "burying" selected buffers instead of killing them.
;; Since they won't be killed, I'm calling them "undead buffers".
;; The main function cj/kill-buffer-or-bury-alive replaces kill-buffer.
;;
;; Additional helper commands and key bindings:
;;  - C-; b k (=cj/kill-buffer-and-window=): delete this window and bury/kill its buffer.
;;  - C-; b K (=cj/kill-other-window-buffer=): bury/kill the other window's buffer,
;;    keeping that window and the split intact.
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
  "Buffer names to bury instead of killing (exact match).")

(defvar cj/undead-buffer-regexps nil
  "Regexps for buffer names to bury instead of killing, alongside
`cj/undead-buffer-list'.  Use for dynamically-named buffer families where an
exact name can't be pre-listed -- e.g. ai-term agents, named \"agent [<project>]\".
Register one with `cj/make-buffer-pattern-undead'.")

(defun cj/make-buffer-undead (name)
  "Append NAME to `cj/undead-buffer-list' if not present.
Signal an error if NAME is not a non-empty string. Return the updated list."
  (unless (and (stringp name) (> (length name) 0))
    (error "cj/bury-alive-add: NAME must be a non-empty string"))
  (add-to-list 'cj/undead-buffer-list name t))

(defun cj/make-buffer-pattern-undead (regexp)
  "Append REGEXP to `cj/undead-buffer-regexps' if not present.
A buffer whose name matches REGEXP is buried instead of killed.  Signal an
error if REGEXP is not a non-empty string.  Return the updated list."
  (unless (and (stringp regexp) (> (length regexp) 0))
    (error "cj/make-buffer-pattern-undead: REGEXP must be a non-empty string"))
  (add-to-list 'cj/undead-buffer-regexps regexp t))

(defun cj/--buffer-undead-p (name)
  "Return non-nil when buffer NAME should be buried instead of killed.
NAME is undead when it is in `cj/undead-buffer-list' (exact) or matches any
regexp in `cj/undead-buffer-regexps'."
  (and (stringp name)
       (or (member name cj/undead-buffer-list)
           (seq-some (lambda (re) (string-match-p re name))
                     cj/undead-buffer-regexps))))

(defun cj/kill-buffer-or-bury-alive (buffer)
  "Kill BUFFER or bury it if it's in `cj/undead-buffer-list'."
  (interactive "bBuffer to kill or bury: ")
  (with-current-buffer buffer
	(if current-prefix-arg
		(progn
          (add-to-list 'cj/undead-buffer-list (buffer-name))
		  (message "Added %s to bury-alive-list" (buffer-name)))
      (if (cj/--buffer-undead-p (buffer-name))
		  (bury-buffer)
		(kill-buffer)))))
(keymap-global-set "<remap> <kill-buffer>" #'cj/kill-buffer-or-bury-alive)

(defun cj/undead-buffer-p ()
  "Replacement for `save-some-buffers' skips undead-buffers.
Undead-buffers are buffers in `cj/undead-buffer-list'."
  (let* ((buf (current-buffer))
		 (name (buffer-name buf)))
	(and
     (not (cj/--buffer-undead-p name))
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
(keymap-global-set "M-S-o" #'cj/kill-other-window)

(defun cj/kill-other-window-buffer ()
  "Kill or bury the buffer shown in the other window, keeping that window
and the split intact -- the window then shows whatever bury/kill surfaces
next.  With more than two windows, acts on `next-window'.

Sibling of `cj/kill-other-window', which deletes the other window; here the
split is preserved.  Buffers in `cj/undead-buffer-list' are buried."
  (interactive)
  (if (one-window-p)
	  (user-error "No other window")
	(with-selected-window (next-window)
	  (cj/kill-buffer-or-bury-alive (current-buffer)))))
;; Keybinding in custom-buffer-file.el (C-; b K)

(defun cj/kill-all-other-buffers-and-windows ()
  "Kill or bury all other buffers, then delete other windows."
  (interactive)
  (cj/save-some-buffers)
  (delete-other-windows)
  (mapc #'cj/kill-buffer-or-bury-alive
		(delq (current-buffer) (buffer-list))))
(keymap-global-set "M-S-m" #'cj/kill-all-other-buffers-and-windows)  ;; was M-M

(provide 'undead-buffers)
;;; undead-buffers.el ends here
