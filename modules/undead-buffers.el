;;; undead-buffers.el --- Bury Rather Than Kill These Buffers -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;;
;; This library allows for “burying” selected buffers instead of killing them.
;; Since they won't be killed, I'm calling them "undead buffers".
;; The main function cj/kill-buffer-or-bury-alive replaces kill-buffer.
;;
;; Additional helper commands and key bindings:
;;  - M-C (=cj/kill-buffer-and-window=): delete this window and bury/kill its buffer.
;;  - M-O (=cj/kill-other-window=): delete the next window and bury/kill its buffer.
;;  - M-M (=cj/kill-all-other-buffers-and-windows=): kill or bury all buffers except
;;    the current one and delete all other windows.
;;
;; Add to the list of "undead buffers" by adding to the cj/buffer-bury-alive-list
;; variable.
;;
;;; Code:

(defvar cj/buffer-bury-alive-list
  '("*dashboard*" "*scratch*" "*EMMS Playlist*" "*Messages*" "*ert*" "*AI-Assistant*")
  "Buffers to bury instead of killing.")

(defun cj/kill-buffer-or-bury-alive (buffer)
  "Kill BUFFER or bury it if it's in `cj/buffer-bury-alive-list'."
  (interactive "bBuffer to kill or bury: ")
  (with-current-buffer buffer
	(if current-prefix-arg
		(progn
		  (add-to-list 'cj/buffer-bury-alive-list (buffer-name))
		  (message "Added %s to bury-alive-list" (buffer-name)))
	  (if (member (buffer-name) cj/buffer-bury-alive-list)
		  (bury-buffer)
		(kill-buffer)))))
(global-set-key [remap kill-buffer] #'cj/kill-buffer-or-bury-alive)

(defun cj/undead-buffer-p ()
  "Predicate for =save-some-buffers= that skips buffers in =cj/buffer-bury-alive-list=."
  (let* ((buf (current-buffer))
		 (name (buffer-name buf)))
	(and
	 (not (member name cj/buffer-bury-alive-list))
	 (buffer-file-name buf)
	 (buffer-modified-p buf))))

(defun cj/save-some-buffers (&optional arg)
  "Save some buffers, omitting those in =cj/buffer-bury-alive-list=.
ARG is passed to =save-some-buffers=."
  (interactive "P")
  (save-some-buffers arg #'cj/undead-buffer-p))

(defun cj/kill-buffer-and-window ()
  "Delete window and kill or bury its buffer."
  (interactive)
  (let ((buf (current-buffer)))
	(delete-window)
	(cj/kill-buffer-or-bury-alive buf)))
(global-set-key (kbd "M-C") #'cj/kill-buffer-and-window)

(defun cj/kill-other-window ()
  "Delete the next window and kill or bury its buffer."
  (interactive)
  (other-window 1)
  (let ((buf (current-buffer)))
	(unless (one-window-p)
	  (delete-window))
	(cj/kill-buffer-or-bury-alive buf)))
(global-set-key (kbd "M-O") #'cj/kill-other-window)

(defun cj/kill-all-other-buffers-and-windows ()
  "Kill or bury all other buffers, then delete other windows."
  (interactive)
  (cj/save-some-buffers)
  (delete-other-windows)
  (mapc #'cj/kill-buffer-or-bury-alive
		(delq (current-buffer) (buffer-list))))
(global-set-key (kbd "M-M") #'cj/kill-all-other-buffers-and-windows)

(provide 'undead-buffers)
;;; undead-buffers.el ends here.

;; --------------------------------- ERT Tests ---------------------------------
;; Run these tests with M-x ert RET t RET

(require 'ert)
(require 'cl-lib)

(ert-deftest undead-buffers/kill-or-bury-when-not-in-list-kills ()
  "cj/kill-buffer-or-bury-alive should kill a buffer not in `cj/buffer-bury-alive-list'."
  (let* ((buf   (generate-new-buffer "test-not-in-list"))
		 (orig  (copy-sequence cj/buffer-bury-alive-list)))
	(unwind-protect
		(progn
		  (should (buffer-live-p buf))
		  (cj/kill-buffer-or-bury-alive (buffer-name buf))
		  (should-not (buffer-live-p buf)))
	  (setq cj/buffer-bury-alive-list orig)
	  (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest undead-buffers/kill-or-bury-when-in-list-buries ()
  "cj/kill-buffer-or-bury-alive should bury (not kill) a buffer in the list."
  (let* ((name  "*dashboard*")      ; an element already in the default list
		 (buf   (generate-new-buffer name))
		 (orig  (copy-sequence cj/buffer-bury-alive-list))
		 win-was)
	(unwind-protect
		(progn
		  (add-to-list 'cj/buffer-bury-alive-list name)
		  ;; show it in a temporary window so we can detect bury
		  (setq win-was (display-buffer buf))
		  (cj/kill-buffer-or-bury-alive name)
		  ;; bury should leave it alive
		  (should (buffer-live-p buf))
		  ;; note: Emacs’s `bury-buffer` does not delete windows by default,
		  ;; so we no longer assert that no window shows it.
		  )
	  ;; cleanup
	  (setq cj/buffer-bury-alive-list orig)
	  (delete-windows-on buf)
	  (kill-buffer buf))))

(ert-deftest undead-buffers/kill-or-bury-adds-to-list-with-prefix ()
  "Calling `cj/kill-buffer-or-bury-alive' with a prefix arg should add the buffer to the list."
  (let* ((buf   (generate-new-buffer "test-add-prefix"))
		 (orig  (copy-sequence cj/buffer-bury-alive-list)))
	(unwind-protect
		(progn
		  (let ((current-prefix-arg '(4)))
			(cj/kill-buffer-or-bury-alive (buffer-name buf)))
		  (should (member (buffer-name buf) cj/buffer-bury-alive-list)))
	  (setq cj/buffer-bury-alive-list orig)
	  (kill-buffer buf))))

(ert-deftest undead-buffers/kill-buffer-and-window-removes-window ()
  "cj/kill-buffer-and-window should delete the current window and kill/bury its buffer."
  (let* ((buf   (generate-new-buffer "test-kill-and-win"))
		 (orig  (copy-sequence cj/buffer-bury-alive-list)))
	(split-window)                   ; now two windows
	(let ((win (next-window)))
	  (set-window-buffer win buf)
	  (select-window win)
	  (cj/kill-buffer-and-window)
	  (should-not (window-live-p win))
	  (unless (member (buffer-name buf) orig)
		(should-not (buffer-live-p buf))))
	(setq cj/buffer-bury-alive-list orig)))

(ert-deftest undead-buffers/kill-other-window-deletes-that-window ()
  "cj/kill-other-window should delete the *other* window and kill/bury its buffer."
  (let* ((buf1  (current-buffer))
		 (buf2  (generate-new-buffer "test-other-window"))
		 (orig  (copy-sequence cj/buffer-bury-alive-list)))
	(split-window)
	(let* ((win1 (selected-window))
		   (win2 (next-window win1)))
	  (set-window-buffer win2 buf2)
	  ;; stay on the original window
	  (select-window win1)
	  (cj/kill-other-window)
	  (should-not (window-live-p win2))
	  (unless (member (buffer-name buf2) orig)
		(should-not (buffer-live-p buf2))))
	(setq cj/buffer-bury-alive-list orig)))

(ert-deftest undead-buffers/kill-all-other-buffers-and-windows-keeps-only-current ()
  "cj/kill-all-other-buffers-and-windows should delete other windows and kill/bury all other buffers."
  (let* ((main  (current-buffer))
		 (extra (generate-new-buffer "test-all-others"))
		 (orig  (copy-sequence cj/buffer-bury-alive-list)))
	(split-window)
	(set-window-buffer (next-window) extra)
	(cj/kill-all-other-buffers-and-windows)
	(should (one-window-p))
	;; main buffer still exists
	(should (buffer-live-p main))
	;; extra buffer either buried or killed
	(unless (member (buffer-name extra) orig)
	  (should-not (buffer-live-p extra)))
	;; cleanup
	(setq cj/buffer-bury-alive-list orig)
	(when (buffer-live-p extra) (kill-buffer extra))))
