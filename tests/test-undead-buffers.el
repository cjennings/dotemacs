;;; test-undead-buffers.el ---  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'undead-buffers)

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

(provide 'test-undead-buffers)
;;; test-undead-buffers.el ends here.
