;;; custom-file-buffer.el --- Custom Buffer and File Operations -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; ------------------------- Buffer And File Operations ------------------------

(defun cj/move-buffer-and-file (dir)
  "Move both current buffer and the file it visits to DIR."
  (interactive "DMove buffer and file (to new directory): ")
  (let* ((name (buffer-name))
		 (filename (buffer-file-name))
		 (dir
		  (if (string-match dir "\\(?:/\\|\\\\)$")
			  (substring dir 0 -1) dir))
		 (newname (concat dir "/" name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	  (progn  (copy-file filename newname 1)  (delete-file filename)
			  (set-visited-file-name newname)  (set-buffer-modified-p nil)  t))))

(defun cj/rename-buffer-and-file (new-name)
  "Rename both current buffer and the file it visits to NEW-NAME."
  (interactive
   (list (if (not (buffer-file-name))
			 (user-error "Buffer '%s' is not visiting a file!" (buffer-name))
		   (read-string "Rename buffer and file (to new name): "
						(file-name-nondirectory (buffer-file-name))))))
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (get-buffer new-name)
		(message "A buffer named '%s' already exists!" new-name)
	  (progn
		(rename-file filename new-name 1)
		(rename-buffer new-name)
		(set-visited-file-name new-name)
		(set-buffer-modified-p nil)))))

(defun cj/delete-buffer-and-file ()
  "Kill the current buffer and delete the file it visits."
  (interactive)
  (let ((filename (buffer-file-name)))
	(when filename
	  (if (vc-backend filename)
		  (vc-delete-file filename)
		(progn
		  (delete-file filename t)
		  (message "Deleted file %s" filename)
		  (kill-buffer))))))

(defun cj/copy-link-to-buffer-file ()
  "Copy the full file:// path of the current buffer's source file to the kill ring."
  (interactive)
  (let ((file-path (buffer-file-name)))
	(when file-path
	  (setq file-path (concat "file://" file-path))
	  (kill-new file-path)
	  (message "Copied file link to kill ring: %s" file-path))))

(defun cj/copy-path-to-buffer-file-as-kill ()
  "Copy the full path of the current buffer's file to the kill ring.
Signal an error if the buffer is not visiting a file."
  (interactive)
  (let ((path (buffer-file-name)))
	(if (not path)
		(user-error "Current buffer is not visiting a file")
	  (kill-new path)
	  (message "Copied file path: %s" path)
	  path)))

(defun cj/copy-whole-buffer ()
  "Copy the entire contents of the current buffer to the kill ring.
Point and mark are left exactly where they were.  No transient region
is created.  A message is displayed when done."
  (interactive)
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
	(kill-new contents)
	(message "Buffer contents copied to kill ring")))

(defun cj/clear-to-bottom-of-buffer ()
  "Delete all text from point to the end of the current buffer.
This does not save the deleted text in the kill ring."
  (interactive)
  (delete-region (point) (point-max))
  (message "Buffer contents removed to the end of the buffer."))

(defun cj/clear-to-top-of-buffer ()
  "Delete all text from point to the beginning of the current buffer.
Do not save the deleted text in the kill ring."
  (interactive)
  (delete-region (point) (point-min))
  (message "Buffer contents removed to the beginning of the buffer."))



;; ------------------------- Print Buffer As Postscript ------------------------

;; prints using postscript for much nicer output
(use-package ps-print
  :ensure nil                     ;; built-in
  :config
  (defun cj/print-buffer-ps ()
	"Print the current buffer as PostScript (monochrome) to the system default printer.
Sends directly to the spooler (no temp files), with no page header."
	(interactive)
	(let* ((spooler
			(cond
			 ((executable-find "lpr") "lpr")
			 ((executable-find "lp")  "lp")
			 (t (user-error "Cannot print: neither 'lpr' nor 'lp' found in PATH"))))
		   ;; Configure spooler for this invocation
		   (ps-lpr-command spooler)
		   (ps-printer-name nil)      ;; nil => system default printer
		   (ps-lpr-switches nil)
		   ;; Force monochrome and ignore face backgrounds for this job
		   (ps-print-color-p nil)
		   (ps-use-face-background nil)
		   ;; Ensure no headers
		   (ps-print-header nil)
		   (ps-header-lines 0)
		   (ps-left-header nil)
		   (ps-right-header nil))
	  (ps-print-buffer-with-faces)
	  (message "Sent print job via %s to default printer (no header)" spooler))))

;; --------------------------- Buffer And File Keymap --------------------------

;; Buffer & file operations prefix and keymap
(define-prefix-command 'cj/buffer-and-file-map nil
					   "Keymap for buffer-and-file operations.")
(define-key cj/custom-keymap "b" 'cj/buffer-and-file-map)
(define-key cj/buffer-and-file-map "m" 'cj/move-buffer-and-file)
(define-key cj/buffer-and-file-map "r" 'cj/rename-buffer-and-file)
(define-key cj/buffer-and-file-map "p" 'cj/print-buffer-ps)
(define-key cj/buffer-and-file-map "d" 'cj/delete-buffer-and-file)
(define-key cj/buffer-and-file-map "c" 'cj/copy-whole-buffer)
(define-key cj/buffer-and-file-map "t" 'cj/clear-to-top-of-buffer)
(define-key cj/buffer-and-file-map "b" 'cj/clear-to-bottom-of-buffer)
(define-key cj/buffer-and-file-map "x" 'erase-buffer)
(define-key cj/buffer-and-file-map "s" 'write-file) ;; save as :)

(define-key cj/buffer-and-file-map "l" 'cj/copy-link-to-buffer-file)
(define-key cj/buffer-and-file-map "P" 'cj/copy-path-to-buffer-file-as-kill)

(provide 'custom-file-buffer)
;;; custom-file-buffer.el ends here.
