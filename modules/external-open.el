;;; external-open.el --- Open Files Using Default OS Handler -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; This library provides a simple mechanism for opening files with specific
;; extensions using your operating system’s default application rather than
;; visiting them in an Emacs buffer.  It offers:
;;
;; • A simple method to run a command on the current buffer's file
;;   "C-c x o" bound to cj/open-this-file-with
;; • A customizable list =default-open-extensions= of file‐type suffixes
;;   (e.g. “pdf”, “docx”, “png”) that should be handled externally.
;; • A function =default-open-file= (and its helper commands) which will
;;   launch the matching file in the OS’s default MIME handler.
;; • Integration with =find-file-hook= so that any file whose extension
;;   appears in =default-open-extensions= is automatically opened externally
;;   upon visit.
;; • Optional interactive commands for manually invoking an external open on
;;   point or on a user-chosen file.
;;
;;; Code:

(require 'host-environment) ;; environment information functions

;; ------------------------------- Open File With ------------------------------

(defun cj/open-this-file-with (command)
  "Asynchronously run COMMAND on the current buffer's file."
  (interactive "MOpen with program: ")
  (let ((display-buffer-alist
		 '(("\\*Async Shell Command\\*" display-buffer-no-window))))
	(async-shell-command (format "%s \"%s\"" command buffer-file-name))))
(global-set-key (kbd "C-c x o") #'cj/open-this-file-with)

;; ------------------------- Use Default File Handlers -------------------------

(defun cj/identify-external-open-command ()
  "Return the OS-default \"open\" command for this host.
Signals an error if the host is unsupported."
  (cond
   ((env-linux-p)   "xdg-open")
   ((env-macos-p)   "open")
   ((env-windows-p) "start")
   (t (error "external-open: unsupported host environment"))))

(defun cj/xdg-open (&optional filename)
  "Open FILENAME (or the file at point) with the OS default handler.
Logs output and exit code to buffer *cj-xdg-open.log*."
  (interactive)
  (let* ((file (expand-file-name
				(or filename
					(dired-file-name-at-point))))
		 (cmd  (cj/identify-external-open-command))
		 (logbuf (get-buffer-create "*external-open.log*")))
	(with-current-buffer logbuf
	  (goto-char (point-max))
	  (insert (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time)))
	  (insert (format "Opening: %s\n" file)))
	;; Use call-process with nowait flag
	(call-process cmd nil 0 nil file)
	;; Log completion
	(with-current-buffer logbuf
	  (insert "  → Launched asynchronously\n"))
	nil))

(defun cj/find-file-auto (orig-fun &rest args)
  "If file is media or Office, open exernally, else call ORIG-FUN with ARGS."
  (let ((file (car args))
		(exts '("\\.avi\\'"
				"\\.docx?\\'"
				"\\.m4a\\'"
				"\\.mkv\\'"
				"\\.webm\\'"
				"\\.mov\\'"
				"\\.flac\\'"
				"\\.mp3\\'"
				"\\.mp4\\'"
				"\\.ogg\\'"
				"\\.opus\\'"
				"\\.pptx?\\'"
				"\\.xlsx?\\'")))
	(if (cl-find-if (lambda (re) (string-match re file)) exts)
		(cj/xdg-open file)
	  (apply orig-fun args))))
(advice-add 'find-file :around #'cj/find-file-auto)

(provide 'external-open)
;;; external-open.el ends here.
