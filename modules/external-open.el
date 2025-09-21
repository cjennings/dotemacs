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
(require 'cl-lib)

(defgroup external-open nil
  "Open certain files with the OS default handler."
  :group 'files)

(defcustom default-open-extensions
  '(
	;; Video
	"\\.3g2\\'" "\\.3gp\\'" "\\.asf\\'" "\\.avi\\'" "\\.divx\\'" "\\.dv\\'"
	"\\.f4v\\'" "\\.flv\\'" "\\.m1v\\'" "\\.m2ts\\'" "\\.m2v\\'" "\\.m4v\\'"
	"\\.mkv\\'" "\\.mov\\'" "\\.mpe\\'" "\\.mpeg\\'" "\\.mpg\\'" "\\.mp4\\'"
	"\\.mts\\'" "\\.ogv\\'" "\\.rm\\'" "\\.rmvb\\'" "\\.ts\\'" "\\.vob\\'"
	"\\.webm\\'" "\\.wmv\\'"

	;; Audio
	"\\.aac\\'" "\\.ac3\\'" "\\.aif\\'" "\\.aifc\\'" "\\.aiff\\'"
	"\\.alac\\'" "\\.amr\\'" "\\.ape\\'" "\\.caf\\'"
	"\\.dff\\'" "\\.dsf\\'" "\\.flac\\'" "\\.m4a\\'" "\\.mka\\'"
	"\\.mid\\'" "\\.midi\\'" "\\.mp2\\'" "\\.mp3\\'" "\\.oga\\'"
	"\\.ogg\\'" "\\.opus\\'" "\\.ra\\'" "\\.spx\\'" "\\.wav\\'"
	"\\.wave\\'" "\\.weba\\'" "\\.wma\\'"

	;; Microsoft Word
	"\\.docx?\\'" "\\.docm\\'"
	"\\.dotx?\\'" "\\.dotm\\'"
	"\\.rtf\\'"

	;; Microsoft Excel
	"\\.xlsx?\\'" "\\.xlsm\\'" "\\.xlsb\\'"
	"\\.xltx?\\'" "\\.xltm\\'"

	;; Microsoft PowerPoint
	"\\.pptx?\\'" "\\.pptm\\'"
	"\\.ppsx?\\'" "\\.ppsm\\'"
	"\\.potx?\\'" "\\.potm\\'"

	;; Microsoft OneNote / Visio / Project / Access / Publisher
	"\\.one\\'" "\\.onepkg\\'" "\\.onetoc2\\'"
	"\\.vsdx?\\'" "\\.vsdm\\'" "\\.vstx?\\'" "\\.vstm\\'" "\\.vssx?\\'" "\\.vssm\\'"
	"\\.mpp\\'" "\\.mpt\\'"
	"\\.mdb\\'" "\\.accdb\\'" "\\.accde\\'" "\\.accdr\\'" "\\.accdt\\'"
	"\\.pub\\'"

	;; OpenDocument (LibreOffice/OpenOffice)
	"\\.odt\\'" "\\.ott\\'"
	"\\.ods\\'" "\\.ots\\'"
	"\\.odp\\'" "\\.otp\\'"
	"\\.odg\\'" "\\.otg\\'"
	"\\.odm\\'" "\\.odf\\'"
	;; Flat OpenDocument variants
	"\\.fodt\\'" "\\.fods\\'" "\\.fodp\\'"

	;; Apple iWork
	"\\.pages\\'" "\\.numbers\\'" "\\.key\\'"

	;; Microsoft’s fixed-layout formats
	"\\.xps\\'" "\\.oxps\\'"
	)
  "Regexps matching file extensions that should be opened externally."
  :type '(repeat (regexp :tag "File extension regexp"))
  :group 'external-open)

;; ------------------------------- Open File With ------------------------------

(defun cj/open-this-file-with (command)
  "Open this buffer's file with COMMAND, detached from Emacs."
  (interactive "MOpen with program: ")
  (unless buffer-file-name
	(user-error "Current buffer is not visiting a file"))
  (let ((file (expand-file-name buffer-file-name)))
	(cond
	 ;; Windows: launch via ShellExecute so the child isn't tied to Emacs.
	 ((env-windows-p)
	  (w32-shell-execute "open" command (format "\"%s\"" file)))
	 ;; POSIX: disown with nohup + background. No child remains.
	 (t
	  (call-process-shell-command
	   (format "nohup %s %s >/dev/null 2>&1 &"
			   command (shell-quote-argument file))
	   nil 0)))))

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

Logs output and exit code to buffer *external-open.log*."
  (interactive)
  (let* ((file  (expand-file-name (or filename (dired-file-name-at-point))))
		 (cmd   (cj/identify-external-open-command))
		 (logbuf (get-buffer-create "*external-open.log*")))
	(with-current-buffer logbuf
	  (goto-char (point-max))
	  (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
	  (insert (format "Opening: %s\n" file)))
	(cond
	 ;; Windows: let the shell handle association; fully detached.
	 ((env-windows-p)
	  (w32-shell-execute "open" file))
	 ;; macOS/Linux: run the opener synchronously; it returns immediately.
	 (t
	  (call-process cmd nil 0 nil file)
	  (with-current-buffer logbuf
		(insert "  → Launched asynchronously\n"))))
	nil))

(defun cj/find-file-auto (orig-fun &rest args)
  "If file has an extension in `default-open-extensions', open externally.

Else call ORIG-FUN with ARGS."
  (let* ((file (car args))
		 (case-fold-search t))
	(if (and (stringp file)
			 (cl-some (lambda (re) (string-match-p re file))
					  default-open-extensions))
		(cj/xdg-open file)
	  (apply orig-fun args))))

;; Make advice idempotent if you reevaluate this form.
(advice-remove 'find-file #'cj/find-file-auto)
(advice-add 'find-file :around #'cj/find-file-auto)

(provide 'external-open)
;;; external-open.el ends here.
