;;; file-config.el --- Open Files Using Default OS Handler -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; This library provides a simple mechanism for opening files with specific
;; extensions using your operating system’s default application rather than
;; visiting them in an Emacs buffer.  It offers:

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

(defun cj/xdg-open-command ()
  "Return the OS-default \"open\" command for this host.
Signals an error if the host is unsupported."
  (cond
   ((env-linux-p)   "xdg-open")
   ((env-macos-p)   "open")
   ((env-windows-p) "start")
   (t (error "cj/xdg-open: unsupported host environment"))))

(defun cj/xdg-open (&optional filename)
  "Open FILENAME (or the file at point) with the OS default handler.
Logs output and exit code to buffer *cj-xdg-open.log*."
  (interactive)
  (let* ((file (expand-file-name
				(or filename
					(dired-file-name-at-point))))
		 (cmd  (cj/xdg-open-command))
		 (logbuf (get-buffer-create "*cj-xdg-open.log*"))
		 exit-code)
	(with-current-buffer logbuf
	  (goto-char (point-max))
	  (insert (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time)))
	  (insert (format "Running `%s %s`\n" cmd file))
	  (setq exit-code
			(call-process cmd nil logbuf t file))
	  (insert (format "Exit code: %d\n\n" exit-code)))
	exit-code))

(defun cj/find-file-auto (orig-fun &rest args)
  "If file is media or Office, open via xdg-open, else call ORIG-FUN with ARGS."
  (let ((file (car args))
		(exts '("\\.avi\\'"
				"\\.mp4\\'"
				"\\.mkv\\'"
				"\\.mov\\'"
				"\\.mp3\\'"
				"\\.ogg\\'"
				"\\.docx?\\'"
				"\\.pptx?\\'"
				"\\.xlsx?\\'")))
	(if (cl-find-if (lambda (re) (string-match re file)) exts)
		(cj/xdg-open file)
	  (apply orig-fun args))))
(advice-add 'find-file :around #'cj/find-file-auto)

(provide 'file-config)
;;; file-config.el ends here.

;; --------------------------------- ERT Tests ---------------------------------
;; Run these tests with M-x ert RET t RET

(require 'ert)
(require 'cl-lib)


(ert-deftest file-config/open-this-file-with-invokes-async-shell-command ()
  "Ensure `cj/open-this-file-with` calls `async-shell-command` with the
program name and the buffer's file name, properly quoted."
  (let* ((test-file (expand-file-name "space in name.txt"
									  temporary-file-directory))
		 called-cmd)
	;; Create a temp buffer and pretend its file is TEST-FILE
	(with-temp-buffer
	  (set (make-local-variable 'buffer-file-name) test-file)
	  ;; Override `async-shell-command` to capture its argument
	  (cl-letf (((symbol-function 'async-shell-command)
				 (lambda (cmd &rest _)
				   (setq called-cmd cmd))))
		(cj/open-this-file-with "myprog")))
	;; Now assert it was called with: myprog "full/path/to/space in name.txt"
	(should (string= called-cmd
					 (format "myprog \"%s\"" test-file)))))

(ert-deftest file-config/find-file-auto-opens-matching-extension ()
  "cj/find-file-auto should invoke `cj/xdg-open' when the filename has a media/Office extension."
  (let* ((called-file nil)
		 (test-file "/tmp/video.mp4")
		 ;; stash the real function so we can restore it later
		 (orig-xdg-open (symbol-function 'cj/xdg-open)))
	(cl-letf (((symbol-function 'cj/xdg-open)
			   (lambda (file)
				 (setq called-file file)
				 ;; pretend we returned an exit code
				 0)))
	  (unwind-protect
		  (progn
			;; call the advice wrapper as if Emacs were doing (find-file test-file)
			(cj/find-file-auto
			 (lambda (_file) (error "Should not call original find-file"))
			 test-file)
			(should (equal called-file test-file)))
		;; restore
		(fset 'cj/xdg-open orig-xdg-open)))))

(ert-deftest file-config/find-file-auto-falls-back-for-nonmatching ()
  "cj/find-file-auto should fall back to the original FIND-FILE when the extension does not match."
  (let ((called-file nil)
		(test-file "/tmp/document.txt"))
	(cj/find-file-auto
	 (lambda (file)
	   (setq called-file file)
	   ;; pretend that visiting succeeded
	   t)
	 test-file)
	(should (equal called-file test-file))))

(ert-deftest file-config/xdg-open-logs-and-returns-exit-code ()
  "cj/xdg-open should call the OS command, log its invocation+exit code, and return that code."
  (let* ((dummy-file (make-temp-file "cj-test" nil ".foo"))
		 (logbuf     (get-buffer-create "*cj-xdg-open.log*"))
		 ;; override the command to something harmless
		 (orig-cmd-fn (symbol-function 'cj/xdg-open-command)))
	(cl-letf (((symbol-function 'cj/xdg-open-command)
			   (lambda () "true")))
	  (unwind-protect
		  (let ((code (cj/xdg-open dummy-file)))
			(should (= code 0))
			(with-current-buffer logbuf
			  (goto-char (point-min))
			  (should (search-forward (format "`true %s`" dummy-file) nil t))
			  (should (search-forward "Exit code: 0" nil t))))
		;; cleanup
		(kill-buffer logbuf)
		(fset 'cj/xdg-open-command orig-cmd-fn)))))
