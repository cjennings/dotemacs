;;; external-open.el --- Open Files Using Default OS Handler -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/D.
;; Load shape: eager.
;; Eager reason: command library with no side effects; eager only by init order.
;;   A deferral candidate (autoload commands) for Phase 4.
;; Top-level side effects: none.
;; Runtime requires: host-environment, system-lib, external-open-lib, cl-lib.
;; Direct test load: yes (pure command helpers).
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
(require 'system-lib) ;; for cj/file-from-context
(require 'external-open-lib) ;; pure dispatch helpers
(require 'cl-lib)

;; Declare platform-specific functions
(declare-function w32-shell-execute "w32fns.c" (operation document &optional parameters show-flag))

(defgroup external-open nil
  "Open certain files with the OS default handler."
  :group 'files)

(defcustom cj/video-extensions
  '("\\.3g2\\'" "\\.3gp\\'" "\\.asf\\'" "\\.avi\\'" "\\.divx\\'" "\\.dv\\'"
	"\\.f4v\\'" "\\.flv\\'" "\\.m1v\\'" "\\.m2ts\\'" "\\.m2v\\'" "\\.m4v\\'"
	"\\.mkv\\'" "\\.mov\\'" "\\.mpe\\'" "\\.mpeg\\'" "\\.mpg\\'" "\\.mp4\\'"
	"\\.mts\\'" "\\.ogv\\'" "\\.rm\\'" "\\.rmvb\\'" "\\.vob\\'"
	"\\.webm\\'" "\\.wmv\\'")
  "Regexps matching video files opened in a looping player.
These route through `cj/open-video-looping' (mpv --loop-file=inf by default)
instead of the OS default handler, so a video opened from dirvish plays on
repeat."
  :type '(repeat (regexp :tag "Video extension regexp"))
  :group 'external-open)

(defcustom cj/video-open-command "mpv"
  "Player command used to open local video files on repeat.
Launched detached from Emacs with `cj/video-open-args' before the file name."
  :type 'string
  :group 'external-open)

(defcustom cj/video-open-args '("--loop-file=inf")
  "Arguments passed to `cj/video-open-command' before the file name.
Defaults to mpv's infinite single-file loop so the video plays on repeat."
  :type '(repeat string)
  :group 'external-open)

(defcustom default-open-extensions
  '(
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

(defun cj/xdg-open (&optional filename)
  "Open FILENAME (or the file at point) with the OS default handler.
Logs output and exit code to buffer *external-open.log*."
  (interactive)
  (let* ((file  (expand-file-name
                 (or (cj/file-from-context filename)
                     (user-error "No file associated with this buffer"))))
         (cmd   (or (cj/external-open-command)
                    (user-error "External-open: unsupported host environment")))
         (logbuf (get-buffer-create "*external-open.log*")))
    (with-current-buffer logbuf
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (format "Opening: %s\n" file)))
    (cond
     ((env-windows-p)
      (w32-shell-execute "open" file))
     (t
      (call-process cmd nil 0 nil file)
      (with-current-buffer logbuf
        (insert "  → Launched asynchronously\n"))))
    nil))

;; ------------------------------- Open File With ------------------------------

(defun cj/--open-with-argv (command file)
  "The argv list to open FILE with the user-typed COMMAND.
COMMAND may carry arguments (\"mpv --fs\"); `split-string-and-unquote'
splits it so a double-quoted argument survives as one word.  FILE is
appended as the final element, so paths with spaces or shell
metacharacters never meet a shell.  Signals a `user-error' when COMMAND
is empty or whitespace."
  (let ((argv (split-string-and-unquote command)))
	(unless argv
	  (user-error "No program given"))
	(append argv (list file))))

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
	 ;; POSIX: argv launch, DESTINATION 0 detaches with no shell in between.
	 (t
	  (let ((argv (cj/--open-with-argv command file)))
		(unless (executable-find (car argv))
		  (user-error "Program not found: %s" (car argv)))
		(apply #'call-process (car argv) nil 0 nil (cdr argv)))))))


;; -------------------------- Open Videos On Repeat ----------------------------

(defun cj/--video-file-p (file)
  "Return non-nil when FILE matches a regexp in `cj/video-extensions'."
  (and (stringp file)
	   (let ((case-fold-search t))
		 (cl-some (lambda (re) (string-match-p re file)) cj/video-extensions))))

(defun cj/--video-open-arglist (file)
  "Return the argument list to play FILE on repeat: `cj/video-open-args' + FILE."
  (append cj/video-open-args (list file)))

(defun cj/open-video-looping (&optional filename)
  "Open FILENAME (or the file at point) in a looping video player, detached.
Uses `cj/video-open-command' and `cj/video-open-args' (mpv --loop-file=inf by
default) so the video plays on repeat.  Launched asynchronously so it never
blocks Emacs."
  (interactive)
  (let* ((file (expand-file-name
				(or (cj/file-from-context filename)
					(user-error "No file associated with this buffer"))))
		 (args (cj/--video-open-arglist file)))
	(if (env-windows-p)
		(w32-shell-execute "open" cj/video-open-command
						   (mapconcat (lambda (a) (format "\"%s\"" a)) args " "))
	  ;; Guard like `cj/open-this-file-with': this fires via the find-file
	  ;; advice, so a missing player must fail with a clear message, not an
	  ;; opaque call-process error mid-visit.
	  (unless (executable-find cj/video-open-command)
		(user-error "Program not found: %s" cj/video-open-command))
	  (apply #'call-process cj/video-open-command nil 0 nil args))))

;; -------------------- Open Files With Default File Handler -------------------

(defun cj/find-file-auto (orig-fun &rest args)
  "Open FILE externally based on its extension, else call ORIG-FUN with ARGS.
A video (`cj/video-extensions') opens in a looping player; any other extension
in `default-open-extensions' opens with the OS default handler."
  (let* ((file (car args))
		 (case-fold-search t))
	(cond
	 ((cj/--video-file-p file)
	  (cj/open-video-looping file))
	 ((and (stringp file)
		   (cl-some (lambda (re) (string-match-p re file))
					default-open-extensions))
	  (cj/xdg-open file))
	 (t (apply orig-fun args)))))

(defun cj/external-open-install-advice ()
  "Install the `cj/find-file-auto' advice on `find-file'.
Idempotent: re-running removes any prior copy of the advice before
adding it back, so re-evaluating the module updates the advice rather
than stacking it."
  (advice-remove 'find-file #'cj/find-file-auto)
  (advice-add 'find-file :around #'cj/find-file-auto))

(cj/external-open-install-advice)

(provide 'external-open)
;;; external-open.el ends here.
