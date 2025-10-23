;;; host-environment.el --- Host Environment Convenience Functions -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Convenience functions to report about the host environment
;;
;;; Code:

;; for the byte-compiler/linter
(declare-function battery-format "battery" (format data))
(defvar battery-status-function)



(defun env-laptop-p ()
  "Non-nil if a battery is present."
  (when (and (require 'battery nil 'noerror)
			 battery-status-function)
	(not (string= "N/A"
				  (battery-format "%B" (funcall battery-status-function))))))

(defun env-desktop-p ()
  "Return t if host is a laptop (has a battery), nil if not."
  (when (not (env-laptop-p))
	t))

(defun env-linux-p ()
  "Return t if host system is GNU/Linux."
  (eq system-type 'gnu/linux))

(defun env-bsd-p ()
  "Return t if host system is FreeBSD."
  (eq system-type 'berkeley-unix))

(defun env-macos-p ()
  "Return non-nil if running on macOS (Darwin)."
  (eq system-type 'darwin))

(defun env-windows-p ()
  "Return t if host system is Windows."
  (memq system-type '(cygwin windows-nt ms-dos)))

(defun env-x-p ()
  "Return t if host system is running the X Window System."
  (string= (window-system) "x"))

(defun env-terminal-p ()
  "Return t if running in a terminal."
  (not (display-graphic-p)))

(defun env-gui-p ()
  "Return t if running in graphical environment."
  (display-graphic-p))

;; ------------------------------- Timezone Info -------------------------------

(defun cj/match-localtime-to-zoneinfo ()
  "Detect system timezone by comparing /etc/localtime with zoneinfo files.
This replicates the shell command:
find /usr/share/zoneinfo -type f ! -name `posixrules' \\
  -exec cmp -s {} /etc/localtime \\;
  -print | sed -e \='s@.*/zoneinfo/@@\=' | head -n1"
  (when (and (file-exists-p "/etc/localtime")
			 (file-directory-p "/usr/share/zoneinfo"))
	(let ((localtime-content
		   (with-temp-buffer
			 (insert-file-contents-literally "/etc/localtime")
			 (buffer-string)))
		  (result nil))
	  (catch 'found
		(dolist (file (directory-files-recursively
					   "/usr/share/zoneinfo"
					   ".*"
					   nil
					   (lambda (name)
						 (not (string-match-p "posixrules" name)))))
		  (when (and (file-regular-p file)
					 (not (string-match-p "/posixrules$" file)))
			(let ((file-content
				   (with-temp-buffer
					 (insert-file-contents-literally file)
					 (buffer-string))))
			  (when (string= localtime-content file-content)
				(setq result (replace-regexp-in-string
							  "^.*/zoneinfo/" "" file))
				(throw 'found result)))))
		result))))

(defun cj/detect-system-timezone ()
  "Detect the system timezone in IANA format (e.g., `America/Los_Angeles').
Tries multiple methods in order of reliability:
1. Environment variable TZ
2. File comparison of /etc/localtime with zoneinfo database
3. /etc/timezone file contents
4. /etc/localtime symlink target"
  (or
   ;; Compare file contents (reliable on Arch/modern systems)
   (cj/match-localtime-to-zoneinfo)

   ;; Environment variable (most explicit if set)
   (getenv "TZ")

   ;; Read /etc/timezone (Debian/Ubuntu)
   (when (file-exists-p "/etc/timezone")
	 (with-temp-buffer
	   (insert-file-contents "/etc/timezone")
	   (string-trim (buffer-string))))

   ;; Method 4: Parse symlink (fallback for older systems)
   (when (file-symlink-p "/etc/localtime")
	 (let ((target (file-truename "/etc/localtime")))
	   (when (string-match ".*/zoneinfo/\\(.+\\)" target)
		 (match-string 1 target))))

   ;; Default to nil - lets org-gcal use its default
   nil))

(provide 'host-environment)
;;; host-environment.el ends here.
