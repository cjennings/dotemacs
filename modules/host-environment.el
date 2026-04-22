;;; host-environment.el --- Host Environment Convenience Functions -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Convenience functions to report about the host environment
;;
;;; Code:

;; for the byte-compiler/linter
(declare-function battery-format "battery" (format data))
(defvar battery-status-function)



(defun env--battery-status-char-indicates-battery-p (status)
  "Return non-nil if STATUS indicates a real battery is present.
STATUS is the value returned by `battery-format' with the \"%B\" spec.
A real battery reports \"!\" (critical), \"+\" (charging), or \"-\"
(discharging).  Desktops report \"N/A\", \"unknown\", or empty, which
all mean no battery."
  (and (stringp status)
	   (member status '("!" "+" "-"))
	   t))

(defun env--power-supply-has-battery-p (power-supply-dir)
  "Return non-nil if POWER-SUPPLY-DIR contains a BAT* entry.
Canonical Linux check.  A laptop exposes directories like
/sys/class/power_supply/BAT0; a desktop exposes only AC adapters
and USB-C power entries."
  (and (file-directory-p power-supply-dir)
	   (file-expand-wildcards (expand-file-name "BAT*" power-supply-dir))
	   t))

(defun env-laptop-p ()
  "Non-nil if the host has a battery.
On Linux, checks /sys/class/power_supply for BAT* entries, which is the
canonical signal.  On other platforms, falls back to `battery-format'
\"%B\" and checks for a live battery status char."
  (cond
   ((eq system-type 'gnu/linux)
	(env--power-supply-has-battery-p "/sys/class/power_supply"))
   ((and (require 'battery nil 'noerror)
		 battery-status-function)
	(env--battery-status-char-indicates-battery-p
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

(defun env-x11-p ()
  "Return t if running under X11 (not Wayland)."
  (and (eq (window-system) 'x)
       (not (getenv "WAYLAND_DISPLAY"))))

(defun env-wayland-p ()
  "Return t if running under Wayland.
Checks WAYLAND_DISPLAY env var, which is set by Wayland compositors.
This returns t even if Emacs is running through XWayland."
  (and (getenv "WAYLAND_DISPLAY") t))

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

   ;; Default to nil if detection fails
   nil))

(provide 'host-environment)
;;; host-environment.el ends here.
