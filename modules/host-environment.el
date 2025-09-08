;;; host-environment.el --- Host Environment Convenience Functions -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Convenience functions to report about the host environment

;;; Code:

(require 'battery)

(defun env-laptop-p ()
  "Return t if host is a laptop (i.e., has a battery), nil if not."
  (when (and battery-status-function
			 (not (string-match-p "N/A"
								  (battery-format "%B"
												  (funcall battery-status-function)))))
	t))

(defun env-desktop-p ()
  "Return t if host is a laptop (has a battery), nil if not."
  (when (not (env-laptop-p))
	t))

(defun env-linux-p ()
  "Return t if host system is GNU/Linux."
  (string-equal system-type "gnu/linux"))

(defun env-bsd-p ()
  "Return t if host system is FreeBSD."
  (string-equal system-type "berkeley-unix"))

(defun env-macos-p ()
  "Return t if host system is Mac OS (darwin-based)."
  (string-equal system-type "darwin"))

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

(provide 'host-environment)
;;; host-environment.el ends here.
