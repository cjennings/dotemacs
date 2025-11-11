;;; chrono-tools.el --- Config for Date and Time-Related Utils -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; This module centralizes configuration for Emacs time-related tools:
;;
;; – time-zones: interactive world clock with fuzzy search and time shifting
;; – calendar: quick navigation keybindings by day, month, and year
;; – tmr: lightweight timer setup with sounds, notifications, and history
;;
;;; Code:

(require 'user-constants)

;; -------------------------------- Time Zones ---------------------------------

(use-package time-zones
  :defer
  :commands time-zones
  :bind ("C-x c" . time-zones))

;; Commented out old world-clock config while testing time-zone package above
;; (use-package time
;;   :ensure nil ;; built-in
;;   :defer 0.5
;;   :bind ("C-x c" . world-clock)
;;   :config
;;   (setq world-clock-list
;; 		'(("Pacific/Honolulu"    " Honolulu")
;; 		  ("America/Los_Angeles" " San Francisco, LA")
;; 		  ("America/Chicago"     " Chicago, New Orleans")
;; 		  ("America/New_York"    " New York, Boston")
;; 		  ("Etc/UTC"             " UTC =================")
;; 		  ("Europe/London"       " London, Lisbon")
;; 		  ("Europe/Paris"        " Paris, Berlin, Rome")
;; 		  ("Europe/Athens"       " Athens, Istanbul, Moscow")
;; 		  ("Asia/Kolkata"        " India")
;; 		  ("Asia/Shanghai"       " Shanghai, Singapore")
;; 		  ("Asia/Tokyo"          " Tokyo, Seoul")))
;;   (setq world-clock-time-format " %a, %d %b @ %I:%M %p %Z"))

(use-package calendar
  :ensure nil ;; built-in
  :defer 0.5
  :bind (("M-#" . calendar)
		 :map calendar-mode-map
		 ("."   . calendar-goto-today)
		 ("<left>"   . calendar-backward-day)
		 ("<right>"   . calendar-forward-day)
		 ("C-<left>"   . calendar-backward-month)
		 ("C-<right>"   . calendar-forward-month)
		 ("M-<left>" . calendar-backward-year)
		 ("M-<right>" . calendar-forward-year)))


;; ------------------------------------ TMR ------------------------------------

(defun cj/tmr-select-sound-file ()
  "Select a sound file from `sounds-dir' to use for tmr timers.

Present all audio files in the sounds directory and set the chosen file as
`tmr-sound-file'. Use \\[universal-argument] to reset to the default sound."
  (interactive)
  (if current-prefix-arg
	  ;; With prefix arg, reset to default
	  (progn
		(setq tmr-sound-file notification-sound)
		(message "Timer sound reset to default: %s"
				 (file-name-nondirectory notification-sound)))
	;; Otherwise, select a new sound
	(let* ((audio-extensions '("mp3" "m4a" "ogg" "opus" "wav" "flac" "aac"))
		   (extension-regex (concat "\\." (regexp-opt audio-extensions t) "$"))
		   (sound-files (when (file-directory-p sounds-dir)
						  (directory-files sounds-dir nil extension-regex)))
		   (current-file (when (and tmr-sound-file (file-exists-p tmr-sound-file))
						   (file-name-nondirectory tmr-sound-file)))
		   (selected-file (when sound-files
							(completing-read
							 (format "Select timer sound%s: "
									 (if current-file
										 (format " (current: %s)" current-file)
									   ""))
							 sound-files
							 nil
							 t
							 nil
							 nil
							 current-file))))  ; Default to current file
	  (cond
	   ((not (file-directory-p sounds-dir))
		(message "Sounds directory does not exist: %s" sounds-dir))
	   ((null sound-files)
		(message "No audio files found in %s" sounds-dir))
	   (selected-file
		(setq tmr-sound-file (expand-file-name selected-file sounds-dir))
		(when (equal tmr-sound-file notification-sound)
		  (message "Timer sound set to default: %s" selected-file))
		(unless (equal tmr-sound-file notification-sound)
		  (message "Timer sound set to: %s" selected-file)))
	   (t
		(message "No file selected"))))))

(defun cj/tmr-reset-sound-to-default ()
  "Reset the tmr sound file to the default notification sound."
  (interactive)
  (setq tmr-sound-file notification-sound)
  (message "Timer sound reset to default: %s"
		   (file-name-nondirectory notification-package)))

(use-package tmr
  :defer 0.5
  :init
  (global-unset-key (kbd "M-t"))
  :bind (("M-t" . tmr-prefix-map)
		 :map tmr-prefix-map
		 ("*" . tmr)
		 ("t" . tmr-with-details)
		 ("S" . cj/tmr-select-sound-file)
		 ("R" . cj/tmr-reset-sound-to-default))
  :config
  (setq tmr-sound-file notification-sound)
  (setq tmr-notification-urgency 'normal)
  (setq tmr-descriptions-list 'tmr-description-history))

(provide 'chrono-tools)
;;; chrono-tools.el ends here
