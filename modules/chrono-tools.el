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
  :init
  (setq time-zones--city-list-file
        (expand-file-name "persist/time-zones-cities.el" user-emacs-directory))
  :bind ("M-S-c" . time-zones))  ;; was M-C, overrides capitalize-word

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

(defconst cj/tmr--audio-extensions
  '("mp3" "m4a" "ogg" "opus" "wav" "flac" "aac")
  "Audio file extensions offered to `cj/tmr-select-sound-file'.")

(defun cj/tmr--available-sound-files ()
  "Return a list of audio filenames in `sounds-dir', or nil if none.
Returns nil if `sounds-dir' does not exist."
  (when (and (boundp 'sounds-dir) (file-directory-p sounds-dir))
    (let ((regex (concat "\\." (regexp-opt cj/tmr--audio-extensions t) "$")))
      (directory-files sounds-dir nil regex))))

(defun cj/tmr-reset-sound-to-default ()
  "Reset the tmr sound file to the default notification sound."
  (interactive)
  (setq tmr-sound-file notification-sound)
  (message "Timer sound reset to default: %s"
		   (file-name-nondirectory notification-sound)))

(defun cj/tmr-select-sound-file ()
  "Select a sound file from `sounds-dir' to use for tmr timers.

Present all audio files in the sounds directory and set the chosen file as
`tmr-sound-file'. Use \\[universal-argument] to reset to the default sound."
  (interactive)
  (cond
   (current-prefix-arg
    (cj/tmr-reset-sound-to-default))
   ((not (and (boundp 'sounds-dir) (file-directory-p sounds-dir)))
    (message "Sounds directory does not exist: %s"
             (if (boundp 'sounds-dir) sounds-dir "<unset>")))
   (t
    (let ((sound-files (cj/tmr--available-sound-files)))
      (cond
       ((null sound-files)
        (message "No audio files found in %s" sounds-dir))
       (t
        (let* ((current-file (when (and tmr-sound-file
                                        (file-exists-p tmr-sound-file))
                               (file-name-nondirectory tmr-sound-file)))
               (selected-file
                (completing-read
                 (format "Select timer sound%s: "
                         (if current-file
                             (format " (current: %s)" current-file)
                           ""))
                 sound-files nil t nil nil current-file)))
          (cond
           ((or (null selected-file) (string-empty-p selected-file))
            (message "No file selected"))
           (t
            (setq tmr-sound-file (expand-file-name selected-file sounds-dir))
            (if (equal tmr-sound-file notification-sound)
                (message "Timer sound set to default: %s" selected-file)
              (message "Timer sound set to: %s" selected-file)))))))))))

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
