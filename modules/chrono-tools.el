;;; chrono-tools.el --- Config for Date and Time-Related Utils -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; This module centralizes configuration for Emacs time-related tools:
;;
;; – world-clock: predefined city list and custom time format
;; – calendar: quick navigation keybindings by day, month, and year
;; – tmr: lightweight timer setup with sounds, notifications, and history
;;
;;; Code:

(use-package time
  :ensure nil ;; built-in
  :defer 0.5
  :bind ("C-x c" . world-clock)
  :config
  (setq world-clock-list
		'(("Pacific/Honolulu"    " Honolulu")
		  ("America/Los_Angeles" " San Francisco, LA")
		  ("America/Chicago"     " Chicago, New Orleans")
		  ("America/New_York"    " New York, Boston")
		  ("Etc/UTC"             " UTC =================")
		  ("Europe/London"       " London, Lisbon")
		  ("Europe/Paris"        " Paris, Berlin, Rome")
		  ("Europe/Athens"       " Athens, Istanbul, Moscow")
		  ("Asia/Kolkata"        " India")
		  ("Asia/Shanghai"       " Shanghai, Singapore")
		  ("Asia/Tokyo"          " Tokyo, Seoul")))
  (setq world-clock-time-format " %a, %d %b @ %I:%M %p %Z"))

(use-package calendar
  :ensure nil ;; built-in
  :defer 0.5
  :bind (("M-#" . calendar)
		 :map calendar-mode-map
		 (","   . calendar-backward-day)
		 ("."   . calendar-forward-day)
		 ("<"   . calendar-backward-month)
		 (">"   . calendar-forward-month)
		 ("M-," . calendar-backward-year)
		 ("M-." . calendar-forward-year)))

(use-package tmr
  :defer 0.5
  :bind ("M-t" . tmr-prefix-map)
  :config
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
		tmr-notification-urgency 'normal
		tmr-descriptions-list 'tmr-description-history))


(provide 'chrono-tools)
;;; chrono-tools.el ends here.

;; --------------------------------- ERT Tests ---------------------------------
;; Run these tests with M-x ert RET t RET

(ert-deftest chrono-tools/tmr-sound-file-exists ()
  "Test that `tmr-sound-file` points to an existing file."
  (require 'tmr)
  (should (file-exists-p tmr-sound-file)))
