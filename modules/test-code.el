;;; test-code.el --- test code -*- lexical-binding: t; -*-

;;; Commentary:
;; This is where you should put config code you want to test.
;; I recommend calling this file from the end of your init.el
;; if something breaks, you have most of your Emacs config loaded.

;;; Code:


;; --------------------------------- Recording ---------------------------------

(defvar cj/ffmpeg-process nil
  "Variable to store the process of the ffmpeg recording.")

(defvar cj/recording-location "~/videos/recordings"
  "The location to save the ffmpeg recordings.")

(defun cj/start-recording (arg)
  "Starts the ffmpeg recording.
If called with a prefix arg C-u, choose the location on where to save the recording,
otherwise use the default location in `cj/recording-location'."
  (interactive "P")
  (let* ((location (if arg
                       (read-directory-name "Enter recording location: ")
                     cj/recording-location))
         (directory (file-name-directory location)))
    (unless (file-directory-p directory)
      (make-directory directory t))
    (cj/ffmpeg-record location)))

(defun cj/ffmpeg-record (directory)
  "Start an ffmpeg recording. Save output to DIRECTORY."
  (unless cj/ffmpeg-process
	(let* ((location (expand-file-name directory))
		   (name (format-time-string "%Y-%m-%d-%H-%M-%S"))
		   (filename (concat location "/" name ".mkv"))
		   (ffmpeg-command
			(concat "ffmpeg -framerate 30 -f x11grab -i :0.0+ "
					"-f pulse -i alsa_input.pci-0000_00_1b.0.analog-stereo "
					"-ac 1 -f pulse -i alsa_output.pci-0000_00_1b.0.analog-stereo.monitor "
                    "-ac 2 " filename)))
      ;; start the recording
	  (setq cj/ffmpeg-process
			(start-process-shell-command "ffmpeg-recording"
										 "*ffmpeg-recording*"
										 ffmpeg-command))
	  (set-process-query-on-exit-flag cj/ffmpeg-process nil)
	  (message "Started recording process."))))

(defun cj/stop-recording ()
  "Stop the ffmpeg recording process."
  (interactive)
  (when cj/ffmpeg-process
	(delete-process cj/ffmpeg-process)
	(setq cj/ffmpeg-process nil)
	(message "Stopped recording process.")))

;; ------------------------ Insert Around Word Or Region -----------------------

(defun cj/insert-around-word-or-region ()
  "Prompt for a string, insert it before and after the word at point or selected region."
  (interactive)
  (let ((str (read-string "Enter a string: "))
        (regionp (use-region-p)))
    (save-excursion
      (if regionp
          (let ((beg (region-beginning))
                (end (region-end)))
            (goto-char end)
            (insert str)
            (goto-char beg)
            (insert str))
        (if (thing-at-point 'word)
            (let ((bounds (bounds-of-thing-at-point 'word)))
              (goto-char (cdr bounds))
              (insert str)
              (goto-char (car bounds))
              (insert str))
          (message "Can't insert around. No word at point and no region selected."))))))

(global-set-key (kbd "C-; i a") 'cj/insert-around-word-or-region)

;; --------------------------------- Easy Hugo ---------------------------------

(use-package easy-hugo
  :defer .5
  :init
  (setq easy-hugo-basedir "~/code/cjennings.net/")
  (setq easy-hugo-url "https://cjennings.net")
  (setq easy-hugo-sshdomain "cjennings.net")
  (setq easy-hugo-root "/var/www/cjennings/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-postdir "content")
  (setq easy-hugo-server-flags "-D")
  (setq easy-hugo-default-ext ".md")
  :bind ("C-c H" . easy-hugo)
  :config
  (easy-hugo-enable-menu))

;; -------------------------------- Google This --------------------------------

;; BUG: Fix warnings and errors thrown
(use-package google-this
  :load-path "~/code/emacs-google-this/"
  :defer 1
  :bind
  ("C-h g" . 'google-this-search)
  :config
  (google-this-mode 1)
  (setq google-this-browse-url-function 'eww-browse-url))

;; ----------------------------------- Wttrin ----------------------------------
;; show the weather forecast in an Emacs buffer

(use-package wttrin
  :defer .5
  :after xterm-color
  :load-path ("~/code/emacs-wttrin")
  :bind
  ("C-c w" . wttrin)
  :custom
  (wttrin-unit-system "u")
  (wttrin-default-cities '(
						   "Albuquerque, New Mexico"
						   "Berkeley, CA"
						   "Boston, Massachussetts"
						   "Chicago, Illinois"
						   "Huntington Beach, CA"
						   "Littlestown, PA"
						   "London, UK"
						   "New Orleans, LA"
						   "New York, New York"
						   "Oakland, California"
						   "Paris, FR"
						   "San Francisco, California"
						   "Santa Fe, New Mexico"
						   "Yerevan, AM"
						   )))

;; dependency for wttrin
(use-package xterm-color
  :defer .5)

;; ------------------------------ ERC Yank To Gist -----------------------------
;; automatically create a Gist if pasting more than 5 lines
;; this module requires https://github.com/defunkt/gist
;; via ruby: 'gem install gist'	via the aur: yay -S gist

(use-package erc-yank
  :defer 1
  :after erc
  :bind
  (:map erc-mode-map
		("C-y" . erc-yank)))

;; --------------------------------- Ob-Racket ---------------------------------

;; (use-package ob-racket
;;   :load-path "~/code/ob-racket"
;;   :defer .5
;;   :after racket-mode
;;   :commands (org-babel-execute:racket)
;;   :quelpa (ob-racket
;; 		 :fetcher github
;; 		 :repo "hasu/emacs-ob-racket"
;; 		 :files ("*.el" "*.rkt")))

(provide 'test-code)
;;; test-code.el ends here.
