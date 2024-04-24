;;; test-code.el --- test code -*- lexical-binding: t; -*-

;;; Commentary:
;; This is where you should put config code you want to test.
;; I recommend calling this file from the end of your init.el
;; if something breaks, you have most of your Emacs config loaded.

;;; Code:

;; -------------------------------- Geiser Guile -------------------------------
;; Guile support in Emacs

(use-package geiser-guile
  :defer 1
  :commands (geiser-guile)
  :bind ("C-c G" . geiser-guile)
  :config
  (setq geiser-guile-binary "/usr/bin/guile"))

;; ---------------------------------- Yeetube ----------------------------------
;; youtube frontend for emacs

(use-package yeetube
  :init (define-prefix-command 'cj/yeetube-map)
  :bind (("C-c y" . 'cj/yeetube-map)
		 :map cj/yeetube-map
		 ("s" . 'yeetube-search)
		 ("b" . 'yeetube-play-saved-video)
		 ("d" . 'yeetube-download-videos)
		 ("p" . 'yeetube-mpv-toggle-pause)
		 ("v" . 'yeetube-mpv-toggle-video)
		 ("V" . 'yeetube-mpv-toggle-no-video-flag)
		 ("k" . 'yeetube-remove-saved-video))
  :custom
  (yeetube-results-limit 50)
  (yeetube-download-directory (expand-file-name "videos" "~"))
  (yeetube-filter "Views")
  (setq yeetube-display-thumbnails nil)
  :config
  (setf yeetube-mpv-disable-video nil))

;; --------------------------------- Easy Hugo ---------------------------------

(use-package easy-hugo
  :defer .5
  :init
  (setq easy-hugo-basedir "~/code/cjennings-net/")
  (setq easy-hugo-url "https://cjennings.net")
  (setq easy-hugo-sshdomain "cjennings.net")
  (setq easy-hugo-root "/var/www/cjennings/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-postdir "content")
  (setq easy-hugo-server-flags "-D --noHTTPCache --disableFastRender")
  (setq easy-hugo-default-ext ".md")
  :bind ("C-c H" . easy-hugo)
  :config
  (easy-hugo-enable-menu))


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


;; -------------------------------- Google This --------------------------------

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
  :load-path ("~/code/emacs-wttrin")
  :ensure nil ;; local package
  :preface
  ;; dependency for wttrin
  (use-package xterm-color
	:demand t)
  :bind
  ("M-W" . wttrin))

;; ------------------------------ ERC Yank To Gist -----------------------------
;; automatically create a Gist if pasting more than 5 lines
;; this module requires https://github.com/defunkt/gist
;; via ruby: 'gem install gist' via the aur: yay -S gist

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
;;       :fetcher github
;;       :repo "hasu/emacs-ob-racket"
;;       :files ("*.el" "*.rkt")))

(provide 'test-code)
;;; test-code.el ends here.
