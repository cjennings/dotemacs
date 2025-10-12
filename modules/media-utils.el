;;; media-utils.el --- Utilities for Downloading and Viewing Media  -*- coding: utf-8; lexical-binding: t; -*-
;; TASK: Update commentary to include default media selection
;;
;;; Commentary:
;;
;; This library provides reusable Emacs methods for working with online and
;; local media, to support media download playback from Emacs.
;;
;; Main features:
;;
;; - Asynchronously download videos (e.g., from YouTube and similar sites) via
;;   yt-dlp, with queueing and background management handled by the
;;   task-spooler (tsp) utility.
;;
;; - Asynchronously play media URLs using a user-defined choice of external
;;   media players (mpv, VLC, etc.), with automatic stream resolution via
;;   yt-dlp when required, and dynamic configuration of playback options.
;;
;;; Code:

;; ------------------------ Default Media Configurations -----------------------
;; Common yt-dlp format codes:
;; 18  - 360p MP4 (good for low bandwidth)
;; 22  - 720p MP4 (good balance of quality and size)
;; best - best single file format
;; best[height<=720] - best format up to 720p
;; best[height<=1080] - best format up to 1080p
;; bestvideo+bestaudio - best video and audio (may require ffmpeg)
;; For more formats, run: yt-dlp -F <youtube-url>

(defcustom cj/media-players
  '((mpv . (:command "mpv"
					 :args nil
					 :name "MPV"
					 :needs-stream-url nil
					 :yt-dlp-formats nil))
	(vlc . (:command "vlc"
					 :args nil
					 :name "VLC"
					 :needs-stream-url t
					 :yt-dlp-formats ("22" "18" "best")))  ; Try formats in order
	(cvlc . (:command "cvlc"
					  :args "-vvv"
					  :name "cvlc"
					  :needs-stream-url t
					  :yt-dlp-formats ("22" "18" "best")))
	(mplayer . (:command "mplayer"
						 :args nil
						 :name "MPlayer"
						 :needs-stream-url t
						 :yt-dlp-formats ("18" "22" "best")))
	(iina . (:command "iina"
					  :args nil
					  :name "IINA"
					  :needs-stream-url t
					  :yt-dlp-formats ("best[height<=1080]" "22" "best"))))
  "Define media players and their configurations for yt-dlp and playing.
Each entry is (SYMBOL . PLIST). The PLIST accepts the keys :command for the
executable name, :args for optional arguments, :name for a human-readable
label, :needs-stream-url for a boolean flag indicating whether to extract a
stream URL with `yt-dlp', and :yt-dlp-formats for a prioritized list of format
strings."
  :type '(alist :key-type symbol
				:value-type (plist :key-type keyword
								   :value-type sexp))
  :group 'elfeed)

(defcustom cj/default-media-player 'mpv
  "The default media player to use for videos.

Should be a key from `cj/media-players'."
  :type 'symbol
  :group 'elfeed)

(defun cj/get-available-media-players ()
  "Return a list of available media players from `cj/media-players'."
  (cl-loop for (player . config) in cj/media-players
		   when (executable-find (plist-get config :command))
		   collect player))

(defun cj/select-media-player ()
  "Interactively select a media player from available options."
  (interactive)
  (let* ((available (cj/get-available-media-players))
		 (choices (mapcar (lambda (player)
							(let ((config (alist-get player cj/media-players)))
							  (cons (plist-get config :name) player)))
						  available))
		 (selection (completing-read
					 (format "Select media player (current: %s): "
							 (plist-get (alist-get cj/default-media-player cj/media-players) :name))
					 choices nil t))
		 (player (alist-get selection choices nil nil #'string=)))
	(when player
	  (setq cj/default-media-player player)
	  (message "Media player set to: %s" selection))))

;; ---------------------- Playing Via Default Media Player ---------------------

(defun cj/media-play-it (url)
  "Play the URL with the configured media player in an async process."
  (let* ((player-config (alist-get cj/default-media-player cj/media-players))
		 (command (plist-get player-config :command))
		 (args (plist-get player-config :args))
		 (player-name (plist-get player-config :name))
		 (needs-stream-url (plist-get player-config :needs-stream-url))
		 (yt-dlp-formats (plist-get player-config :yt-dlp-formats))
		 (url-display (truncate-string-to-width url 50)))

	(unless (executable-find command)
	  (error "%s is not installed or not in PATH" player-name))

	(let* ((buffer-name (format "*%s: %s*" player-name url-display))
		   (shell-command
			(if needs-stream-url
				;; Use shell substitution with yt-dlp
				(let ((format-string (if yt-dlp-formats
										 (format "-f %s"
												 (mapconcat #'shell-quote-argument
															yt-dlp-formats
															"/"))
									   "")))
				  (format "%s %s $(%s %s -g %s)"
						  command
						  (or args "")
						  "yt-dlp"
						  format-string
						  (shell-quote-argument url)))
			  ;; Direct playback without yt-dlp
			  (format "%s %s %s"
					  command
					  (or args "")
					  (shell-quote-argument url)))))

	  (message "Playing with %s: %s" player-name url-display)
	  (cj/log-silently "DEBUG: Executing: %s" shell-command)

	  (let ((process (start-process-shell-command
					  player-name
					  buffer-name
					  shell-command)))
		(set-process-sentinel
		 process
		 (lambda (proc event)
		   (cond
			((string-match-p "finished" event)
			 (message "✓ Finished playing: %s" url-display))
			((string-match-p "exited abnormally" event)
			 (message "✗ Playback failed: %s" url-display)
			 (with-current-buffer (process-buffer proc)
			   (goto-char (point-min))
			   (when (re-search-forward "ERROR:" nil t)
				 (cj/log-silently "DEBUG: yt-dlp error: %s"
								  (buffer-substring-no-properties
								   (line-beginning-position)
								   (line-end-position)))))))
		   (when (string-match-p "finished\\|exited" event)
			 (kill-buffer (process-buffer proc)))))))))

;; ------------------------- Media-Download Via yt-dlp -------------------------

(defun cj/yt-dl-it (url)
  "Downloads the URL in an async shell."
  (unless (executable-find "yt-dlp")
	(error "The program yt-dlp is not installed or not in PATH"))
  (unless (executable-find "tsp")
	(error "The tsp (task-spooler) program is not installed or not in PATH"))
  (let* ((default-directory videos-dir)
		 (buffer-name (format "*yt-dlp: %s*" (truncate-string-to-width url 50)))
		 (output-template (format "%s/%%(channel)s-%%(title)s.%%(ext)s" videos-dir))
		 (url-display (truncate-string-to-width url 50))
		 (process (start-process "yt-dlp" buffer-name
								 "tsp" "yt-dlp" "--add-metadata" "-ic"
								 "-o" output-template url)))
	(message "Started download: %s" url-display)
	(set-process-sentinel process
						  (lambda (proc event)
							(cond
							 ((string-match-p "finished" event)
							  (message "✓ Finished downloading: %s" url-display))
							 ((string-match-p "exited abnormally" event)
							  (message "✗ Download failed: %s" url-display)))
							(when (string-match-p "finished\\|exited" event)
							  (kill-buffer (process-buffer proc)))))))

(provide 'media-utils)
;;; media-utils.el ends here.
