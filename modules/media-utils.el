;;; media-utils.el --- Utilities for Downloading and Viewing Media  -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/L.
;; Load shape: eager.
;; Eager reason: command library with no side effects; eager only by init order.
;;   Downloads and players should run by command, so a Phase 4 deferral candidate.
;; Top-level side effects: none.
;; Runtime requires: system-lib.
;; Direct test load: yes (pure command helpers).
;;
;; This library provides reusable Emacs methods for working with online and
;; local media, to support media download and playback from Emacs.
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
;; - Default media player selection via `cj/default-media-player', allowing
;;   users to set their preferred player (mpv, VLC, IINA, MPlayer, etc.) which
;;   will be used automatically when playing media.
;;
;; - Customizable media player configurations in `cj/media-players', with
;;   support for different yt-dlp format preferences, command-line arguments,
;;   and stream URL handling for each player.
;;
;;; Code:

(require 'system-lib)

;; Declare variables from other modules
(defvar videos-dir) ;; from user-constants.el

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
  :group 'media)

(defcustom cj/default-media-player 'mpv
  "The default media player to use for videos.
Should be a key from `cj/media-players'.  mpv is the default because it
resolves streaming-site URLs itself via yt-dlp, so it needs no pre-extracted
stream URL (see the :needs-stream-url flag in `cj/media-players')."
  :type 'symbol
  :group 'media)

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

(defun cj/media--yt-dlp-argv (url formats)
  "The argv to resolve URL's stream address: yt-dlp [-f FORMATS] -g URL.
FORMATS is a prioritized list of yt-dlp format codes, or nil for the
default.  URL stays one verbatim argv element, so it never meets a shell."
  (append (list "yt-dlp")
		  (when formats (list "-f" (string-join formats "/")))
		  (list "-g" url)))

(defun cj/media--stream-urls (output)
  "The non-empty lines of yt-dlp -g OUTPUT, surrounding whitespace trimmed."
  (split-string output "\n" t "[ \t\r]+"))

(defun cj/media--play-argv (command args urls)
  "The argv to play URLS with COMMAND.
ARGS is the player's raw option string from `cj/media-players' (nil for
none); it splits with `split-string-and-unquote' so a quoted option
survives as one word."
  (append (list command)
		  (and args (split-string-and-unquote args))
		  urls))

(defun cj/media--resolve-stream-urls (url formats)
  "Resolve URL to direct stream URLs with a synchronous yt-dlp -g capture.
FORMATS is the player's format-preference list.  Only stdout is parsed
for URLs -- yt-dlp's warnings go to stderr, captured separately for the
error message.  Signals an error when yt-dlp exits non-zero or resolves
nothing."
  (let ((err-file (make-temp-file "yt-dlp-stderr")))
	(unwind-protect
		(with-temp-buffer
		  (let* ((argv (cj/media--yt-dlp-argv url formats))
				 (exit (apply #'call-process (car argv) nil
							  (list t err-file) nil (cdr argv))))
			(unless (and (integerp exit) (zerop exit))
			  (error "yt-dlp failed (exit %s): %s" exit
					 (string-trim
					  (with-temp-buffer
						(insert-file-contents err-file)
						(buffer-string)))))
			(or (cj/media--stream-urls (buffer-string))
				(error "yt-dlp resolved no stream URL for %s" url))))
	  (delete-file err-file))))

(defun cj/media-play-it (url)
  "Play the URL with the configured media player in an async process.
A player flagged :needs-stream-url gets the URL resolved first via a
synchronous yt-dlp -g capture (blocks briefly); the player then launches
with a plain argv list -- no shell anywhere in the pipeline."
  (let* ((player-config (alist-get cj/default-media-player cj/media-players))
		 (command (plist-get player-config :command))
		 (args (plist-get player-config :args))
		 (player-name (plist-get player-config :name))
		 (needs-stream-url (plist-get player-config :needs-stream-url))
		 (yt-dlp-formats (plist-get player-config :yt-dlp-formats))
		 (url-display (truncate-string-to-width url 50)))

	(unless (executable-find command)
	  (error "%s is not installed or not in PATH" player-name))
	(when needs-stream-url
	  (unless (executable-find "yt-dlp")
		(error "The program yt-dlp is not installed or not in PATH")))

	(let* ((urls (if needs-stream-url
					 (progn
					   (message "Resolving stream URL: %s" url-display)
					   (cj/media--resolve-stream-urls url yt-dlp-formats))
				   (list url)))
		   (argv (cj/media--play-argv command args urls))
		   (buffer-name (format "*%s: %s*" player-name url-display)))

	  (message "Playing with %s: %s" player-name url-display)
	  (cj/log-silently "DEBUG: Executing: %s" (string-join argv " "))

	  (let ((process (apply #'start-process player-name buffer-name argv)))
		(set-process-sentinel
		 process
		 (lambda (proc event)
		   (cond
			((string-match-p "finished" event)
			 (message "✓ Finished playing: %s" url-display))
			((string-match-p "exited abnormally" event)
			 (message "✗ Playback failed: %s" url-display)))
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
