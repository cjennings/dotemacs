;;; elfeed-config --- Settings and Enhancements to the Elfeed RSS Feed Reader -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'user-constants)
(require 'system-utils)

;; ------------------------------ Media Player Config --------------------------

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
  "Define media players and their configurations for Elfeed integration.

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
  "Choose the default media player to use for Elfeed videos.

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

;; ------------------------------- Elfeed Config -------------------------------

(use-package elfeed
  :bind
  ("M-R" . cj/elfeed-open)
  (:map elfeed-show-mode-map
        ("w"  . eww-open-in-new-buffer))
  (:map elfeed-search-mode-map
        ("w"   . cj/elfeed-eww-open)            ;; opens in eww
        ("b"   . cj/elfeed-browser-open)        ;; opens in external browser
        ("d"   . cj/elfeed-youtube-dl)          ;; async download with yt-dlp and tsp
        ("v"   . cj/play-with-video-player))              ;; async play with mpv
  ("V"   . cj/select-media-player)       ;; Capital V to select player
  ("R"   . cj/elfeed-mark-all-as-read)    ;; capital marks all as read, since upper case marks one as read
  ("U"   . cj/elfeed-mark-all-as-unread) ;; capital marks all as unread, since lower case marks one as unread
  :config
  (setq elfeed-db-directory (concat user-emacs-directory ".elfeed-db"))
  (setq-default elfeed-search-title-max-width 150)
  (setq-default elfeed-search-title-min-width 80)
  (setq-default elfeed-search-filter "+unread")
  (setq elfeed-feeds
        '(
          ;; The Daily
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLdMrbgYfVl-s16D_iT2BJCJ90pWtTO1A4" yt nytdaily)

          ;; The Ezra Klein Show
		  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCnxuOd8obvLLtf5_-YKFbiQ" yt ezra)

          ;; Pivot with Kara Swisher and Scott Galloway
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBHGZpDF2fsqPIPi0pNyuTg" yt pivot)

          ;; The Prof G Pod
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLtQ-jBytlXCasRuBG86m22rOQfrEPcctq" yt profg)

          ;; On with Kara Swisher
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMDxbhGcsE7EnknxPEzC_Iw" yt)

          ;; Raging Moderates
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCcvDWzvxz6Kn1iPQHMl2teA" yt raging-moderates)

          ;; Prof G Markets
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLtQ-jBytlXCY28ucRF8P1mNMSG8uC06Aw" yt profg-markets)

		  ;; Trae Crowder Porch Rants
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL45Mc1cDgnsB-u1iLPBYNF1fk-y1cVzTJ" yt trae)

		  ;; Senator Bernie Sanders
		  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCD_DaKNac0Ta-2PeHuoQ1uA" yt bernie)

		  ;; If You're Listening | ABC News In-depth
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLDTPrMoGHssAfgMMS3L5LpLNFMNp1U_Nq" yt listening)
          )))

;; ------------------------------ Elfeed Functions -----------------------------

(defun cj/elfeed-open ()
  "Open Elfeed, update all feeds, then move to the first entry."
  (interactive)
  (elfeed)
  (elfeed-update)
  (elfeed-search-update--force))

;; -------------------------- Elfeed Filter Functions --------------------------

(defun cj/elfeed-mark-all-as-read ()
  "Remove the \='unread\=' tag from all visible entries in search buffer."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun cj/elfeed-mark-all-as-unread ()
  "Add the \='unread\=' tag from all visible entries in the search buffer."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-tag-all 'unread))

(defun cj/elfeed-set-filter-and-update (filterstring)
  "Set the Elfeed filter to FILTERSTRING and update the buffer."
  (interactive "sFilter: ")
  (setq elfeed-search-filter filterstring)
  (elfeed-search-update--force)
  (goto-char (point-min)))

;; ----------------------------- Extract Stream Url ----------------------------


(defun cj/extract-stream-url (url format)
  "Extract the direct stream URL from URL using yt-dlp with FORMAT.

Returns the stream URL or nil on failure."
  (unless (executable-find "yt-dlp")
	(error "The program yt-dlp is not installed or not in PATH"))
  (let* ((format-args (if format
						  (list "-f" format)
						nil))
         (cmd-args (append '("yt-dlp" "-q" "-g")
						   format-args
						   (list url)))
		 ;; DEBUG: Log the command
		 (_ (cj/log-silently "DEBUG: Extracting with command: %s"
							 (mapconcat #'shell-quote-argument cmd-args " ")))
		 (output (with-temp-buffer
				   (let ((exit-code (apply #'call-process
										   (car cmd-args) nil t nil
										   (cdr cmd-args))))
					 (if (zerop exit-code)
						 (string-trim (buffer-string))
					   (progn
						 ;; DEBUG: Log failure
						 (cj/log-silently "DEBUG: yt-dlp failed with exit code %d" exit-code)
						 (cj/log-silently "DEBUG: Error output: %s" (buffer-string))
						 nil))))))
    ;; DEBUG: Log the result
	(cj/log-silently "DEBUG: Extracted URL: %s"
					 (if output (truncate-string-to-width output 100) "nil"))
    (when (and output (string-match-p "^https?://" output))
      output)))


;; -------------------------- Elfeed Core Processing ---------------------------

(defun cj/elfeed-process-entries (action-fn action-name &optional skip-error-handling)
  "Process selected Elfeed entries with ACTION-FN.

ACTION-NAME is used for error messages. Marks entries as read and
advances to the next line. If SKIP-ERROR-HANDLING is non-nil, errors
are not caught (useful for actions that handle their own errors)."
  (let ((entries (elfeed-search-selected)))
    (unless entries
      (error "No entries selected"))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             for link = (elfeed-entry-link entry)
             when link
             do (if skip-error-handling
                    (funcall action-fn link)
                  (condition-case err
                      (funcall action-fn link)
                    (error (message "Failed to %s %s: %s"
                                    action-name
                                    (truncate-string-to-width link 50)
                                    (error-message-string err))))))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; -------------------------- Elfeed Browser Functions -------------------------

(defun cj/elfeed-eww-open ()
  "Opens the links of the currently selected Elfeed entries with EWW.

Applies cj/eww-readable-nonce hook after EWW rendering."
  (interactive)
  (cj/elfeed-process-entries
   (lambda (link)
     (add-hook 'eww-after-render-hook #'cj/eww-readable-nonce)
     (eww-browse-url link))
   "open in EWW"
   t))  ; skip error handling since eww handles its own errors

(defun cj/eww-readable-nonce ()
  "Once-off call to eww-readable after EWW is done rendering."
  (unwind-protect
      (progn
        (eww-readable)
        (goto-char (point-min)))
    (remove-hook 'eww-after-render-hook #'cj/eww-readable-nonce)))

(defun cj/elfeed-browser-open ()
  "Opens the link of the selected Elfeed entries in the default browser."
  (interactive)
  (cj/elfeed-process-entries
   #'browse-url-default-browser
   "open in browser"
   t))  ; skip error handling since browser handles its own errors

;; --------------------- Elfeed Play And Download Functions --------------------

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

(defun cj/elfeed-youtube-dl ()
  "Downloads the selected Elfeed entries' links with youtube-dl."
  (interactive)
  (cj/elfeed-process-entries #'cj/yt-dl-it "download"))

(defun cj/play-with-video-player ()
  "Plays the selected Elfeed entries' links with the configured media player.

Note: Function name kept for backwards compatibility."
  (interactive)
  (cj/elfeed-process-entries #'cj/media-play-it
                             (format "play with %s"
                                     (plist-get (alist-get cj/default-media-player cj/media-players) :name))))

;; --------------------- Youtube Url To Elfeed Feed Format ---------------------

(defun cj/youtube-to-elfeed-feed-format (url type)
  "Convert YouTube URL to elfeed-feeds format.

TYPE should be either \='channel or \='playlist."
  (let ((id nil)
        (title nil)
        (buffer nil)
        (id-pattern (if (eq type 'channel)
                        "href=\"https://www\\.youtube\\.com/feeds/videos\\.xml\\?channel_id=\\([^\"]+\\)\""
                      "/playlist\\?list=\\([^&]+\\)"))
        (feed-format (if (eq type 'channel)
                         "https://www.youtube.com/feeds/videos.xml?channel_id=%s"
                       "https://www.youtube.com/feeds/videos.xml?playlist_id=%s"))
        (error-msg (if (eq type 'channel)
                       "Could not extract channel information"
                     "Could not extract playlist information")))

    ;; Extract ID based on type
    (if (eq type 'channel)
        ;; For channels, we need to fetch the page to get the channel_id
        (progn
          (setq buffer (url-retrieve-synchronously url))
          (when buffer
            (with-current-buffer buffer
              ;; Decode the content as UTF-8
              (set-buffer-multibyte t)
              (decode-coding-region (point-min) (point-max) 'utf-8)
              (goto-char (point-min))
              ;; Search for the channel_id in the RSS feed link
              (when (re-search-forward id-pattern nil t)
                (setq id (match-string 1))))))
      ;; For playlists, extract from URL first
      (when (string-match id-pattern url)
        (setq id (match-string 1 url))
        (setq buffer (url-retrieve-synchronously url))))

    ;; Get title from the page
    (when (and buffer id)
      (with-current-buffer buffer
        (unless (eq type 'channel)
          ;; Decode for playlist (already done for channel above)
          (set-buffer-multibyte t)
          (decode-coding-region (point-min) (point-max) 'utf-8))
        ;; Search for the title in og:title meta tag
        (goto-char (point-min))
        (when (re-search-forward "<meta property=\"og:title\" content=\"\\([^\"]+\\)\"" nil t)
          (setq title (match-string 1))
          ;; Simple HTML entity decoding
          (setq title (replace-regexp-in-string "&amp;" "&" title))
          (setq title (replace-regexp-in-string "&lt;" "<" title))
          (setq title (replace-regexp-in-string "&gt;" ">" title))
          (setq title (replace-regexp-in-string "&quot;" "\"" title))
          (setq title (replace-regexp-in-string "&#39;" "'" title))
          (setq title (replace-regexp-in-string "&#x27;" "'" title))))
      (kill-buffer buffer))

    (if (and id title)
        (format ";; %s\n(\"%s\" yt)"
                title
                (format feed-format id))
      (error error-msg))))

(defun cj/youtube-channel-to-elfeed-feed-format (url)
  "Convert YouTube channel URL to elfeed-feeds format and insert at point."
  (interactive "sYouTube Channel URL: ")
  (let ((result (cj/youtube-to-elfeed-feed-format url 'channel)))
    (when (called-interactively-p 'interactive)
      (insert result))
    result))

(defun cj/youtube-playlist-to-elfeed-feed-format (url)
  "Convert YouTube playlist URL to elfeed-feeds format and insert at point."
  (interactive "sYouTube Playlist URL: ")
  (let ((result (cj/youtube-to-elfeed-feed-format url 'playlist)))
    (when (called-interactively-p 'interactive)
      (insert result))
    result))

(provide 'elfeed-config)
;;; elfeed-config.el ends here.
