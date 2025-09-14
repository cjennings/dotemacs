;;; elfeed-config --- Settings and Enhancements to the Elfeed RSS Feed Reader -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'user-constants)

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
		("p"   . cj/play-with-mpv)              ;; async play with mpv
		("v"   . cj/play-with-mpv)              ;; async play with mpv
		("R"   . cj/elfeed-mark-all-as-read)    ;; capital marks all as read, since upper case marks one as read
		("U"   . cj/elfeed-mark-all-as-unread)) ;; capital marks all as unread, since lower case marks one as unread
  :config
  (setq elfeed-db-directory (concat user-emacs-directory ".elfeed-db"))
  (setq-default elfeed-search-title-max-width 150)
  (setq-default elfeed-search-title-min-width 80)
  (setq-default elfeed-search-filter "+unread")
  (setq elfeed-feeds
		'(
		  ;; The Prof G Pod – Scott Galloway
		  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1E1SVcVyU3ntWMSQEp38Yw" youtube prof-g)

		  ;; The Daily
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLdMrbgYfVl-s16D_iT2BJCJ90pWtTO1A4" youtube the-daily)

		  ;; The Ezra Klein Show
		  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCnxuOd8obvLLtf5_-YKFbiQ" youtube ezra-klein)

		  ;; Pivot with Kara Swisher and Scott Galloway
		  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBHGZpDF2fsqPIPi0pNyuTg" youtube pivot)
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
  "Remove the 'unread' tag from all elfeed entries visible in the elfeed search buffer."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun cj/elfeed-mark-all-as-unread ()
  "Add the 'unread' tag from all elfeed entries visible in the elfeed search buffer."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-tag-all 'unread))

(defun cj/elfeed-set-filter-and-update (filterstring)
  "Set the Elfeed filter to FILTERSTRING and update the buffer."
  (interactive "sFilter: ")
  (setq elfeed-search-filter filterstring)
  (elfeed-search-update--force)
  (goto-char (point-min)))

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
	(error "yt-dlp is not installed or not in PATH"))
  (unless (executable-find "tsp")
	(error "tsp (task-spooler) is not installed or not in PATH"))
  (let* ((default-directory videos-dir)
		 (buffer-name (format "*yt-dlp: %s*" (truncate-string-to-width url 50)))
		 (output-template (format "%s/%%(channel)s-%%(title)s.%%(ext)s" videos-dir))
		 (url-display (truncate-string-to-width url 50))
		 (process (start-process "yt-dlp" buffer-name
								"tsp" "yt-dlp" "--add-metadata" "-ic"
								"-o" output-template url)))
	(message "Started downloading: %s" url-display)
	(set-process-sentinel process
						  (lambda (proc event)
							(cond
							 ((string-match-p "finished" event)
							  (message "✓ Finished downloading: %s" url-display))
							 ((string-match-p "exited abnormally" event)
							  (message "✗ Download failed: %s" url-display)))
							(when (string-match-p "finished\\|exited" event)
							  (kill-buffer (process-buffer proc)))))))

(defun cj/mpv-play-it (url)
  "Play the URL with mpv in an async shell."
  (unless (executable-find "mpv")
    (error "mpv is not installed or not in PATH"))
  (let* ((buffer-name (format "*mpv: %s*" (truncate-string-to-width url 50)))
		 (url-display (truncate-string-to-width url 50))
		 (process (start-process "mpv" buffer-name "mpv" url)))
	(message "Started playing: %s" url-display)
	(set-process-sentinel process
						  (lambda (proc event)
							(cond
							 ((string-match-p "finished" event)
							  (message "✓ Finished playing: %s" url-display))
							 ((string-match-p "exited abnormally" event)
							  (message "✗ Playback failed: %s" url-display)))
							(when (string-match-p "finished\\|exited" event)
							  (kill-buffer (process-buffer proc)))))))

(defun cj/elfeed-youtube-dl ()
  "Downloads the selected Elfeed entries' links with youtube-dl."
  (interactive)
  (cj/elfeed-process-entries #'cj/yt-dl-it "download"))

(defun cj/play-with-mpv ()
  "Plays the selected Elfeed entries' links with MPV."
  (interactive)
  (cj/elfeed-process-entries #'cj/mpv-play-it "play"))

;; --------------------- Youtube Url To Elfeed Feed Format ---------------------

(defun cj/youtube-to-elfeed-feed-format (url type)
  "Convert YouTube URL to elfeed-feeds format.
TYPE should be either 'channel or 'playlist."
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
		(format ";; %s\n(\"%s\" youtube)"
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
