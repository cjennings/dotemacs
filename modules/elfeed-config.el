;;; elfeed-config --- Settings and Enhancements to the Elfeed RSS Feed Reader -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; optional feed reader, a command-loaded deferral
;;   candidate.
;; Top-level side effects: package configuration via use-package (binds the
;;   elfeed launch key and the search/show-mode keys).
;; Runtime requires: user-constants, system-lib, media-utils.
;; Direct test load: yes.
;;
;; Launch Elfeed with M-R to update feeds and focus the newest entry right away.
;; Inside the search buffer:
;; - Use v to stream via the default player, d to download, w/b to open via EWW or browser.
;; - Hit V to pick a different player for future launches.
;; - Use R/U to mark all visible stories read or unread before narrowing again.
;;
;; When a video needs streaming credentials the player selection drives yt-dlp format choices;
;; use `cj/select-media-player` to swap profiles, or customize `cj/media-players` for your system.
;; All commands assume yt-dlp and your players live on PATH.
;;
;;; Code:

(require 'user-constants)
(require 'system-lib)
(require 'media-utils)

(declare-function elfeed "elfeed")
(declare-function elfeed-update "elfeed")
(declare-function elfeed-entry-link "elfeed")
(declare-function elfeed-untag "elfeed")
(declare-function elfeed-search-selected "elfeed")
(declare-function elfeed-search-tag-all "elfeed")
(declare-function elfeed-search-update-entry "elfeed")
(declare-function elfeed-search-update--force "elfeed")
(declare-function elfeed-search-untag-all-unread "elfeed")
(declare-function eww-browse-url "eww")
(declare-function eww-readable "eww")

;; elfeed paints its search and entry buffers with manual `face' text properties
;; (the date, title, feed, and tag faces the theme styles).  Left in
;; `global-font-lock-mode', font-lock overwrites those with syntactic string
;; fontification, so the buffer loses the theme colors.  Exclude both modes, the
;; same reason dashboard and mu4e are excluded.
(cj/exclude-from-global-font-lock 'elfeed-search-mode 'elfeed-show-mode)

;; ------------------------------- Elfeed Config -------------------------------

(use-package elfeed
  :bind
  (:map elfeed-show-mode-map
        ("w"  . eww-open-in-new-buffer))
  (:map elfeed-search-mode-map
        ("V"   . cj/select-media-player))       ;; Capital V to select player
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

          ;; Platypus Economics with Justin Wolfers
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCB5eaPWEwR6wR2MxRx64s0g" yt platypus)

          ;; Conversations with Tyler (Tyler Cowen)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_AnpBvnhXTcipgGEHLWoOg" yt cwt)

          ;; Plain English with Derek Thompson
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoOUW7SiXzLbc_O3nSDOBYA" yt plain-english)

          ;; Odd Lots (Bloomberg) -- Joe Weisenthal & Tracy Alloway
          ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLe4PRejZgr0MuA6M0zkZyy-99-qc87wKV" yt oddlots)

          ;; All-In Podcast
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCESLZhusAkFfsNsApnjF_Cg" yt allin)

          ;; The Prof G Pod
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLtQ-jBytlXCasRuBG86m22rOQfrEPcctq" yt profg)

		  ;; On with Kara Swisher
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLKof9YSAshgxI6odrEJFKsJbxamwoQBju" yt on)

          ;; Raging Moderates
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCcvDWzvxz6Kn1iPQHMl2teA" yt raging-moderates)

          ;; Prof G Markets
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PLtQ-jBytlXCY28ucRF8P1mNMSG8uC06Aw" yt profg-markets)

		  ;; Trae Crowder Porch Rants
		  ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL45Mc1cDgnsB-u1iLPBYNF1fk-y1cVzTJ" yt trae)

		  ;; Tropical Tidbits
		  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrFIk7g_riIm2G2Vi90pxDA" yt tropical)

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
(keymap-global-set "M-S-r" #'cj/elfeed-open)  ;; was M-R

;; -------------------------- Elfeed Filter Functions --------------------------

(defun cj/elfeed-mark-all-as-read ()
  "Remove the \='unread\=' tag from all visible entries in search buffer."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max) nil t)
  (elfeed-search-untag-all-unread))

(defun cj/elfeed-mark-all-as-unread ()
  "Add the \='unread\=' tag from all visible entries in the search buffer."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max) nil t)
  (elfeed-search-tag-all 'unread))

(defun cj/elfeed-set-filter-and-update (filterstring)
  "Set the Elfeed filter to FILTERSTRING and update the buffer."
  (interactive "sFilter: ")
  (setq elfeed-search-filter filterstring)
  (elfeed-search-update--force)
  (goto-char (point-min)))

;; ----------------------------- Extract Stream URL ----------------------------
;; TASK: Is this method reused anywhere here or in another file?

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
		 (output (with-temp-buffer
				   (let ((exit-code (apply #'call-process
										   (car cmd-args) nil t nil
										   (cdr cmd-args))))
					 (if (zerop exit-code)
						 (string-trim (buffer-string))
					   nil)))))
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

(defconst cj/elfeed-url-fetch-timeout 10
  "Seconds to wait for a synchronous YouTube page fetch before giving up.
Without a timeout a hung request would block Emacs indefinitely.")

(defun cj/--decode-html-entities (text)
  "Decode the common HTML entities in TEXT.
Handles &amp; &lt; &gt; &quot; &#39; and &#x27; -- the entities YouTube's
og:title meta tag emits.  Decoded left-to-right, &amp; first."
  (let ((entities '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                    ("&quot;" . "\"") ("&#39;" . "'") ("&#x27;" . "'"))))
    (dolist (pair entities text)
      (setq text (replace-regexp-in-string (car pair) (cdr pair) text)))))

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

    (unwind-protect
        (progn
          ;; Extract ID based on type
          (if (eq type 'channel)
              ;; For channels, we need to fetch the page to get the channel_id
              (progn
                (setq buffer (url-retrieve-synchronously
                              url nil nil cj/elfeed-url-fetch-timeout))
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
              (setq buffer (url-retrieve-synchronously
                            url nil nil cj/elfeed-url-fetch-timeout))))

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
                ;; Decode HTML entities in the extracted title
                (setq title (cj/--decode-html-entities title))))))
      ;; Always kill the temporary URL buffer, even when extraction failed --
      ;; the old code only killed it when an ID was found, leaking it otherwise.
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))

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

;; --------------------------- Search-Mode Keybindings -------------------------
;; Bound here (not in use-package :bind) because these commands are defined in
;; this file; a :bind autoload stub plus the defun triggers a "defined multiple
;; times" byte-compile warning.

(with-eval-after-load 'elfeed
  (keymap-set elfeed-search-mode-map "w" #'cj/elfeed-eww-open)        ;; opens in eww
  (keymap-set elfeed-search-mode-map "b" #'cj/elfeed-browser-open)    ;; opens in external browser
  (keymap-set elfeed-search-mode-map "d" #'cj/elfeed-youtube-dl)      ;; async download with yt-dlp and tsp
  (keymap-set elfeed-search-mode-map "v" #'cj/play-with-video-player) ;; async play with mpv
  (keymap-set elfeed-search-mode-map "R" #'cj/elfeed-mark-all-as-read)    ;; capital R marks all read (lower case marks one)
  (keymap-set elfeed-search-mode-map "U" #'cj/elfeed-mark-all-as-unread)) ;; capital U marks all unread (lower case marks one)

(provide 'elfeed-config)
;;; elfeed-config.el ends here.
