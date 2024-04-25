;;; elfeed-config --- Settings and Enhancements to the Elfeed RSS Feed Reader -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ------------------------------- Elfeed Config -------------------------------

(use-package elfeed
  :bind
  (:map elfeed-show-mode-map
        ("w"  . eww-open-in-new-buffer))
  (:map elfeed-search-mode-map
        ("w"   . cj/elfeed-eww-open)                ;; opens in eww
        ("b"   . cj/elfeed-browser-open)            ;; opens in external browser
        ("d"   . cj/elfeed-youtube-dl)              ;; async download with yt-dlp and tsp
		("p"   . cj/play-with-mpv)                  ;; async play with mpv
		("v"   . cj/play-with-mpv)                  ;; async play with mpv
        ("R"   . cj/elfeed-mark-all-as-read)        ;; capital marks all as read, since upper case marks one as read
        ("U"   . cj/elfeed-mark-all-as-unread))     ;; capital marks all as unread, since lower case marks one as unread
  :config
  (setq elfeed-db-directory (concat user-emacs-directory ".elfeed-db"))
  (setq-default elfeed-search-title-max-width 150)
  (setq-default elfeed-search-title-min-width 80)
  (setq-default elfeed-search-filter "+mustread +unread"))

;; ---------------------------- Elfeed Org Feed List ---------------------------

(use-package elfeed-org
  :defer .5
  :after elfeed
  :config
  (setq rmh-elfeed-org-files
        (list (concat user-emacs-directory "assets/elfeed-feeds.org")))
  (elfeed-org))

;; ------------------------------ Elfeed Dashboard -----------------------------

(use-package elfeed-dashboard
  :defer .5
  :bind
  ("M-R" . elfeed-dashboard)
  :config
  (setq elfeed-dashboard-file (concat user-emacs-directory "assets/elfeed-dashboard.org"))
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

;; ------------------------------ Elfeed Functions -----------------------------

(defun cj/elfeed-open ()
  "Open Elfeed, update all feeds, then move to the first entry."
  (interactive)
  (elfeed)
  (elfeed-update)
  (elfeed-search-update--force))

;; -------------------------- Elfeed Filter Functions --------------------------

(defun cj/elfeed-mark-all-as-read ()
  "Temove the 'unread' tag from all elfeed entries visible in the elfeed search buffer."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun cj/elfeed-mark-all-as-unread ()
  "Add the 'unread' tag from all elfeed entries visible in the elfeed search buffer."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-tag-all 'unread))

(defun cj/elfeed-set-filter-and-update(filterstring)
  "Set the Elfeed filter to 'FILTERSTRING' and update the the buffer."
  (interactive "P")
  (setq elfeed-search-filter filterstring)
  (elfeed-search-update--force)
  (elfeed-search-first-entry))

;; -------------------------- Elfeed Browser Functions -------------------------

(defun cj/elfeed-eww-open (&optional use-generic-p)
  "Open currently selected entry with EWW."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do
             (add-hook 'eww-after-render-hook #'cj/eww-readable-nonce)
             (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; hook for cj/elfeed-eww-open to open entry in eww readable mode
;; https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode/47757
(defun cj/eww-readable-nonce ()
  "Once-off call to eww-readable after EWW is done rendering."
  (unwind-protect
      (progn
        (eww-readable)
        (goto-char (point-min)))
    (remove-hook 'eww-after-render-hook #'cj/eww-readable-nonce)))

(defun cj/elfeed-browser-open (&optional use-generic-p)
  "Open the currently selected entry with default browser."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (browse-url-default-browser it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; --------------------- Elfeed Plan And Download Functions --------------------

(defun cj/elfeed-youtube-dl (&optional use-generic-p)
  "Youtube-DL link."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (cj/yt-dl-it it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun cj/yt-dl-it (url)
  "Downloads the URL in an async shell."
  (let ((default-directory videos-dir))
    (save-window-excursion
      (async-shell-command (format "tsp yt-dlp --add-metadata -ic -o '%%(channel)s-%%(title)s.%%(ext)s' '%s'" url)))))

(defun cj/play-with-mpv (&optional use-generic-p)
  "MPV link."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (cj/mpv-play-it it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun cj/mpv-play-it (url)
  "Play the URL with mpv in an async shell."
  (async-shell-command  (format "mpv '%s'" url)))

(provide 'elfeed-config)
;;; elfeed-config.el ends here
