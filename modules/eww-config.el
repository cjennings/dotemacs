;;; eww-config --- EWW Text Browser Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; This module provides a minimal, privacy-focused browsing experience with:
;; - Simplified navigation keybindings (< and > for back/forward)
;; - Quick URL copying to clipboard
;; - Image toggle functionality
;; - Privacy-conscious defaults (no tracking info sent)
;; - Alternative search engine (Frog Find for simplified web pages)
;;
;; Key features:
;; - `M-E' to launch EWW
;; - `u' to copy current URL
;; - `i' to toggle images
;; - `o' to open link in new buffer
;;
;; The configuration tries to prioritize text-based browsing and minimal distractions.
;;
;;; Code:

;; --------------------------- EWW (Emacs Web Wowser) --------------------------

(use-package eww
  :ensure nil ;; built-in
  :bind
  ("M-E" . eww)
  (:map eww-mode-map
        ("<" . eww-back-url)                ;; in addition to 'l'
        (">" . eww-forward-url)             ;; in addition to 'n'
        ("i" . eww-toggle-images)           ;; toggle images on/off (default off)
        ("u" . cj/eww-copy-url)             ;; copy url to clipboard
        ("b" . cj/eww-bookmark-quick-add)   ;; quick add bookmark
        ("B" . eww-list-bookmarks)          ;; list all eww bookmarks
        ("/" . cj/eww-switch-search-engine) ;; swap the search engine
        ("&" . cj/eww-open-in-external)     ;; open in external browser
        ("o" . eww-open-in-new-buffer)
        ("r" . cj/eww-readable))            ;; strip page down to readable content

  :init
  (defvar cj/eww-search-engines
    '(("frog" . "http://frogfind.com/?q=")
      ("ddg" . "https://duckduckgo.com/html?q=")
      ("searx" . "https://searx.be/search?q="))
    "List of search engines for EWW.")

  (defun cj/eww-switch-search-engine ()
    "Switch between different search engines."
    (interactive)
    (let* ((engine (completing-read "Search engine: "
                                    (mapcar #'car cj/eww-search-engines)))
           (url (cdr (assoc engine cj/eww-search-engines))))
      (setq eww-search-prefix url)
      (message "Search engine set to: %s" engine)))

  (defun cj/eww-open-in-external ()
    "Open current URL in external browser."
    (interactive)
    (when-let ((url (plist-get eww-data :url)))
      (browse-url-xdg-open url)))

  (defun cj/eww-readable ()
    "Toggle readable mode for current page."
    (interactive)
    (let ((url (plist-get eww-data :url)))
      (eww-readable)
      (message "Readable mode applied")))

  (defun cj/eww-bookmark-quick-add ()
    "Quickly bookmark current page with minimal prompting."
    (interactive)
    (let ((url (plist-get eww-data :url))
          (title (plist-get eww-data :title)))
      (eww-add-bookmark url (read-string "Bookmark name: " title))
      (message "Bookmarked: %s" title)))

  (defun cj/eww-copy-url ()
    "Copy the current EWW URL to clipboard."
    (interactive)
    (when (eq major-mode 'eww-mode)
      (let ((current-url (plist-get eww-data :url)))
        (if current-url
            (progn
              (kill-new current-url)
              (message "URL copied: %s" current-url))
          (message "No URL to copy")))))

  (setq shr-use-colors nil)                          ;; disable colors in the html
  (setq shr-bullet "• ")                             ;; unordered lists use bullet glyph
  (setq eww-search-prefix "http://frogfind.com/?q=") ;; use Frog Find as search engine
  (setq url-cookie-file "~/.local/share/cookies.txt")
  (setq url-privacy-level '(email agent lastloc))   ;; don't send any info listed here

  (setq shr-inhibit-images t)         ;; Don't load images by default
  (setq shr-use-fonts nil)            ;; Don't use variable fonts
  (setq shr-max-image-proportion 0.2) ;; Limit image size when enabled
  (setq eww-retrieve-command nil))     ;; Use built-in URL retrieval


(provide 'eww-config)
;;; eww-config.el ends here
