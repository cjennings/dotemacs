;;; eww-config --- EWW Text Browser Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; -------------------------------- Eww Copy Url -------------------------------

(defun cj/eww-copy-url ()
  "Copies the eww url to clipboard"
  (interactive)
  (when (string= major-mode "eww-mode")
	(let ((current-url (plist-get eww-data :url)))
	  (when current-url
		(kill-new current-url)))))

;; (defun cj/eww-copy-url ()
;;   "Copies the eww url to clipboard"
;;   (interactive)
;;   (when (string= major-mode "eww-mode") ; Ensure we're in eww-mode
;; 	(kill-new eww-current-url))) ; Copy to clipboard

;; ------------------------------------ EWW ------------------------------------

(use-package eww
  :ensure nil ;; built-in
  :bind
  ("M-E" . eww)
  (:map eww-mode-map
        ("<" . eww-back-url)    ;; in addition to 'l'
        (">" . eww-forward-url) ;; in addition to 'n'
		("i" . eww-toggle-images)
		("u" . cj/eww-copy-url)
        ("o" . eww-open-in-new-buffer))
  :config
  (setq shr-use-colors nil)                          ;; respect colors in the html
  (setq shr-bullet "• ")                             ;; unordered lists use bullet glyph
  (setq shr-folding-mode t)
  (setq eww-search-prefix "http://frogfind.com/?q=") ;; use Frog Find as search engine
  ;; (setq eww-search-prefix "https://duckduckgo.com/html?q=") ;; use Duck Duck Go as search engine
  (setq url-cookie-file "~/.local/share/cookies.txt")
  (setq url-privacy-level '(email agent lastloc)))   ;; don't send any info listed here

(provide 'eww-config)
;;; eww-config.el ends here
