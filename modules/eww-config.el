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

;; ----------------------- EWW-Only User-Agent Injection -----------------------

(require 'cl-lib)

(defgroup my-eww-user-agent nil
  "EWW-only User-Agent management."
  :group 'eww)

(defcustom my-eww-user-agent
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:128.0) Gecko/20100101 Firefox/128.0"
  "User-Agent string to send for EWW requests only."
  :type 'string
  :group 'my-eww-user-agent)

(defun my-eww--inject-user-agent (orig-fun &rest args)
  "Set a User-Agent only when making requests from an EWW buffer."
  (if (derived-mode-p 'eww-mode)
	  (let* ((ua my-eww-user-agent)
			 ;; Remove any existing UA header, then add ours.
			 (url-request-extra-headers
			  (cons (cons "User-Agent" ua)
					(cl-remove-if (lambda (h)
									(and (consp h)
										 (stringp (car h))
										 (string-equal (car h) "User-Agent")))
								  url-request-extra-headers))))
		(apply orig-fun args))
	(apply orig-fun args)))

(with-eval-after-load 'url
  ;; Cover both async and sync fetches used by eww/shr (pages, images, etc.).
  (advice-add 'url-retrieve :around #'my-eww--inject-user-agent)
  (advice-add 'url-retrieve-synchronously :around #'my-eww--inject-user-agent))

;; --------------------------------- EWW Config --------------------------------

(use-package eww
  :ensure nil ;; built-in
  :bind
  (("M-E" . eww)
   :map eww-mode-map
   ("<" . eww-back-url)
   (">" . eww-forward-url)
   ("i" . eww-toggle-images)
   ("u" . cj/eww-copy-url)
   ("b" . cj/eww-bookmark-quick-add)
   ("B" . eww-list-bookmarks)
   ("/" . cj/eww-switch-search-engine)
   ("&" . cj/eww-open-in-external)
   ("o" . eww-open-in-new-buffer)
   ("r" . eww-readable))

  :config
  ;; Define search engines
  (defvar cj/eww-search-engines
	'(("frog" . "http://frogfind.com/?q=")
	  ("ddg" . "https://duckduckgo.com/html?q=")
	  ("searx" . "https://searx.be/search?q="))
	"List of search engines for EWW.")

  (defvar cj/eww-current-search-engine "frog"
	"Currently selected search engine.")

  ;; Function definitions
  (defun cj/eww-switch-search-engine ()
	"Switch between different search engines."
	(interactive)
	(let* ((engine (completing-read "Search engine: "
									(mapcar #'car cj/eww-search-engines)
									nil t nil nil cj/eww-current-search-engine))
		   (url (cdr (assoc engine cj/eww-search-engines))))
	  (when url
		(setq eww-search-prefix url)
		(setq cj/eww-current-search-engine engine)
		(message "Search engine set to: %s" engine))))

  (defun cj/eww-open-in-external ()
	"Open current URL in external browser."
	(interactive)
	(unless (derived-mode-p 'eww-mode)
	  (user-error "Not in EWW buffer"))
	(if-let ((url (plist-get eww-data :url)))
		(browse-url-xdg-open url)
	  (user-error "No URL to open")))

  (defun cj/eww-bookmark-quick-add ()
	"Quickly bookmark current page with minimal prompting."
	(interactive)
	(unless (derived-mode-p 'eww-mode)
	  (user-error "Not in EWW buffer"))
	(when-let ((title (plist-get eww-data :title)))
	  (let ((eww-bookmarks-directory (expand-file-name "eww-bookmarks" user-emacs-directory)))
		(unless (file-exists-p eww-bookmarks-directory)
		  (make-directory eww-bookmarks-directory t))
		(eww-add-bookmark)
		(message "Bookmarked: %s" title))))

  (defun cj/eww-copy-url ()
	"Copy the current EWW URL to clipboard."
	(interactive)
	(unless (derived-mode-p 'eww-mode)
	  (user-error "Not in EWW buffer"))
	(if-let ((current-url (plist-get eww-data :url)))
		(progn
		  (kill-new current-url)
		  (message "URL copied: %s" current-url))
	  (message "No URL to copy")))

  (defun cj/eww-clear-cookies ()
	"Clear all cookies."
	(interactive)
	(setq url-cookie-storage nil)
	(when (and url-cookie-file (file-exists-p url-cookie-file))
	  (delete-file url-cookie-file))
	(message "Cookies cleared"))

  ;; Configuration settings
  (setq shr-use-colors nil)
  (setq shr-bullet "• ")
  (setq eww-search-prefix (cdr (assoc cj/eww-current-search-engine cj/eww-search-engines)))
  (setq url-cookie-file (expand-file-name "~/.local/share/cookies.txt"))
  ;; sets the user-agent for everything (e.g., package.el)
  ;;(setq url-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:128.0) Gecko/20100101 Firefox/128.0")
  (setq url-privacy-level '(email lastloc))
  (setq shr-inhibit-images t)
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.2)
  (setq eww-retrieve-command nil))

(provide 'eww-config)
;;; eww-config.el ends here
