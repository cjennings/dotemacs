;;; quick-video-capture.el --- Video Capturing with Org Capture -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; This package provides a seamless "fire-and-forget" workflow for downloading
;; videos from the browser to your local system using yt-dlp and task-spooler.
;;
;; Features:
;; - Browser bookmarklet integration via org-protocol
;; - Automatic queueing of downloads through task-spooler
;; - Works with any yt-dlp supported site
;; - Can be triggered manually via org-capture with URL prompt
;;
;; Setup:
;; 1. Load this file and call (cj/setup-video-download)
;; 2. Add the bookmarklet from `cj/video-download-bookmarklet' to your browser
;; 3. Click the bookmarklet on any video page to queue a download
;;
;; Alternatively, trigger manually with C-c c v and enter a URL

;;; Code:

(require 'org-protocol)
(require 'org-capture)
(require 'media-utils)

(defconst cj/video-download-bookmarklet
  "javascript:location.href='org-protocol://video-download?url='+encodeURIComponent(location.href);void(0);"
  "JavaScript bookmarklet for triggering video downloads from the browser.
Add this as a bookmark in your browser to enable one-click video downloads.")

(defun cj/org-protocol-video-download (info)
  "Process org-protocol video download requests.
INFO is a plist containing :url from the org-protocol call."
  (let ((url (plist-get info :url)))
	(when url
	  ;; Store the URL for the capture template to use
	  (setq cj/video-download-current-url url))
	;; Trigger the capture
	(org-capture nil "v")
	nil))  ; Return nil to indicate we handled it

(defun cj/video-download-capture-handler ()
  "Handle video download during org-capture.
This function is called from the capture template."
  (let ((url (or cj/video-download-current-url
				 (read-string "Video URL: "))))
	;; Clear the stored URL after using it
	(setq cj/video-download-current-url nil)
	(if (string-empty-p url)
		(error "No URL provided for download")
	  (cj/yt-dl-it url)
	  ;; Return empty string to prevent capture from saving anything
	  "")))

(defvar cj/video-download-current-url nil
  "Temporary storage for URL passed via org-protocol.")

;; register the handler and the capture template after org-protocol is loaded
(with-eval-after-load 'org-protocol
  ;; Register the org-protocol handler
  (add-to-list 'org-protocol-protocol-alist
			   '("video-download"
				 :protocol "video-download"
				 :function cj/org-protocol-video-download
				 :kill-client t))

  ;; Add the capture template
  (add-to-list 'org-capture-templates
			   '("v" "Video Download" entry
				 (file "")  ; No file needed since we're not saving
				 "%(cj/video-download-capture-handler)"
				 :immediate-finish t
				 :jump-to-captured nil)))

(defun cj/video-download-bookmarklet-instructions ()
  "Display instructions for setting up the browser bookmarklet."
  (interactive)
  (let ((buf (get-buffer-create "*Video Download Bookmarklet Setup*")))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert "Video Download Bookmarklet Setup\n")
	  (insert "=================================\n\n")
	  (insert "1. Create a new bookmark in your browser\n")
	  (insert "2. Set the name to: Download Video (or your preference)\n")
	  (insert "3. Set the URL to the following JavaScript code:\n\n")
	  (insert cj/video-download-bookmarklet)
	  (insert "\n\n")
	  (insert "4. Save the bookmark to your bookmarks bar\n")
	  (insert "5. Click the bookmark when viewing a video to download it\n\n")
	  (insert "Note: Make sure Emacs server is running (M-x server-start)\n")
	  (insert "and emacsclient is properly configured for org-protocol.\n"))
	(switch-to-buffer buf)))

;;; Commentary for bookmarklet:
;;
;; To use the browser bookmarklet, add this JavaScript as a bookmark URL:
;; javascript:location.href='org-protocol://video-download?url='+encodeURIComponent(location.href);void(0);
;;
;; This will send the current page URL to Emacs via org-protocol, triggering
;; the download through yt-dlp and task-spooler.

(provide 'quick-video-capture)
;;; quick-video-capture.el ends here.
