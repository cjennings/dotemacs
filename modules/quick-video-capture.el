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
;; 1. Load this file - it will auto-initialize after Emacs startup
;; 2. Add the bookmarklet from `cj/video-download-bookmarklet' to your browser
;; 3. Click the bookmarklet on any video page to queue a download
;;
;; Alternatively, trigger manually with C-c c v and enter a URL

;;; Code:

(require 'system-lib)

;; Declare external functions to avoid warnings
(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-protocol-check-filename-for-protocol "org-protocol" (fname restoffiles client))
(declare-function cj/yt-dl-it "media-utils" (url))

(defvar org-capture-templates)
(defvar org-protocol-protocol-alist)

(defconst cj/video-download-bookmarklet
  "javascript:location.href='org-protocol://video-download?url='+encodeURIComponent(location.href);void(0);"
  "JavaScript bookmarklet for triggering video downloads from the browser.
Add this as a bookmark in your browser to enable one-click video downloads.")

(defvar cj/video-download-current-url nil
  "Temporary storage for URL passed via org-protocol.")

(defvar cj/video-download-initialized nil
  "Flag to track if video download has been initialized.")

(defun cj/org-protocol-video-download (info)
  "Process org-protocol video download requests.
INFO is a plist containing :url from the org-protocol call."
  ;; Ensure we're initialized when called via protocol
  (cj/ensure-video-download-initialized)
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
  ;; Ensure media-utils is loaded when we actually need to download
  (require 'media-utils)
  (let ((url (or cj/video-download-current-url
                 (read-string "Video URL: "))))
    ;; Clear the stored URL after using it
    (setq cj/video-download-current-url nil)
    (if (string-empty-p url)
        (error "No URL provided for download")
      (cj/yt-dl-it url)
      ;; Return empty string to prevent capture from saving anything
      "")))

(defun cj/ensure-video-download-initialized ()
  "Ensure video download is initialized, loading dependencies if needed."
  (unless cj/video-download-initialized
    (cj/setup-video-download)))

(defun cj/setup-video-download ()
  "Initialize video download functionality.
This function sets up org-protocol handlers and capture templates.
It's designed to be idempotent - safe to call multiple times."
  (when (not cj/video-download-initialized)
    ;; Load required packages
    (require 'org-protocol)
    (require 'org-capture)
    
    ;; Register the org-protocol handler if not already registered
    (unless (assoc "video-download" org-protocol-protocol-alist)
      (add-to-list 'org-protocol-protocol-alist
                   '("video-download"
                     :protocol "video-download"
                     :function cj/org-protocol-video-download
                     :kill-client t)))
    
    ;; Add the capture template if not already added
    (unless (assoc "v" org-capture-templates)
      (add-to-list 'org-capture-templates
                   '("v" "Video Download" entry
                     (file "")  ; No file needed since we're not saving
                     "%(cj/video-download-capture-handler)"
                     :immediate-finish t
                     :jump-to-captured nil)))
    
    (setq cj/video-download-initialized t)
    (cj/log-silently "Video download functionality initialized")))

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

;; Deferred initialization strategy:
;; 1. Try to load shortly after Emacs is idle following init
;; 2. Fallback timer ensures loading within 2 seconds regardless
(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer 0.5 nil #'cj/setup-video-download)))

;; Fallback: ensure initialization within 2 seconds of loading this file
(run-with-timer 2 nil #'cj/setup-video-download)

;; If someone manually triggers capture before initialization
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook #'cj/ensure-video-download-initialized))

;;; Commentary for bookmarklet:
;;
;; To use the browser bookmarklet, add this JavaScript as a bookmark URL:
;; javascript:location.href='org-protocol://video-download?url='+encodeURIComponent(location.href);void(0);
;;
;; This will send the current page URL to Emacs via org-protocol, triggering
;; the download through yt-dlp and task-spooler.

(provide 'quick-video-capture)
;;; quick-video-capture.el ends here