;;; org-webclipper.el --- Web Page Clipping via org-protocol -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; web clipping runs via org-protocol/command, a Phase 4
;;   protocol/command-loaded deferral candidate.
;; Top-level side effects: org-protocol handler registration via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;; This package provides a seamless "fire-and-forget" workflow for clipping
;; web pages from the browser directly into an Org file using org-protocol
;; and org-web-tools.
;;
;; Features:
;; - Browser bookmarklet integration via org-protocol
;; - Automatic conversion to Org format using eww-readable and Pandoc
;; - One-click capture from any web page
;; - Preserves page structure and formatting
;; - Smart heading adjustment (removes page title, demotes remaining headings)
;;
;; Setup:
;; 1. Ensure this file is loaded in your Emacs configuration
;; 2. Make sure emacsclient is configured for org-protocol
;; 3. Add the following bookmarklet to your browser's bookmarks bar:
;;
;;    javascript:location.href='org-protocol://webclip?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title);void(0);
;;
;;    To add the bookmarklet:
;;    a. Create a new bookmark in your browser
;;    b. Set the name to: Clip to Org (or your preference)
;;    c. Set the URL to the JavaScript code above
;;    d. Save it to your bookmarks bar for easy access
;;
;; 4. Click the bookmarklet on any web page to clip its content
;;
;; The clipped content will be added to the file specified by `webclipped-file`
;; under the "Webclipped Inbox" heading with proper formatting and metadata.
;;
;; Architecture:
;; - cj/--process-webclip-content: Pure function for content processing
;; - cj/org-protocol-webclip-handler: Handles URL fetching and capture
;; - cj/org-webclipper-EWW: Direct capture from EWW/W3M buffers
;;
;; Requirements:
;; - org-web-tools package
;; - Pandoc installed on your system
;; - Emacs server running (M-x server-start)

;;; Code:

(declare-function org-web-tools--url-as-readable-org "org-web-tools")
(declare-function org-w3m-copy-for-org-mode "org-w3m")
(declare-function org-eww-copy-for-org-mode "org-eww")
(declare-function org-capture-get "org-capture")
;; Special vars from org-capture / org-protocol / user-constants, loaded at
;; runtime; declared here so standalone byte-compilation does not warn.
(defvar org-capture-templates)
(defvar org-protocol-protocol-alist)
(defvar webclipped-file)

;; Variables for storing org-protocol data
(defvar cj/--webclip-url nil
  "URL for the active web clip, dynamically bound around `org-capture'.
The org-protocol entry point `let'-binds this for the dynamic extent of
its capture call, so the capture template and handler see it while the
capture runs, and an aborted or erroring capture unwinds the binding
instead of leaving stale state for the next capture.")

(defvar cj/--webclip-title nil
  "Page title for the active web clip, dynamically bound around `org-capture'.
See `cj/--webclip-url' for the binding contract.")

;; Flag to track if we've done initialization
(defvar cj/webclipper-initialized nil
  "Track if webclipper has been initialized.")

(use-package org-web-tools
  :defer t)

;; Lazy initialization function
(defun cj/webclipper-ensure-initialized ()
  "Ensure webclipper is initialized when first used."
  (unless cj/webclipper-initialized
    ;; Load required packages now
    (require 'org-protocol)
    (require 'org-capture)
    (require 'org-web-tools)
    (require 'user-constants) ;; for webclipped-file

    ;; The org-protocol handler registration lives in the
    ;; `with-eval-after-load 'org-protocol' block at the bottom of
    ;; this module -- that's the more robust home (it survives
    ;; org-protocol being loaded before or after this module).  Two
    ;; registration sites would silently drift if the alist entry
    ;; shape ever changes.

    ;; Add capture templates if not already present
    (unless (assoc "W" org-capture-templates)
      (add-to-list 'org-capture-templates
                   '("W" "Web Clipper (Protocol)" entry
                     (file+headline webclipped-file "Webclipped Inbox")
                     "* [[%(identity cj/--webclip-url)][%(identity cj/--webclip-title)]] :website:\nURL: %(identity cj/--webclip-url)\nCaptured On:%U\n%(cj/org-protocol-webclip-handler)\n"
					 :prepend t
                     :immediate-finish t)
                   t))

    (unless (assoc "w" org-capture-templates)
      (add-to-list 'org-capture-templates
                   '("w" "Web Page Clipper" entry
                     (file+headline webclipped-file "Webclipped Inbox")
                     "* %a\nURL: %L\nCaptured On:%U\n%(cj/org-webclipper-EWW)\n"
                     :prepend t :immediate-finish t)
                   t))

    (setq cj/webclipper-initialized t)))

(defun cj/--process-webclip-content (org-content)
  "Process webclip ORG-CONTENT by removing first heading and demoting others.
ORG-CONTENT is the raw org-mode text from the web page conversion.
Returns the processed content as a string with:
- First top-level heading removed
- Initial blank lines removed
- All remaining headings demoted by one level"
  (with-temp-buffer
    (insert org-content)
    (goto-char (point-min))
    ;; Skip the first heading line (we'll use our template's heading)
    (when (looking-at "^\\* .*\n")
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Remove any initial blank lines
    (while (looking-at "^[ \t]*\n")
      (delete-char 1))
    ;; Demote all remaining headings by one level
    ;; since our template already provides the top-level heading
    (while (re-search-forward "^\\(\\*+\\) " nil t)
      (replace-match (concat (match-string 1) "* ") t t))
    (buffer-string)))

(defun cj/org-protocol-webclip (info)
  "Process org-protocol webclip requests.
INFO is a plist containing :url and :title from the org-protocol call.
Signals `user-error' when :url is missing, nil, empty, or non-string -- an
unexpected plist shape used to silently set the globals to nil and fail
downstream inside the capture handler with confusing messages."
  (cj/webclipper-ensure-initialized)
  (let ((url (plist-get info :url))
        (title (plist-get info :title)))
    (unless (and (stringp url) (not (string-empty-p url)))
      (user-error
       "org-protocol webclip: expected non-empty :url string, got %S" url))
    (when (and title (not (stringp title)))
      (user-error
       "org-protocol webclip: :title must be a string when provided, got %S"
       title))
    ;; Bind url+title for the dynamic extent of the capture call only, so
    ;; the template and handler see them while the capture runs and an
    ;; aborted/erroring capture unwinds the binding rather than leaving
    ;; stale state for the next clip.
    (let ((cj/--webclip-url url)
          (cj/--webclip-title (or title "Untitled")))
      (org-capture nil "W"))
    nil))  ; Return nil to indicate we handled it

(defun cj/org-protocol-webclip-handler ()
  "Handle web page clipping during org-capture.
This function is called from the capture template.
It fetches the page content and converts it to Org format."
  ;; Load org-web-tools only when actually needed.  Use plain `setq'
  ;; rather than `setopt' because the variable is a plain float with no
  ;; custom-set handler that needs to fire, and `setopt' is a macro --
  ;; tests that try to stub it via `cl-letf' on the function cell hit
  ;; the already-expanded `setopt--set' in the byte-compiled handler
  ;; and fail with a `void-variable widget-field-keymap' error from
  ;; the customize machinery loading lazily.
  (require 'org-web-tools)
  (unless (executable-find "pandoc")
    (user-error
     "pandoc not found on PATH; it is required to convert the clipped page to Org"))
  (setq org-web-tools-pandoc-sleep-time 0.5)

  (let ((url cj/--webclip-url)
        (title cj/--webclip-title))
    (if (not url)
        (error "No URL provided for clipping")
      (condition-case err
          (let* ((org-content (org-web-tools--url-as-readable-org url))
                 (processed-content (cj/--process-webclip-content org-content)))
            ;; Show success message with the title
            (require 'user-constants) ;; Ensure webclipped-file is available
            (message "'%s' added to %s" title webclipped-file)
            ;; Return the processed content for insertion
            processed-content)
        (error
         ;; Handle any errors during fetching or conversion
         (error "Failed to clip web page: %s" (error-message-string err)))))))

;; ---------------------------- Org Webpage Clipper ----------------------------


(defun cj/org-webclipper-EWW ()
  "Capture the current web page for later viewing in an Org file.
Return the yanked content as a string so templates can insert it."
  (interactive)
  (cj/webclipper-ensure-initialized)
  (let* ((source-buffer (org-capture-get :original-buffer))
         (source-mode (with-current-buffer source-buffer major-mode)))
    (cond
     ((eq source-mode 'w3m-mode)
      (with-current-buffer source-buffer
        (org-w3m-copy-for-org-mode)))
     ((eq source-mode 'eww-mode)
      (with-current-buffer source-buffer
        (org-eww-copy-for-org-mode)))
     (t
      (error "Not valid -- must be in w3m or eww mode")))
    ;; extract the webpage content from the kill ring
    (car kill-ring)))

;; ----------------------------- Webclipper Keymap -----------------------------

;; keymaps shouldn't be required for webclipper
;; Setup keymaps
;;
;; (defun cj/webclipper-setup-keymaps ()
;;   "Setup webclipper keymaps."
;;   (define-prefix-command 'cj/webclipper-map nil
;;                          "Keymap for weblipper operations.")
;;   (define-key cj/custom-keymap "c" 'cj/webclipper-map)
;;   (define-key cj/webclipper-map "n" 'cj/move-org-branch-to-roam))

;; ;; Call keymap setup if cj/custom-keymap is already defined
;; (when (boundp 'cj/custom-keymap)
;;   (cj/webclipper-setup-keymaps))

;; Register protocol handler early for external calls
(with-eval-after-load 'org-protocol
  (unless (assoc "webclip" org-protocol-protocol-alist)
    (add-to-list 'org-protocol-protocol-alist
                 '("webclip"
                   :protocol "webclip"
                   :function cj/org-protocol-webclip
                   :kill-client t))))

;; (with-eval-after-load 'cj/custom-keymap
;;   (require 'org-webclipper)
;;   (cj/webclipper-setup-keymaps))

(provide 'org-webclipper)
;;; org-webclipper.el ends here
