;;; org-webclipper.el --- Web Page Clipping via org-protocol -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
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
;; Requirements:
;; - org-web-tools package
;; - Pandoc installed on your system
;; - Emacs server running (M-x server-start)

;;; Code:

;; Declare functions and variables to avoid warnings
(declare-function org-protocol-protocol-alist "org-protocol")
(declare-function org-capture "org-capture")
(declare-function org-capture-get "org-capture")
(declare-function org-web-tools--url-as-readable-org "org-web-tools")
(declare-function org-w3m-copy-for-org-mode "org-w3m")
(declare-function org-eww-copy-for-org-mode "org-eww")
(declare-function org-at-heading-p "org")
(declare-function org-heading-components "org")
(declare-function org-copy-subtree "org")
(declare-function org-cut-subtree "org")
(declare-function org-id-new "org-id")
(declare-function org-roam-db-sync "org-roam")
(defvar org-capture-templates)
(defvar org-protocol-protocol-alist)
(defvar org-roam-directory)
(defvar webclipped-file)

;; Variables for storing org-protocol data
(defvar cj/webclip-current-url nil
  "Temporary storage for URL passed via org-protocol.")

(defvar cj/webclip-current-title nil
  "Temporary storage for page title passed via org-protocol.")

;; Flag to track if we've done initialization
(defvar cj/webclipper-initialized nil
  "Track if webclipper has been initialized.")

;; Lazy initialization function
(defun cj/webclipper-ensure-initialized ()
  "Ensure webclipper is initialized when first used."
  (unless cj/webclipper-initialized
    ;; Load required packages now
    (require 'org-protocol)
    (require 'org-capture)
    (require 'user-constants) ;; for webclipped-file

    ;; Register the org-protocol handler
    (add-to-list 'org-protocol-protocol-alist
                 '("webclip"
                   :protocol "webclip"
                   :function cj/org-protocol-webclip
                   :kill-client t))

    ;; Add capture templates if not already present
    (unless (assoc "W" org-capture-templates)
      (add-to-list 'org-capture-templates
                   '("W" "Web Clipper (Protocol)" entry
                     (file+headline webclipped-file "Webclipped Inbox")
                     "* [[%(identity cj/webclip-current-url)][%(identity cj/webclip-current-title)]] :website:\nURL: %(identity cj/webclip-current-url)\nCaptured On:%U\n%(cj/org-protocol-webclip-handler)\n"
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

;;;###autoload
(defun cj/org-protocol-webclip (info)
  "Process org-protocol webclip requests.
INFO is a plist containing :url and :title from the org-protocol call."
  (cj/webclipper-ensure-initialized)
  (let ((url (plist-get info :url))
        (title (plist-get info :title)))
    (when url
      ;; Store the URL and title for the capture template to use
      (setq cj/webclip-current-url url
            cj/webclip-current-title (or title "Untitled")))
    ;; Trigger the capture
    (org-capture nil "W")
    nil))  ; Return nil to indicate we handled it

(defun cj/org-protocol-webclip-handler ()
  "Handle web page clipping during org-capture.
This function is called from the capture template.
It fetches the page content and converts it to Org format."
  ;; Load org-web-tools only when actually needed
  (require 'org-web-tools)
  (setopt org-web-tools-pandoc-sleep-time 0.5)

  (let ((url cj/webclip-current-url)
        (title cj/webclip-current-title))
    ;; Clear the stored values after using them
    (setq cj/webclip-current-url nil
          cj/webclip-current-title nil)

    (if (not url)
        (error "No URL provided for clipping")
      (condition-case err
          (let* ((org-content (org-web-tools--url-as-readable-org url))
                 ;; Process the content to adjust heading levels
                 (processed-content
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
                    (buffer-string))))
            ;; Show success message with the title
            (require 'user-constants) ;; Ensure webclipped-file is available
            (message "'%s' added to %s" title webclipped-file)
            ;; Return the processed content for insertion
            processed-content)
        (error
         ;; Handle any errors during fetching or conversion
         (error "Failed to clip web page: %s" (error-message-string err)))))))

;; ---------------------------- Org Webpage Clipper ----------------------------

;;;###autoload
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

;; ------------------------ Org-Branch To Org-Roam-Node ------------------------

(defun cj/org-link-get-description (text)
  "Extract the description from an org link, or return the text unchanged.
If TEXT contains an org link like [[url][description]], return description.
If TEXT contains multiple links, only process the first one.
Otherwise return TEXT unchanged."
  (if (string-match "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]" text)
      (let ((description (match-string 2 text))
            (url (match-string 1 text)))
        ;; If there's a description, use it; otherwise use the URL
        (or description url))
    text))

;;;###autoload
(defun cj/move-org-branch-to-roam ()
  "Move the org subtree at point to a new org-roam node.
The node filename will be timestamp-based with the heading name.
The heading becomes the node title, and the entire subtree is demoted to level 1.
If the heading contains a link, extract the description for the title."
  (interactive)
  ;; Lazy load org and org-roam when needed
  (require 'org)
  (require 'org-id)
  (require 'org-roam)

  (unless (org-at-heading-p)
    (user-error "Not at an org heading"))

  (let* ((heading-components (org-heading-components))
         (current-level (nth 0 heading-components))
         (raw-title (nth 4 heading-components))
         ;; Extract clean title from potential link
         (title (cj/org-link-get-description raw-title))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         ;; Convert title to filename-safe format
         (title-slug (replace-regexp-in-string
                      "[^a-zA-Z0-9]+" "-"
                      (downcase title)))
         ;; Remove leading/trailing hyphens
         (title-slug (replace-regexp-in-string
                      "^-\\|-$" "" title-slug))
         (filename (format "%s-%s.org" timestamp title-slug))
         (filepath (expand-file-name filename org-roam-directory))
         ;; Generate a unique ID for the node
         (node-id (org-id-new))
         ;; Store the subtree in a temporary buffer
         subtree-content)

    ;; Copy the subtree content
    (org-copy-subtree)
    (setq subtree-content (current-kill 0))

    ;; Now cut it to remove from original buffer
    (org-cut-subtree)

    ;; Process the subtree to demote it to level 1
    (with-temp-buffer
      (org-mode)
      (insert subtree-content)
      ;; Demote the entire tree so the top level becomes level 1
      (goto-char (point-min))
      (when (> current-level 1)
        (let ((demote-count (- current-level 1)))
          (while (re-search-forward "^\\*+ " nil t)
            (beginning-of-line)
            (dotimes (_ demote-count)
              (when (looking-at "^\\*\\*")
                (delete-char 1)))
            (forward-line))))
      (setq subtree-content (buffer-string)))

    ;; Create the new org-roam file
    (with-temp-file filepath
      ;; Insert the org-roam template with ID at file level
      (insert ":PROPERTIES:\n")
      (insert ":ID:       " node-id "\n")
      (insert ":END:\n")
      (insert "#+TITLE: " title "\n")
      (insert "#+CATEGORY: " title "\n")
      (insert "#+FILETAGS: Topic\n\n")

      ;; Insert the demoted subtree content
      (insert subtree-content))

    ;; Sync the org-roam database
    (org-roam-db-sync)

    ;; Message to user
    (message "'%s' added as an org-roam node." title)))

;; ----------------------------- Webclipper Keymap -----------------------------

;; keymaps shouldn't be required for webclipper
;; TASK Move org-branch to roam functionality under org-roam
;; Setup keymaps
;; ;;;###autoload
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
;;;###autoload
(with-eval-after-load 'org-protocol
  (unless (assoc "webclip" org-protocol-protocol-alist)
    (add-to-list 'org-protocol-protocol-alist
                 '("webclip"
                   :protocol "webclip"
                   :function cj/org-protocol-webclip
                   :kill-client t))))

(with-eval-after-load 'cj/custom-keymap
  (require 'org-webclipper)
  (cj/webclipper-setup-keymaps))

(provide 'org-webclipper)
;;; org-webclipper.el ends here
