;;; org-capture-config.el --- Org Capture Configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Customizations related to org-capture and org-refile is here.
;; This includes 'cj/org-webpage-clipper' functionality.

;; To ensure the code below is only loaded after org-mode, all code is wrapped in an
;; eval-after-load function.


;;; Code:


(with-eval-after-load 'org

  ;; ---------------------- Org-Webpage-Clipper ----------------------
  ;; Saves a copy of the page eww is visiting in the 'articles'-file for offline
  ;; reading. In other words, it's a "Poor Man's Pocket/Instapaper"

  (defun cj/org-webpage-clipper ()
    "Capture a web page for later viewing in an org-file.
Encodes all links and marks that may interfere with org mode
display, then inserts the content into a file for later offline use.
This is meant to be used in coordination with an org-capture-template.

Example Template:

,@
(\"w\" \"Website\" plain (function cj/org-webpage-clipper)
\"* %a\\nArticle Link: %L\\nCaptured On: %U\\n\\n\" :immediate-finish t)
'@"
    (interactive)

    ;; Ensure valid major mode before encoding
    (cond
     ((eq major-mode 'w3m-mode)
      (org-w3m-copy-for-org-mode))
     ((eq major-mode 'eww-mode)
      (org-eww-copy-for-org-mode))
     (t
      (error "Not valid -- must be in w3m or eww mode")))

    ;; Check for full path to the archive file. Create missing directories.
    (unless (file-exists-p article-file)
      (let ((dir (file-name-directory article-file)))
        (unless (file-exists-p dir)
          (make-directory dir))))

    ;; Move to end of file and insert blank line for org-capture to add timestamp, etc.
    (find-file article-file)
    (goto-char (point-max))
    (insert "\n\n\n\n\n")

    ;; Insert the web content keeping our place.
    (save-excursion (yank))

    ;; Remove page info from kill ring. Also, fix the yank pointer.
    (setq kill-ring (cdr kill-ring))
    (setq kill-ring-yank-pointer kill-ring)

    ;; Final repositioning.
    (forward-line -1))

;;;; --------------------------------- Functions -------------------------------

  (defun org-capture-pdf-active-region ()
    "Capture the active region of the pdf-view buffer.
Intended to be called within an org capture template."
    (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
           (pdf-buf (get-buffer pdf-buf-name)))
      (if (buffer-live-p pdf-buf)
          (with-current-buffer pdf-buf
            (car (pdf-view-active-region-text)))
        (user-error "Buffer %S not alive" pdf-buf-name))))

;;;; --------------------------- Org-Capture Templates -------------------------

  ;; ORG-CAPTURE TEMPLATES
  (setq org-protocol-default-template-key "L")
  (setq org-capture-templates
        '(
          ("e" "Event" entry (file+headline schedule-file "Scheduled Events")
           "* %?\nWHEN: %^t" :prepend t)

          ("E" "Epub Text" entry (file+headline inbox-file "Inbox")
           "* %?\n#+BEGIN_QUOTE\n %i\n#+END_QUOTE\nSource: [[%:link][%(buffer-name (org-capture-get :original-buffer))]]\nCaptured On: %U" :prepend t)

          ("P" "PDF Text" entry (file+headline inbox-file "Inbox")
           "* %?\n#+BEGIN_QUOTE\n %(org-capture-pdf-active-region)\n#+END_QUOTE\nSource:[[%L][%(buffer-name (org-capture-get :original-buffer))]]\nCaptured On: %U" :prepend t)

          ("p" "Link with Selection" entry (file+headline inbox-file "Inbox")
           "* TODO %?\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n[[%:link][%:description]]\nCaptured On: %U" :prepend t)

          ("L" "Link" entry (file+headline inbox-file "Inbox")
           "* TODO %?\n[[%:link][%:description]]\nCaptured On: %U" :prepend t)

          ("m" "Mu4e Email" entry (file+headline inbox-file "Inbox")
           "* TODO %?%(if (string= \"%i\" \"\") \"\" \"\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\")\n[[%:link][%:description]]\nCaptured On: %U"
           :prepend t)

		  ("w" "Web Page Clipper" plain
		   (function cj/org-webpage-clipper)
           "* %a\nArticle Link: %L\nCaptured On: %U\n\n" :immediate-finish t)))

  ) ;; end with-eval-after-load 'org

(provide 'org-capture-config)
;;; org-capture-config.el ends here.
