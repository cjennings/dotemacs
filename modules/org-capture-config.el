;;; org-capture-config.el --- Org Capture Configuration  -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Customizations related to org-capture and org-refile.
;; Includes capture templates for tasks, links, PDFs, EPUBs, emails, and drill questions.

;; bookmarklet code:
;; text
;; javascript:location.href='org-protocol://capture?template=L&url=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection())

;; text + selection
;; javascript:location.href='org-protocol://capture?template=p&url=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection())

;;; Code:

;; --------------------------- Org-Capture Templates ---------------------------
;; you can bring up the org capture menu with C-c c

(use-package org-protocol
  :ensure nil ;; built-in
  :after org
  :config

  ;; -------------------------- Event Capture Formatting -------------------------

  (defun cj/org-capture-format-event-headline ()
    "Format the event headline with YY-MM-DD prefix from the WHEN timestamp.
This function is called during `org-capture' finalization to prepend the date
to the event title for better organization in the schedule file."
    (when (string= (plist-get org-capture-plist :key) "e")
      (save-excursion
        (goto-char (point-min))
        ;; Find the WHEN: line with timestamp
        (when (re-search-forward "^WHEN: \\(<[^>]+>\\)" nil t)
          (let* ((timestamp (match-string 1))
                 ;; Parse the timestamp to extract date components
                 (parsed (org-parse-time-string timestamp))
                 (year (nth 5 parsed))
                 (month (nth 4 parsed))
                 (day (nth 3 parsed))
                 ;; Format as YY-MM-DD
                 (date-prefix (format "%02d-%02d-%02d: "
                                      (mod year 100) month day)))
            ;; Go back to the headline
            (goto-char (point-min))
            ;; Insert date prefix after the asterisks
            (when (looking-at "^\\(\\*+ \\)\\(.*\\)$")
              (replace-match (concat "\\1" date-prefix "\\2"))))))))

  (defun cj/org-capture-event-content ()
    "Get the appropriate content for event capture based on context.
Returns the selected text from either Emacs or browser (via org-protocol)
formatted appropriately for insertion into the capture template."
    (cond
     ;; If called from org-protocol (browser), get the initial from org-store-link-plist
     ((and (boundp 'org-store-link-plist)
           org-store-link-plist
           (plist-get org-store-link-plist :initial))
      (concat "\n" (plist-get org-store-link-plist :initial)))
     ;; If there's a selected region in Emacs, use it from capture plist
     ((and (stringp (plist-get org-capture-plist :initial))
           (not (string= (plist-get org-capture-plist :initial) "")))
      (concat "\n" (plist-get org-capture-plist :initial)))
     ;; Otherwise, return empty string
     (t "")))

  ;; ----------------------- Org Capture PDF Active Region -----------------------

  (defun cj/org-capture-pdf-active-region ()
    "Capture the active region of the pdf-view buffer.

Intended to be called within an org capture template."
    (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
           (pdf-buf (get-buffer pdf-buf-name)))
      (if (buffer-live-p pdf-buf)
          (with-current-buffer pdf-buf
            (car (pdf-view-active-region-text)))
        (user-error "Buffer %S not alive" pdf-buf-name))))

  ;; ORG-CAPTURE TEMPLATES
  (setq org-protocol-default-template-key "L")
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline inbox-file "Inbox")
           "* TODO %?" :prepend t)

          ("a" "Appointment" entry (file gcal-file)
           "* %?\n:PROPERTIES:\n:calendar-id:craigmartinjennings@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n"
           :jump-to-captured t)

          ("e" "Event" entry (file+headline schedule-file "Scheduled Events")
           "* %?%:description
SCHEDULED: %^t%(cj/org-capture-event-content)
Captured On: %U"
           :prepend t
           :prepare-finalize cj/org-capture-format-event-headline)

          ("E" "Epub Text" entry (file+headline inbox-file "Inbox")
           "* %?
#+BEGIN_QUOTE\n %i\n#+END_QUOTE
Source: [[%:link][%(buffer-name (org-capture-get :original-buffer))]]
Captured On: %U" :prepend t)

          ;; requires cj/org-capture-pdf-active-region function defined above
          ("P" "PDF Text" entry (file+headline inbox-file "Inbox")
           "* %?
#+BEGIN_QUOTE\n%(cj/org-capture-pdf-active-region)\n#+END_QUOTE
Source: [[%L][%(buffer-name (org-capture-get :original-buffer))]]
Captured On: %U" :prepend t)

          ("p" "Link with Selection" entry (file+headline inbox-file "Inbox")
           "* %?%:description
#+BEGIN_QUOTE\n%i\n#+END_QUOTE
[[%:link][%:description]]
Captured On: %U\n" :prepend t :immediate-finish t)

          ("L" "Link" entry (file+headline inbox-file "Inbox")
           "* %?%:description
[[%:link][%:description]]\nCaptured On: %U" :prepend t :immediate-finish t)

          ("m" "Mu4e Email" entry (file+headline inbox-file "Inbox")
           "* TODO %?
%(if (string= \"%i\" \"\") \"\" \"\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\")
[[%:link][%:description]]
Captured On: %U"
           :prepend t)

          ("d" "Drill Question" entry
           (file (lambda ()
                   (let ((files (directory-files drill-dir nil "^[^.].*\\.org$")))
                     (expand-file-name
                      (completing-read "Choose file: " files)
                      drill-dir))))
           "* Item   :drill:\n%?
** Answer\n%i\nSource: [[%:link][%:description]
nCaptured On: %U" :prepend t)

          ("f" "Drill Question (from PDF)" entry
           (file (lambda ()
                   (let ((files (directory-files drill-dir nil "^[^.].*\\.org$")))
                     (expand-file-name
                      (completing-read "Choose file: " files)
                      drill-dir))))
           "* Item   :drill:\n%?
** Answer\n%(cj/org-capture-pdf-active-region)
Source: [[%L][%(buffer-name (org-capture-get :original-buffer))]]
Captured On: %U" :prepend t)

          )) ;; end setq
  ) ;; end use-package org-protocol

(provide 'org-capture-config)
;;; org-capture-config.el ends here.
