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

(defvar org-capture-plist)
(defvar org-capture-templates)
(defvar org-complex-heading-regexp-format)

(declare-function org-at-encrypted-entry-p "org-crypt")
(declare-function org-at-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-capture-expand-file "org-capture")
(declare-function org-capture-get "org-capture")
(declare-function org-capture-put "org-capture")
(declare-function org-capture-put-target-region-and-position "org-capture")
(declare-function org-capture-target-buffer "org-capture")
(declare-function org-display-warning "org")
(declare-function org-get-heading "org")
(declare-function org-parse-time-string "org")
(declare-function pdf-view-active-region-text "pdf-view")

(defvar cj/org-capture--file-headline-target-cache (make-hash-table :test #'equal)
  "Cache Org capture file+headline target markers by expanded file and headline.")

(defun cj/org-capture-clear-target-cache ()
  "Clear cached Org capture target markers."
  (interactive)
  (clrhash cj/org-capture--file-headline-target-cache)
  (message "Cleared org-capture target cache"))

(defun cj/org-capture--file-headline-target-p (target)
  "Return non-nil when TARGET is an Org capture file+headline target."
  (pcase target
    (`(file+headline ,_path ,(and _headline (pred stringp))) t)
    (_ nil)))

(defun cj/org-capture--headline-marker-valid-p (marker headline)
  "Return non-nil when MARKER still points at HEADLINE."
  (and (markerp marker)
       (marker-buffer marker)
       (buffer-live-p (marker-buffer marker))
       (with-current-buffer (marker-buffer marker)
         (save-excursion
           (goto-char marker)
           (and (derived-mode-p 'org-mode)
                (org-at-heading-p)
                (string= (org-get-heading t t t t) headline))))))

(defun cj/org-capture--file-headline-cache-key (path headline)
  "Return the cache key for PATH and HEADLINE."
  (list (org-capture-expand-file path) headline))

(defun cj/org-capture--goto-file-headline (path headline)
  "Move to capture target PATH and HEADLINE, using a cached marker when valid.
This implements Org's `file+headline' target positioning behavior, but avoids
re-scanning large target files after the first successful lookup."
  (set-buffer (org-capture-target-buffer path))
  ;; Org expects the target file to be in Org mode, otherwise it throws an
  ;; error.  Match Org's stock file+headline behavior here.
  (unless (derived-mode-p 'org-mode)
    (org-display-warning
     (format "Capture requirement: switching buffer %S to Org mode"
             (current-buffer)))
    (org-mode))
  (org-capture-put-target-region-and-position)
  (widen)
  (let* ((key (list (expand-file-name (buffer-file-name)) headline))
         (marker (gethash key cj/org-capture--file-headline-target-cache)))
    (if (cj/org-capture--headline-marker-valid-p marker headline)
        (goto-char marker)
      (goto-char (point-min))
      (if (re-search-forward (format org-complex-heading-regexp-format
                                      (regexp-quote headline))
                              nil t)
          (forward-line 0)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " headline "\n")
        (forward-line -1))
      (puthash key (copy-marker (point))
               cj/org-capture--file-headline-target-cache))))

(defun cj/org-capture--set-file-headline-target-location (target)
  "Set Org capture target location for file+headline TARGET."
  (pcase target
    (`(file+headline ,path ,headline)
     (let ((target-entry-p t))
       (save-excursion
         (cj/org-capture--goto-file-headline path headline)
         (org-capture-put :buffer (current-buffer)
                          :pos (point)
                          :target-entry-p target-entry-p
                          :decrypted
                          (and (featurep 'org-crypt)
                               (org-at-encrypted-entry-p)
                               (save-excursion
                                 (org-decrypt-entry)
                                 (and (org-back-to-heading t) (point))))))))))

(defun cj/org-capture--set-target-location-advice (orig-fun &optional target)
  "Use cached target lookup around ORIG-FUN for file+headline capture targets."
  (let ((resolved-target (or target (org-capture-get :target))))
    (if (cj/org-capture--file-headline-target-p resolved-target)
        (cj/org-capture--set-file-headline-target-location resolved-target)
      (funcall orig-fun target))))

(with-eval-after-load 'org-capture
  (advice-add 'org-capture-set-target-location
              :around #'cj/org-capture--set-target-location-advice))

;; --------------------------- Org-Capture Templates ---------------------------
;; you can bring up the org capture menu with C-c c

(use-package org-protocol
  :ensure nil ;; built-in
  :after org
  :config

  ;; -------------------------- Event Capture Formatting -------------------------

  (defun cj/org-capture--date-prefix (timestamp)
    "Return \"YY-MM-DD: \" prefix from org TIMESTAMP string, or nil if unparseable."
    (when-let ((parsed (ignore-errors (org-parse-time-string timestamp))))
      (let ((year (nth 5 parsed))
            (month (nth 4 parsed))
            (day (nth 3 parsed)))
        (when (and year month day
                   (> year 0) (> month 0) (> day 0))
          (format "%02d-%02d-%02d: "
                  (mod year 100) month day)))))

  (defun cj/org-capture-format-event-headline ()
    "Format the event headline with YY-MM-DD prefix from the WHEN timestamp.
This function is called during `org-capture' finalization to prepend the date
to the event title for better organization in the schedule file."
    (when (string= (plist-get org-capture-plist :key) "e")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^WHEN: \\(<[^>]+>\\)" nil t)
          (let ((date-prefix (cj/org-capture--date-prefix (match-string 1))))
            (when date-prefix
              (goto-char (point-min))
              (when (looking-at "^\\(\\*+ \\)\\(.*\\)$")
                (replace-match (concat "\\1" date-prefix "\\2")))))))))

  (defun cj/org-capture-event-content ()
    "Get the appropriate content for event capture based on context.
Returns the selected text from either Emacs or browser (via org-protocol)
formatted appropriately for insertion into the capture template."
    (cond
     ;; If called from org-protocol (browser), get the initial from org-store-link-plist
     ((and (boundp 'org-store-link-plist)
           org-store-link-plist
           (let ((val (plist-get org-store-link-plist :initial)))
             (and (stringp val) (not (string-empty-p val)))))
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
** Answer\n%i\nSource: [[%:link][%:description]]
Captured On: %U" :prepend t)

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
