;;; org-capture-config.el --- Org Capture Configuration  -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: capture is a daily hot path; org-protocol capture handlers must
;;   be registered for external capture to work.
;; Top-level side effects: capture templates and org-protocol handlers via
;;   use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
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

(declare-function cj/--drill-pick-file "org-drill-config")
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
(declare-function projectile-project-root "projectile" (&optional dir))
(defvar inbox-file)

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
  (let* ((key (cj/org-capture--file-headline-cache-key path headline))
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

;; ----------------------- Project-Aware Capture Target ------------------------
;; C-c c t (Task) and C-c c b (Bug) file into the current projectile project's
;; todo.org under its "... Open Work" heading.  Outside a project they fall back
;; to the global inbox; in a project with no todo.org they fall back to the
;; inbox with a warning (they never create a project's todo.org).

(defconst cj/--org-open-work-heading-regexp
  "^\\*[ \t]+.*Open Work\\(?:[ \t]+:[^\n]*:\\)?[ \t]*$"
  "Regexp matching a top-level \"... Open Work\" Org heading line.")

(defun cj/--org-capture-project-name (root)
  "Return a display project name for ROOT directory, or nil.
The basename of ROOT with a single leading dot stripped and the first
letter upcased: \"~/.emacs.d/\" -> \"Emacs.d\", \"~/code/duet/\" -> \"Duet\"."
  (when (and (stringp root) (not (string-empty-p root)))
    (let* ((base (file-name-nondirectory (directory-file-name root)))
           (clean (if (and (> (length base) 1) (eq ?. (aref base 0)))
                      (substring base 1)
                    base)))
      (and (not (string-empty-p clean))
           (concat (upcase (substring clean 0 1)) (substring clean 1))))))

(defun cj/--org-capture-project-target (root inbox)
  "Pure capture-target decision for project-aware capture.
ROOT is the projectile project root (or nil); INBOX is the global inbox
file path.  Return a plist (:file F :open-work BOOL :project NAME :warn MSG):
- ROOT with a todo.org -> F is that todo.org, :open-work t.
- ROOT without a todo.org -> F is INBOX, :open-work nil, :warn names the project.
- ROOT nil -> F is INBOX, :open-work nil, :warn nil."
  (if (and (stringp root) (not (string-empty-p root)))
      (let ((todo (expand-file-name "todo.org" root))
            (name (cj/--org-capture-project-name root)))
        (if (file-exists-p todo)
            (list :file todo :open-work t :project name :warn nil)
          (list :file inbox :open-work nil :project name
                :warn (format "No todo.org in project \"%s\"; captured to the inbox instead"
                              name))))
    (list :file inbox :open-work nil :project nil :warn nil)))

(defun cj/--org-capture-goto-open-work (project-name)
  "Move point to a top-level \"... Open Work\" heading in the current buffer.
Create \"* PROJECT-NAME Open Work\" at end of buffer when none exists.
Leave point at the start of the heading line."
  (goto-char (point-min))
  (if (re-search-forward cj/--org-open-work-heading-regexp nil t)
      (forward-line 0)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* %s Open Work\n" project-name))
    (forward-line -1)))

(defun cj/--org-capture-goto-exact-headline (headline)
  "Move point to the top-level HEADLINE in the current buffer.
Create \"* HEADLINE\" at end of buffer when absent.  Leave point at the
start of the heading line."
  (goto-char (point-min))
  (if (re-search-forward (format org-complex-heading-regexp-format
                                 (regexp-quote headline))
                         nil t)
      (forward-line 0)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " headline "\n")
    (forward-line -1)))

(defun cj/--org-capture-project-location ()
  "Org-capture `function' target for project-aware Task/Bug capture.
File into the current projectile project's todo.org under its \"... Open
Work\" heading, else the global inbox (`inbox-file') under \"Inbox\"."
  (let* ((root (and (fboundp 'projectile-project-root)
                    (ignore-errors (projectile-project-root))))
         (plan (cj/--org-capture-project-target root inbox-file)))
    (when (plist-get plan :warn)
      (message "%s" (plist-get plan :warn)))
    (set-buffer (org-capture-target-buffer (plist-get plan :file)))
    (unless (derived-mode-p 'org-mode) (org-mode))
    (org-capture-put-target-region-and-position)
    (widen)
    (if (plist-get plan :open-work)
        (cj/--org-capture-goto-open-work (plist-get plan :project))
      (cj/--org-capture-goto-exact-headline "Inbox"))))

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
        '(("t" "Task" entry (function cj/--org-capture-project-location)
           "* TODO %?" :prepend t)

          ("b" "Bug" entry (function cj/--org-capture-project-location)
           "* TODO [#C] %?" :prepend t)

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
           (file (lambda () (cj/--drill-pick-file drill-dir)))
           "* Item   :drill:\n%?
** Answer\n%i\nSource: [[%:link][%:description]]
Captured On: %U" :prepend t)

          ("f" "Drill Question (from PDF)" entry
           (file (lambda () (cj/--drill-pick-file drill-dir)))
           "* Item   :drill:\n%?
** Answer\n%(cj/org-capture-pdf-active-region)
Source: [[%L][%(buffer-name (org-capture-get :original-buffer))]]
Captured On: %U" :prepend t)

          )) ;; end setq
  ) ;; end use-package org-protocol

;; ---------------------- Popup Capture Frame Auto-Close ----------------------
;; The quick-capture script (Hyprland Super+Shift+N) opens an emacsclient
;; frame named "org-capture"; Hyprland window rules float and center it by
;; that name. These hooks close the frame when the capture finalizes or
;; aborts, so the popup never lingers. Frames not named "org-capture" are
;; untouched — normal in-Emacs captures keep their windows.

(defun cj/org-capture--delete-popup-frame ()
  "Delete the current frame when it is the quick-capture popup."
  (when (equal (frame-parameter nil 'name) "org-capture")
    (delete-frame)))

(add-hook 'org-capture-after-finalize-hook #'cj/org-capture--delete-popup-frame)

(provide 'org-capture-config)
;;; org-capture-config.el ends here.
