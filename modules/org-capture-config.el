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
(declare-function cj/org-capture--date-prefix "org-capture-config")
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

(defun cj/--org-find-or-create-top-heading (search-regexp heading-line)
  "Move point to the top-level heading matched by SEARCH-REGEXP in this buffer.
Search from the start of the buffer; on a match leave point at the start of
that heading line.  With no match, append HEADING-LINE (a full \"* ...\" line,
without a trailing newline) at the end of the buffer and leave point on it.
Returns point."
  (goto-char (point-min))
  (if (re-search-forward search-regexp nil t)
      (forward-line 0)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert heading-line "\n")
    (forward-line -1))
  (point))

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
      (cj/--org-find-or-create-top-heading
       (format org-complex-heading-regexp-format (regexp-quote headline))
       (concat "* " headline))
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

;; --------------------------- Capture F-Key Guard -----------------------------
;; The global popup keys (F1 dashboard sweep, F10 music, F11 dirvish-side,
;; F12 terminal, M-SPC agent swap) fire even while a capture is in progress
;; and pop their UI over the capture popup.  org-capture-mode is a minor
;; mode active exactly for the capture's duration and its keymap shadows
;; the global map, so blocking the keys there scopes the guard precisely:
;; the moment the capture finalizes or aborts, the keys work again.

(defun cj/--org-capture-blocked-key ()
  "Refuse a global popup key while a capture is in progress."
  (interactive)
  (user-error "Key disabled during capture -- finalize with C-c C-c or abort with C-c C-k"))

(with-eval-after-load 'org-capture
  (dolist (key '("<f1>" "<f10>" "<f11>" "<f12>" "M-SPC"))
    (keymap-set org-capture-mode-map key #'cj/--org-capture-blocked-key)))

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
- ROOT without a todo.org -> F is INBOX, :open-work nil, :warn names project.
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
  (cj/--org-find-or-create-top-heading
   cj/--org-open-work-heading-regexp
   (format "* %s Open Work" project-name)))

(defun cj/--org-capture-goto-exact-headline (headline)
  "Move point to the top-level HEADLINE in the current buffer.
Create \"* HEADLINE\" at end of buffer when absent.  Leave point at the
start of the heading line."
  (cj/--org-find-or-create-top-heading
   (format org-complex-heading-regexp-format (regexp-quote headline))
   (concat "* " headline)))

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
;; The quick-capture script (Hyprland Super+N) opens an emacsclient
;; frame named "org-capture"; Hyprland window rules float and center it by
;; that name. These hooks close the frame when the capture finalizes or
;; aborts, so the popup never lingers. Frames not named "org-capture" are
;; untouched — normal in-Emacs captures keep their windows.

(defun cj/org-capture--frame-reapable-p (frame-name buffer-names)
  "Non-nil when a frame named FRAME-NAME showing BUFFER-NAMES is a reapable popup.
Reapable means the quick-capture popup (FRAME-NAME equal to \"org-capture\") with
no capture UI left in any window — no *Org Select* menu and no CAPTURE-* buffer.
A popup still mid-capture has capture UI and is not reapable, so it is spared."
  (and (equal frame-name "org-capture")
       (not (seq-some (lambda (b)
                        (cj/org-capture--popup-sole-window-p frame-name b))
                      buffer-names))))

(defun cj/org-capture-reap-popup-frames ()
  "Delete every quick-capture popup frame that no longer shows capture UI.
Reaps across ALL frames, not just the selected one: a capture that finalizes,
aborts, or errors while the daemon's selected frame is something else (the common
multi-frame case) still cleans up its \"org-capture\" popup, while a popup
mid-capture is spared.  Never deletes the last remaining frame.  Safe to call
anytime — bound to nothing, run via M-x when a stray popup needs clearing."
  (interactive)
  (dolist (f (frame-list))
    (when (and (frame-live-p f)
               (cdr (frame-list))           ; never delete the last frame
               (cj/org-capture--frame-reapable-p
                (frame-parameter f 'name)
                (mapcar (lambda (w) (buffer-name (window-buffer w)))
                        (window-list f 'no-minibuf))))
      (delete-frame f))))

;; Reap on every capture exit.  `remove-hook' first so a live module reload swaps
;; the retired narrow (selected-frame) handler for this one without leaving both.
(remove-hook 'org-capture-after-finalize-hook #'cj/org-capture--delete-popup-frame)
(add-hook 'org-capture-after-finalize-hook #'cj/org-capture-reap-popup-frames)

;; The popup opens a fresh emacsclient frame still showing the daemon's last
;; buffer.  `org-mks' shows the *Org Select* menu via
;; `switch-to-buffer-other-window', and `org-capture-place-template' shows the
;; CAPTURE-* buffer via `pop-to-buffer' with a split action — both split the
;; small floating frame, so two reverse-video modelines read like tmux bars and
;; the working buffer leaks into a popup that should only show capture UI.  A
;; frame-scoped `display-buffer-alist' entry forces both into the frame's sole
;; window.  Gated on the "org-capture" frame name, so normal in-Emacs captures
;; keep their windows.

(defun cj/org-capture--popup-sole-window-p (frame-name buffer-name)
  "Return non-nil when BUFFER-NAME in a frame named FRAME-NAME is capture popup UI.
Capture popup UI is the *Org Select* template menu or a CAPTURE-* buffer
shown in the quick-capture frame (FRAME-NAME equal to \"org-capture\")."
  (and (equal frame-name "org-capture")
       (stringp buffer-name)
       (or (equal buffer-name "*Org Select*")
           (string-prefix-p "CAPTURE-" buffer-name))))

(defun cj/org-capture--popup-display-condition (buffer-name &optional _action)
  "`display-buffer' CONDITION matching capture UI in the quick-capture popup.
BUFFER-NAME is the buffer's name; the selected frame supplies the frame name."
  (cj/org-capture--popup-sole-window-p (frame-parameter nil 'name) buffer-name))

(defun cj/org-capture--display-sole-window (buffer _alist)
  "`display-buffer' ACTION showing BUFFER as the only window of the frame.
Used for the quick-capture popup so the template menu and capture buffer
never split the small floating frame."
  (let ((window (frame-root-window)))
    (delete-other-windows window)
    (set-window-buffer window buffer)
    window))

(add-to-list 'display-buffer-alist
             '(cj/org-capture--popup-display-condition
               cj/org-capture--display-sole-window))

;; A fresh "org-capture" popup opens showing the daemon's last buffer (see the
;; comment above), and only the capture UI + the reap-on-finalize hook clear it.
;; If a capture aborts before its UI paints (a C-g, an erroring template, a path
;; that skips `cj/quick-capture'), the popup lingers showing whatever was current
;; -- and if that was a live terminal (an eat/vterm Claude Code buffer), eat
;; sizes the terminal to that small popup window and clamps the real frame down
;; to the popup's rows.  These two guards keep the popup from ever holding a
;; size-sensitive live buffer: it only ever shows capture UI or *scratch*.

(defun cj/org-capture--neutralize-frame (frame)
  "Point every non-capture-UI window of the \"org-capture\" popup FRAME at
*scratch*.  Capture UI (the *Org Select* menu, a CAPTURE-* buffer) is spared, so
this never disturbs a live capture; it only evicts a stray live buffer (the
daemon's last buffer on open, or a buffer restored on abort) that would
otherwise mirror the popup's size onto its source buffer.  Idempotent: a window
already on *scratch* is left alone, so it can't loop through the
`window-buffer-change-functions' it fires."
  (when (and (frame-live-p frame)
             (equal (frame-parameter frame 'name) "org-capture"))
    (dolist (w (window-list frame 'no-minibuf))
      (let ((name (buffer-name (window-buffer w))))
        (unless (or (cj/org-capture--popup-sole-window-p "org-capture" name)
                    (equal name "*scratch*"))
          (set-window-buffer w (get-buffer-create "*scratch*")))))))

;; Guard 1 (root cause): neutralize the popup the instant it is created, before
;; any capture UI paints, so it never opens mirroring the daemon's last buffer.
(defun cj/org-capture--neutralize-new-frame (frame)
  "Neutralize a freshly-made \"org-capture\" popup FRAME on creation.
See `cj/org-capture--neutralize-frame'."
  (cj/org-capture--neutralize-frame frame))

(add-hook 'after-make-frame-functions #'cj/org-capture--neutralize-new-frame)

;; Guard 2 (safety net): catch any path the finalize reap misses.  If a live
;; buffer is displayed in the popup after creation (an aborted capture restoring
;; the previous buffer, a stray `switch-to-buffer'), evict it at once.
(defun cj/org-capture--neutralize-on-buffer-change (frame-or-window)
  "Neutralize the \"org-capture\" popup after any buffer change in FRAME-OR-WINDOW.
`window-buffer-change-functions' passes a frame (global hook) or a window
(buffer-local); handle both."
  (let ((frame (if (windowp frame-or-window)
                   (window-frame frame-or-window)
                 frame-or-window)))
    (cj/org-capture--neutralize-frame frame)))

(add-hook 'window-buffer-change-functions
          #'cj/org-capture--neutralize-on-buffer-change)

;; The desktop quick-capture popup is launched globally (no browser selection,
;; no mu4e message, no pdf/epub buffer), so the context-dependent templates make
;; no sense there.  `cj/quick-capture' captures a single Task straight into the
;; global inbox -- no template menu -- under its "Inbox" headline, since a
;; desktop capture has no meaningful project context.  It closes the popup frame
;; on every exit path (abort, error, finalize): `org-capture' runs
;; `org-capture-after-finalize-hook' only on a completed capture, so a C-g or an
;; erroring template would otherwise orphan the frame.  The Hyprland script
;; calls this instead of `org-capture'.

(defun cj/--quick-capture-template (inbox)
  "Return the desktop quick-capture template: a single Task into INBOX's Inbox.
INBOX is the inbox file path; the Task files under its \"Inbox\" headline."
  (list (list "t" "Task" 'entry
              (list 'file+headline inbox "Inbox")
              "* TODO %?" :prepend t)))

(defun cj/org-capture--popup-frame ()
  "Return a live frame named \"org-capture\" (the quick-capture popup), or nil."
  (seq-find (lambda (f)
              (and (frame-live-p f)
                   (equal (frame-parameter f 'name) "org-capture")))
            (frame-list)))

(defun cj/quick-capture ()
  "Org-capture entry point for the Hyprland desktop popup (frame \"org-capture\").
Captures a single Task into the global inbox, with no template menu.
Closes the popup frame on abort or error so a stray launch never orphans it.

Selects the \"org-capture\" frame by name before capturing rather than trusting
the ambient selected frame: the launching =emacsclient -c -e= runs before
Hyprland settles focus on the new float, so =(selected-frame)= is still the
daemon's main frame and the capture would otherwise land there."
  (interactive)
  (let ((frame (cj/org-capture--popup-frame)))
    (condition-case err
        (progn
          (when frame (select-frame-set-input-focus frame))
          (let ((org-capture-templates (cj/--quick-capture-template inbox-file)))
            (org-capture nil "t")))
      (quit (cj/org-capture-reap-popup-frames))
      (error (message "Quick-capture: %s" (error-message-string err))
             (cj/org-capture-reap-popup-frames)))))

(provide 'org-capture-config)
;;; org-capture-config.el ends here.
