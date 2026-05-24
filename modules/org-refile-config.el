;;; org-refile-config.el --- Org Refile Customizations -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/S.
;; Load shape: eager.
;; Eager reason: daily refile workflow; the user expects refile targets ready at
;;   the first session.
;; Top-level side effects: an idle timer that builds the refile-target cache
;;   (guarded; spec tracks the cache lifecycle).
;; Runtime requires: system-lib, cj-cache-lib.
;; Direct test load: yes.
;;
;; Configuration and custom functions for org-mode refiling.
;;
;; Performance:
;; - Caches refile targets to avoid scanning 34,000+ files on every refile
;; - Cache builds asynchronously 5 seconds after Emacs startup (non-blocking)
;; - First refile uses cache if ready, otherwise builds synchronously (one-time delay)
;; - Subsequent refiles are instant (cached)
;; - Cache auto-refreshes after 1 hour
;; - Manual refresh: M-x cj/org-refile-refresh-targets (e.g., after adding projects)

;;; Code:

(require 'system-lib)
(require 'cj-cache-lib)

;; Forward-declare org-refile's dynamic var so byte-compiled code treats our
;; `let'/`setq' on it as dynamic.  Without this, compiling the module turns
;; `cj/org-refile-in-file's (let ((org-refile-targets ...)) ...) into a
;; lexical binding that never reaches `org-refile', silently breaking
;; in-file refiling under `make compile'.
(defvar org-refile-targets)

;; ----------------------------- Org Refile Targets ----------------------------
;; sets refile targets
;; - adds project files in org-roam to the refile targets
;; - adds todo.org files in subdirectories of the code and project directories

(defvar cj/--org-refile-targets-cache (cj/cache-make :ttl 3600)
  "Cache state for the refile targets list.  See `cj-cache.el'.")

(defun cj/org-refile-ensure-org-mode (file)
  "Ensure FILE is a .org file and its buffer is in org-mode.
Returns the buffer visiting FILE, switching it to org-mode if needed.
Signals an error if FILE doesn't have a .org extension.

This prevents issues where:
1. Buffers get stuck in fundamental-mode (e.g., opened before org loaded)
2. Non-.org files are accidentally added to refile targets"
  (unless (string-match-p "\\.org\\'" file)
    (error "Refile target \"%s\" is not a .org file" file))

  (let ((buf (org-get-agenda-file-buffer file)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (cj/log-silently "Switching %s to org-mode (was in %s)"
                         (buffer-name) major-mode)
        (org-mode)))
    buf))

(defun cj/--org-refile-scan-dir (dir)
  "Return the todo.org files under DIR, or nil with a warning if unusable.
A missing, unreadable, or permission-denied DIR is non-fatal: it logs a
`display-warning' and returns nil so the rest of the refile-target scan
continues, rather than silently swallowing the failure or crashing the
whole scan on a missing directory."
  (cond
   ((not (file-directory-p dir))
    (display-warning 'org-refile
                     (format "Refile scan: directory missing, skipped: %s" dir)
                     :warning)
    nil)
   ((not (file-readable-p dir))
    (display-warning 'org-refile
                     (format "Refile scan: directory unreadable, skipped: %s" dir)
                     :warning)
    nil)
   (t
    (condition-case _
        (directory-files-recursively
         dir "^[Tt][Oo][Dd][Oo]\\.[Oo][Rr][Gg]$" nil
         (lambda (d) (not (string-match-p "airootfs" d))))
      (permission-denied
       (display-warning 'org-refile
                        (format "Refile scan: permission denied, skipped: %s" dir)
                        :warning)
       nil)))))

(defun cj/--org-refile-scan-targets ()
  "Scan disk for the refile-targets list.  Pure-ish: no caching, no logging.
Returns the list to assign to `org-refile-targets'.  Slow -- walks
30,000+ files across `code-dir' and `projects-dir'."
  (let ((new-files
         (list
          (cons inbox-file     '(:maxlevel . 1))
          (cons reference-file '(:maxlevel . 2))
          (cons schedule-file  '(:maxlevel . 1)))))
    (when (and (fboundp 'cj/org-roam-list-notes-by-tag)
               (fboundp 'org-roam-node-list))
      (let* ((project-and-topic-files
              (append (cj/org-roam-list-notes-by-tag "Project")
                      (cj/org-roam-list-notes-by-tag "Topic")))
             (file-rule '(:maxlevel . 1)))
        (dolist (file project-and-topic-files)
          (unless (assoc file new-files)
            (push (cons file file-rule) new-files)))))
    (let ((file-rule '(:maxlevel . 1)))
      (dolist (dir (list user-emacs-directory code-dir projects-dir))
        (dolist (file (cj/--org-refile-scan-dir dir))
          (unless (assoc file new-files)
            (push (cons file file-rule) new-files)))))
    (nreverse new-files)))

(defun cj/build-org-refile-targets (&optional force-rebuild)
  "Build =org-refile-targets= with caching.

When FORCE-REBUILD is non-nil, bypass cache and rebuild from scratch.
Otherwise, returns cached targets if available and not expired.

This function scans 30,000+ files across code/projects directories,
so caching improves performance from 15-20 seconds to instant."
  (interactive "P")
  (when (cj/cache-building-p cj/--org-refile-targets-cache)
    (cj/log-silently "Waiting for background cache build to complete..."))
  (let* ((start-time (current-time))
         (targets
          (cj/cache-value-or-rebuild
           cj/--org-refile-targets-cache
           #'cj/--org-refile-scan-targets
           :force-rebuild force-rebuild
           :on-hit (lambda (v)
                     (cj/log-silently "Using cached refile targets (%d files)"
                                      (length v)))
           :on-build-success
           (lambda (v)
             (cj/log-silently "Built refile targets: %d files in %.2f seconds"
                              (length v)
                              (- (float-time) (float-time start-time)))))))
    (setq org-refile-targets targets)))

;; Build cache asynchronously after startup to avoid blocking Emacs.
(unless noninteractive
  (run-with-idle-timer
   5  ; Wait 5 seconds after Emacs is idle
   nil ; Don't repeat
   (lambda ()
     (cj/log-silently "Building org-refile targets cache in background...")
     (cj/build-org-refile-targets))))

(defun cj/org-refile-refresh-targets ()
  "Force rebuild of refile targets cache.

Use this after adding new projects or todo.org files.
Bypasses cache and scans all directories from scratch."
  (interactive)
  (cj/build-org-refile-targets 'force-rebuild))

(defun cj/org-refile (&optional ARG DEFAULT-BUFFER RFLOC MSG)
  "Call org-refile with cached refile targets.

Uses cached targets for performance (instant vs 15-20 seconds).
Cache auto-refreshes after 1 hour or on Emacs restart.

To manually refresh cache (e.g., after adding projects):
  M-x cj/org-refile-refresh-targets

ARG DEFAULT-BUFFER RFLOC and MSG parameters passed to org-refile."
  (interactive "P")
  ;; Use cached targets (don't rebuild every time!)
  (cj/build-org-refile-targets)
  (org-refile ARG DEFAULT-BUFFER RFLOC MSG))

;; ----------------------------- Org Refile In File ----------------------------
;; convenience function for scoping the refile candidates to the current buffer.

(defun cj/org-refile-in-file ()
  "Refile to a target within the current file and save the buffer."
  (interactive)
  (let ((org-refile-targets `(((,(buffer-file-name)) :maxlevel . 6))))
	(call-interactively 'org-refile)
	(save-buffer)))


;; --------------------------------- Org Refile --------------------------------

(use-package org-refile
  :ensure nil ;; built-in
  :defer .5
  :bind
  (:map org-mode-map
		("C-c C-w"   . cj/org-refile)
		("C-c w"     . cj/org-refile-in-file))
  :config
  ;; save all open org buffers after a refile is complete
  (advice-add 'org-refile :after
			  (lambda (&rest _)
				(org-save-all-org-buffers)))

  ;; Ensure refile target buffers are in org-mode before processing
  ;; Fixes issue where buffers opened before org loaded get stuck in fundamental-mode
  (advice-add 'org-refile-get-targets :before
              (lambda (&rest _)
                "Ensure all refile target buffers are in org-mode."
                (dolist (target org-refile-targets)
                  (let ((file (car target)))
                    (when (stringp file)
                      (cj/org-refile-ensure-org-mode file)))))))

(provide 'org-refile-config)
;;; org-refile-config.el ends here.
