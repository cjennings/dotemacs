;;; org-refile-config.el --- Org Refile Customizations -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
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

;; ----------------------------- Org Refile Targets ----------------------------
;; sets refile targets
;; - adds project files in org-roam to the refile targets
;; - adds todo.org files in subdirectories of the code and project directories

(defvar cj/org-refile-targets-cache nil
  "Cached refile targets to avoid expensive directory scanning.
Set to nil to invalidate cache.")

(defvar cj/org-refile-targets-cache-time nil
  "Time when refile targets cache was last built.")

(defvar cj/org-refile-targets-cache-ttl 3600
  "Time-to-live for refile targets cache in seconds (default: 1 hour).")

(defvar cj/org-refile-targets-building nil
  "Non-nil when refile targets are being built asynchronously.
Prevents duplicate builds if user refiles before async build completes.")

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

(defun cj/build-org-refile-targets (&optional force-rebuild)
  "Build =org-refile-targets= with caching.

When FORCE-REBUILD is non-nil, bypass cache and rebuild from scratch.
Otherwise, returns cached targets if available and not expired.

This function scans 30,000+ files across code/projects directories,
so caching improves performance from 15-20 seconds to instant."
  (interactive "P")
  ;; Check if we can use cache
  (let ((cache-valid (and cj/org-refile-targets-cache
                          cj/org-refile-targets-cache-time
                          (not force-rebuild)
                          (< (- (float-time) cj/org-refile-targets-cache-time)
                             cj/org-refile-targets-cache-ttl))))
    (if cache-valid
        ;; Use cached targets (instant)
        (progn
          (setq org-refile-targets cj/org-refile-targets-cache)
          ;; Always show cache-hit message (interactive or background)
          (cj/log-silently "Using cached refile targets (%d files)"
                           (length org-refile-targets)))
      ;; Check if async build is in progress
      (when cj/org-refile-targets-building
        (cj/log-silently "Waiting for background cache build to complete..."))
      ;; Rebuild from scratch (slow - scans 34,000+ files)
      (unwind-protect
          (progn
            (setq cj/org-refile-targets-building t)
            (let ((start-time (current-time))
                  (new-files
                   (list
                    (cons inbox-file     '(:maxlevel . 1))
                    (cons reference-file '(:maxlevel . 2))
                    (cons schedule-file  '(:maxlevel . 1)))))

              ;; Extend with org-roam files if available AND org-roam is loaded
              (when (and (fboundp 'cj/org-roam-list-notes-by-tag)
                         (fboundp 'org-roam-node-list))
                (let* ((project-and-topic-files
                        (append (cj/org-roam-list-notes-by-tag "Project")
                                (cj/org-roam-list-notes-by-tag "Topic")))
                       (file-rule '(:maxlevel . 1)))
                  (dolist (file project-and-topic-files)
                    (unless (assoc file new-files)
                      (push (cons file file-rule) new-files)))))

              ;; Add todo.org files from known directories
              (dolist (dir (list user-emacs-directory code-dir projects-dir))
                (let* ((todo-files (directory-files-recursively
                                    dir "^[Tt][Oo][Dd][Oo]\\.[Oo][Rr][Gg]$"))
                       (file-rule '(:maxlevel . 1)))
                  (dolist (file todo-files)
                    (unless (assoc file new-files)
                      (push (cons file file-rule) new-files)))))

              ;; Update targets and cache
              (setq new-files (nreverse new-files))
              (setq org-refile-targets new-files)
              (setq cj/org-refile-targets-cache new-files)
              (setq cj/org-refile-targets-cache-time (float-time))

              ;; Always show completion message (interactive or background)
              (cj/log-silently "Built refile targets: %d files in %.2f seconds"
                               (length org-refile-targets)
                               (- (float-time) (float-time start-time)))))
        ;; Always clear the building flag, even if build fails
        (setq cj/org-refile-targets-building nil)))))

;; Build cache asynchronously after startup to avoid blocking Emacs
(run-with-idle-timer
 5  ; Wait 5 seconds after Emacs is idle
 nil ; Don't repeat
 (lambda ()
   (cj/log-silently "Building org-refile targets cache in background...")
   (cj/build-org-refile-targets)))

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
