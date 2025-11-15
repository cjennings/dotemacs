;;; org-agenda-config-debug.el --- Debug functions for org-agenda-config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; This file contains debug functions for org-agenda-config.el.
;; It is only loaded when cj/debug-modules includes 'org-agenda or is t.
;;
;; Enable with: (setq cj/debug-modules '(org-agenda))
;;          or: (setq cj/debug-modules t)
;;
;; Available debug functions:
;; - cj/org-agenda-debug-dump-files - Show all org-agenda-files with status
;; - cj/org-agenda-debug-rebuild-timing - Measure rebuild performance
;;
;;; Code:

(require 'user-constants)
(require 'system-lib)

;; ---------------------------- Debug Functions --------------------------------

;;;###autoload
(defun cj/org-agenda-debug-dump-files ()
  "Dump all org-agenda-files to *Messages* buffer with status.
Shows which files exist, which are missing, and their sizes."
  (interactive)
  (cj/log-silently "=== Org Agenda Debug: Files ===")
  (cj/log-silently "Total files: %d" (length org-agenda-files))
  (cj/log-silently "")
  (dolist (file org-agenda-files)
    (if (file-exists-p file)
        (let ((size (file-attribute-size (file-attributes file)))
              (mtime (format-time-string "%Y-%m-%d %H:%M:%S"
                                         (file-attribute-modification-time
                                          (file-attributes file)))))
          (cj/log-silently "✓ %s" file)
          (cj/log-silently "  Size: %d bytes, Modified: %s" size mtime))
      (cj/log-silently "✗ %s [MISSING]" file)))
  (cj/log-silently "")
  (cj/log-silently "=== End Org Agenda Debug ===")
  (message "Org Agenda: Dumped %d files to *Messages* buffer" (length org-agenda-files)))

;;;###autoload
(defun cj/org-agenda-debug-rebuild-timing ()
  "Measure and report timing for rebuilding org-agenda-files.
Runs cj/build-org-agenda-list and reports detailed timing."
  (interactive)
  (cj/log-silently "=== Org Agenda Debug: Rebuild Timing ===")
  (let ((start-time (current-time)))
    (cj/build-org-agenda-list)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (cj/log-silently "Rebuild completed in %.3f seconds" elapsed)
      (cj/log-silently "Files found: %d" (length org-agenda-files))
      (cj/log-silently "Average time per file: %.4f seconds"
                       (if (> (length org-agenda-files) 0)
                           (/ elapsed (float (length org-agenda-files)))
                         0.0))))
  (cj/log-silently "=== End Org Agenda Debug ===")
  (message "Org Agenda: Timing info dumped to *Messages* buffer"))

(provide 'org-agenda-config-debug)
;;; org-agenda-config-debug.el ends here
