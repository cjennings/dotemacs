;;; calendar-sync-org.el --- Org rendering and atomic file output -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Created: 2025-11-16

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D.
;; Load shape: library.
;; Top-level side effects: none (defuns only).
;; Runtime requires: subr-x, cj-org-text-lib, calendar-sync-ics.
;; Direct test load: yes (requires calendar-sync-ics explicitly).
;;
;; Output layer of the calendar-sync parser: render a parsed event plist
;; into an Org entry (heading, property drawer, body) and write generated
;; content to disk atomically via a same-directory temp file plus rename,
;; so a reader never sees a half-written calendar.

;;; Code:

(require 'subr-x)
(require 'cj-org-text-lib)
(require 'calendar-sync-ics)

;;; Org Rendering

(defun calendar-sync--event-to-org (event)
  "Convert parsed EVENT plist to org entry string.
Produces property drawer with LOCATION, ORGANIZER, STATUS, URL when present.
Description appears as body text after the drawer."
  (let* ((summary (cj/org-sanitize-heading
                   (or (plist-get event :summary) "(No Title)")))
         (description (plist-get event :description))
         (location (plist-get event :location))
         (start (plist-get event :start))
         (end (plist-get event :end))
         (organizer (plist-get event :organizer))
         (status (plist-get event :status))
         (url (plist-get event :url))
         (timestamp (calendar-sync--format-timestamp start end))
         ;; Build property drawer entries
         (props '()))
    ;; Collect non-nil properties
    (when (and location (not (string-empty-p location)))
      (push (format ":LOCATION: %s"
                    (cj/org-sanitize-property-value location))
            props))
    (when organizer
      (let ((org-name (or (plist-get organizer :cn)
                          (plist-get organizer :email))))
        (when org-name
          (push (format ":ORGANIZER: %s"
                        (cj/org-sanitize-property-value org-name))
                props))))
    (when (and status (not (string-empty-p status)))
      (push (format ":STATUS: %s"
                    (cj/org-sanitize-property-value status))
            props))
    (when (and url (not (string-empty-p url)))
      (push (format ":URL: %s"
                    (cj/org-sanitize-property-value url))
            props))
    (setq props (nreverse props))
    ;; Build output
    (let ((parts (list timestamp (format "* %s" summary))))
      ;; Add property drawer if any properties exist
      (when props
        (push ":PROPERTIES:" parts)
        (dolist (prop props)
          (push prop parts))
        (push ":END:" parts))
      ;; Add description as body text (sanitized to prevent org heading conflicts)
      (when (and description (not (string-empty-p description)))
        (push (cj/org-sanitize-body-text description) parts))
      (string-join (nreverse parts) "\n"))))

;;; Atomic File Output

(defun calendar-sync--write-file (content file)
  "Write CONTENT to FILE atomically.
Creates parent directories if needed, then writes a temp file in the same
directory and renames it into place, so org-agenda or chime reading mid-write
never sees a half-written calendar."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((tmp (make-temp-file (expand-file-name ".calendar-sync-" dir))))
      (with-temp-file tmp
        (insert content))
      (rename-file tmp file t))))

(provide 'calendar-sync-org)
;;; calendar-sync-org.el ends here
