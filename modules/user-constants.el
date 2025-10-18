;;; user-constants.el --- User Constants -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:

;; This module defines important file and directory paths used throughout the
;; Emacs configuration, and ensures they exist during startup.
;;
;; WHY THIS EXISTS:
;; 1. Centralizes all path definitions for easy reference and maintenance
;; 2. Prevents startup errors when required directories or files are missing
;; 3. Makes the configuration more portable across different machines
;;
;; The module first defines constants and variables for directories and files,
;; then provides functions that verify their existence, creating them if needed.
;; This happens automatically when the module loads.
;;
;; The paths are designed with a hierarchical structure, allowing child paths
;; to reference their parents (e.g., roam-dir is inside org-dir) for better
;; maintainability.
;;
;;; Code:

;; -------------------------------- Contact Info -------------------------------

(defvar user-whole-name "Craig Jennings"
  "The user's full name.")
(defconst user-name (getenv "USER")
  "The user's name retrieved from the environment variable.")
(defvar user-mail-address "c@cjennings.net"
  "The user's email address.")

;; ------------------------ Directory And File Constants -----------------------

;; DIRECTORIES
(defconst emacs-init-file (expand-file-name "init.el" user-emacs-directory)
  "The location of Emacs's main init file.")

(defconst emacs-early-init-file (expand-file-name "early-init.el" user-emacs-directory)
  "The location of Emacs's early init file.")

(defconst user-home-dir (getenv "HOME")
  "The user's home directory per the environment variable.")

(defconst books-dir (expand-file-name "sync/books/" user-home-dir)
  "The location of book files for CalibreDB.")

(defconst code-dir (expand-file-name "code/" user-home-dir)
  "Code repositories are located in this directory.")

(defconst dl-dir (expand-file-name "downloads/" user-home-dir)
  "Location of the general downloads directory.")

(defconst pix-dir (expand-file-name "pictures/" user-home-dir)
  "Location of where pictures and images are stored.")

(defconst projects-dir (expand-file-name "projects/" user-home-dir)
  "Non-code projects and repositories are located in this directory.")

(defconst videos-dir (expand-file-name "videos/" user-home-dir)
  "Location of where videos are stored.")

(defconst mail-dir (expand-file-name ".mail/" user-home-dir)
  "Root directory where the mail folders are located.")

(defconst sync-dir (expand-file-name "sync/" user-home-dir)
  "This directory is synchronized across machines.")

(defconst org-dir (expand-file-name "org/" sync-dir)
  "This directory is synchronized across machines.")

(defconst roam-dir (expand-file-name "roam/" org-dir)
  "The location of org-roam files.")

(defconst journals-dir (expand-file-name "journal/" roam-dir)
  "The location of org-roam dailies or journals files.")

(defconst drill-dir (expand-file-name "drill/" org-dir)
  "The location of org-drill org files.")

(defconst snippets-dir (expand-file-name "snippets/" org-dir)
  "The location of ya-snippet snippets.")

(defvar sounds-dir (expand-file-name "assets/sounds/" user-emacs-directory)
  "Directory containing sound files for notifications and timers.")

(defconst video-recordings-dir (expand-file-name "sync/recordings/" user-home-dir)
  "The location to save video recordings.")

(defconst audio-recordings-dir (expand-file-name "sync/recordings/" user-home-dir)
  "The location to save audio recordings.")

(defconst music-dir (expand-file-name "music/" user-home-dir)
  "The location to save your music files.")


;; FILES
(defvar authinfo-file (expand-file-name ".authinfo.gpg" user-home-dir)
  "The location of the encrypted .authinfo or .netrc file.")

(defvar schedule-file (expand-file-name "schedule.org" org-dir)
  "The location of the org file containing scheduled events.")

(defvar gcal-file (expand-file-name "gcal.org" org-dir)
  "The location of the org file containing Google Calendar information.")

(defvar reference-file (expand-file-name "reference.org" org-dir)
  "The location of the org file containing reference information.")

(defvar article-archive (expand-file-name "article-archive.org" org-dir)
  "The location of the org file that stores saved articles to keep.")

(defvar inbox-file (expand-file-name "inbox.org" roam-dir)
  "The location of the org file that serves as the task inbox.")

(defvar reading-notes-file (expand-file-name "reading_notes.org" roam-dir)
  "The default notes file for org-noter.")

(defvar macros-file (concat org-dir "macros.el")
  "The location of the macros file for recorded saved macros via M-f3.")

(defvar contacts-file (expand-file-name "contacts.org" org-dir)
  "The location of the org file containing contact information.")

(defvar notification-sound (expand-file-name "BitWave.opus" sounds-dir)
  "The location of the audio file to use as the default notification.")

(defvar webclipped-file (expand-file-name "webclipped.org" org-dir)
  "The location of the org file that keeps webclips to read.

For more information, see org webclipper section of org-capture-config.el")

;; ------------------------- Verify Or Create Functions ------------------------

(defun cj/directory-writable-p (dir)
  "Check if DIR is writable."
  (and (file-directory-p dir)
       (file-writable-p dir)))

(defun cj/verify-or-create-dir (dir)
  "Verify the directory DIR exists; create it if it doesn't."
  (condition-case err
      (unless (file-directory-p dir)
        (make-directory dir t)
        (message "Created directory: %s" dir))
    (error (message "Error creating directory %s: %s" dir (error-message-string err)))))

(defun cj/verify-or-create-file (file)
  "Verify the file FILE exists; create it if it doesn't."
  (condition-case err
      (let ((dir (file-name-directory file)))
        (when dir (cj/verify-or-create-dir dir))
        (unless (file-exists-p file)
          (write-region "" nil file)
          (message "Created file: %s" file)))
    (error (message "Error creating file %s: %s" file (error-message-string err)))))

(defun cj/initialize-user-directories-and-files ()
  "Initialize all necessary directories and files.

This ensures that all directories and files required by the Emacs configuration
exist, creating them if necessary. This makes the configuration more robust
and portable across different machines."
  (interactive)
  (mapc 'cj/verify-or-create-dir (list sync-dir
									   drill-dir
									   journals-dir
									   roam-dir
									   snippets-dir
									   video-recordings-dir
									   audio-recordings-dir
									   org-dir))
 (mapc 'cj/verify-or-create-file (list schedule-file
                                        inbox-file
										article-archive
										reading-notes-file
										contacts-file
										webclipped-file
                                        reference-file)))

;; Initialize directories and files when this module is loaded
(cj/initialize-user-directories-and-files)

(provide 'user-constants)
;;; user-constants.el ends here
