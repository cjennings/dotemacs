;;; user-constants.el --- User Constants -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Layer: 1 (Foundation).
;; Category: F.
;; Load shape: eager.
;; Eager reason: defines the path constants referenced across the config; other
;;   modules read them at their own load time.
;; Top-level side effects: none — only path definitions. Filesystem creation
;;   lives in `cj/initialize-user-directories-and-files', which init.el calls on
;;   real startup (not at module load), so a bare require is side-effect-free.
;; Runtime requires: none.
;; Direct test load: yes.
;;
;; This module defines important file and directory paths used throughout the
;; Emacs configuration, and provides a command to create them on startup.
;;
;; WHY THIS EXISTS:
;; 1. Centralizes all path definitions for easy reference and maintenance
;; 2. Prevents startup errors when required directories or files are missing
;; 3. Makes the configuration more portable across different machines
;;
;; The module first defines constants and variables for directories and files,
;; then provides functions that verify their existence, creating them if needed.
;; init.el calls `cj/initialize-user-directories-and-files' after requiring this
;; module so the paths exist before the modules that depend on them load.
;;
;; The paths are designed with a hierarchical structure, allowing child paths
;; to reference their parents (e.g., roam-dir is inside org-dir) for better
;; maintainability.
;;
;;; Code:

;; -------------------------------- Debug Toggle -------------------------------

(defcustom cj/debug-modules nil
  "List of modules with debug functions enabled.
Set to t to enable all debug modules.
Set to a list of module symbols (e.g. \\='(org-agenda mail)) to enable
debug output for those modules only.  Possible values: org-agenda,
mail, chime, etc."
  :type '(choice (const :tag "All modules" t)
                 (repeat :tag "Specific modules" symbol))
  :group 'cj)

;; -------------------------------- Contact Info -------------------------------

(defvar user-whole-name "Craig Jennings"
  "The user's full name.")
(defconst user-name (getenv "USER")
  "The user's name retrieved from the environment variable.")
(defvar user-mail-address "c@cjennings.net"
  "The user's email address.")

;; ---------------------------- Buffer Status Colors ---------------------------

(defconst cj/buffer-status-colors
  '((read-only  . "#f06a3f")  ; red   – buffer is read-only
    (overwrite  . "#c48702")  ; gold  – overwrite mode
    (modified   . "#64aa0f")  ; green – modified & writeable
    (unmodified . "#ffffff")) ; white – unmodified & writeable
  "Alist mapping buffer states to their colors.
Used by cursor color, modeline, and other UI elements.")

;; --------------------------- Media File Extensions ---------------------------

(defvar cj/audio-file-extensions
  '("m4a" "mp3" "wav" "flac" "ogg" "opus" "aac"
    "aiff" "aif" "wma" "ape" "alac" "weba")
  "File extensions recognized as audio files.
Used by transcription module and other audio-related functionality.")

(defvar cj/video-file-extensions
  '("mp4" "mkv" "mov" "webm" "avi" "m4v" "wmv" "flv" "mpg" "mpeg" "3gp" "ogv")
  "File extensions recognized as video files.
Used by transcription to dispatch the audio-extract step in front of
the regular transcription pipeline.")

;; ------------------------ Directory And File Constants -----------------------

;; DIRECTORIES
(defconst emacs-init-file (expand-file-name "init.el" user-emacs-directory)
  "The location of Emacs's main init file.")

(defconst emacs-early-init-file (expand-file-name "early-init.el" user-emacs-directory)
  "The location of Emacs's early init file.")

;; Canonical definition of `user-home-dir' lives in `early-init.el' so
;; the package-archive paths there can reference it during package
;; bootstrap.  The `defvar' below is a no-op at runtime (early-init's
;; defconst wins, defvar doesn't reassign a bound symbol) -- it exists
;; only so this module loads / byte-compiles standalone, when
;; early-init hasn't run.  If you ever change the expression here, keep
;; it identical to early-init.el's.
(defvar user-home-dir (getenv "HOME")
  "The user's home directory per the environment variable.
Canonical definition in early-init.el; this form is a standalone-load
fallback only.")

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

(defconst snippets-dir (expand-file-name "snippets/" user-emacs-directory)
  "The location of yasnippet snippets.")

(defvar sounds-dir (expand-file-name "assets/sounds/" user-emacs-directory)
  "Directory containing sound files for notifications and timers.")

(defconst video-recordings-dir (expand-file-name "sync/recordings/" user-home-dir)
  "The location to save video recordings.")

(defconst audio-recordings-dir (expand-file-name "sync/recordings/" user-home-dir)
  "The location to save audio recordings.")

(defconst music-dir (expand-file-name "music/" user-home-dir)
  "The location to save your music files.")

(defconst website-dir (expand-file-name "projects/website/" user-home-dir)
  "Root directory of the Hugo website project.")


;; FILES
(defvar authinfo-file (expand-file-name ".authinfo.gpg" user-home-dir)
  "The location of the encrypted .authinfo or .netrc file.")

(defvar schedule-file (expand-file-name "schedule.org" org-dir)
  "The location of the org file containing scheduled events.")

(defvar gcal-file (expand-file-name "data/gcal.org" user-emacs-directory)
  "The location of the org file containing Google Calendar information.
Stored in .emacs.d/data/ so each machine syncs independently from Google Calendar.")

(defvar pcal-file (expand-file-name "data/pcal.org" user-emacs-directory)
  "The location of the org file containing Proton Calendar information.
Stored in .emacs.d/data/ so each machine syncs independently from Proton Calendar.")

(defvar dcal-file (expand-file-name "data/dcal.org" user-emacs-directory)
  "The location of the org file containing DeepSat Calendar information.
Stored in .emacs.d/data/ so each machine syncs independently from Google Calendar.")

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

(defun cj/--report-path-failure (kind path err required)
  "Report a failure to create the KIND (a string) at PATH from ERR.
A REQUIRED failure raises a prominent `display-warning' so a broken
environment is hard to miss; an optional one is only logged, so it does not
block startup."
  (if required
      (display-warning 'user-constants
                       (format "Failed to create required %s %s: %s"
                               kind path (error-message-string err))
                       :error)
    (message "Error creating %s %s: %s" kind path (error-message-string err))))

(defun cj/verify-or-create-dir (dir &optional required)
  "Verify the directory DIR exists; create it if it doesn't.
With REQUIRED non-nil, a creation failure raises a prominent warning instead
of being logged quietly."
  (condition-case err
      (unless (file-directory-p dir)
        (make-directory dir t)
        (message "Created directory: %s" dir))
    (error (cj/--report-path-failure "directory" dir err required))))

(defun cj/verify-or-create-file (file &optional required)
  "Verify the file FILE exists; create it if it doesn't.
With REQUIRED non-nil, a creation failure raises a prominent warning instead
of being logged quietly.  The parent directory inherits REQUIRED."
  (condition-case err
      (let ((dir (file-name-directory file)))
        (when dir (cj/verify-or-create-dir dir required))
        (unless (file-exists-p file)
          (write-region "" nil file)
          (message "Created file: %s" file)))
    (error (cj/--report-path-failure "file" file err required))))

(defun cj/initialize-user-directories-and-files ()
  "Initialize all necessary directories and files.
This ensures that all directories and files required by the Emacs configuration
exist, creating them if necessary. This makes the configuration more robust
and portable across different machines."
  (interactive)
  ;; Required: the backbone directories everything else hangs off.  If these
  ;; can't be created the config is broken, so failure warns prominently.
  (dolist (d (list sync-dir org-dir roam-dir))
    (cj/verify-or-create-dir d t))
  ;; Optional: secondary directories whose absence degrades one feature, not
  ;; startup.
  (mapc 'cj/verify-or-create-dir (list drill-dir
									   journals-dir
									   snippets-dir
									   video-recordings-dir
									   audio-recordings-dir))
  ;; Required: the calendar stubs — org-agenda-list hangs prompting for these
  ;; if they're missing (calendar-sync populates them on first sync).
  (dolist (f (list gcal-file pcal-file dcal-file))
    (cj/verify-or-create-file f t))
  ;; Optional: content files each populated by their own workflow.
  (mapc 'cj/verify-or-create-file (list schedule-file
										inbox-file
										article-archive
										reading-notes-file
										contacts-file
										webclipped-file
                                        reference-file)))

;; Creation is deferred to startup: init.el calls
;; `cj/initialize-user-directories-and-files' after requiring this module, so a
;; bare `(require 'user-constants)' (tests, byte-compile, batch) stays
;; side-effect-free.

(provide 'user-constants)
;;; user-constants.el ends here
