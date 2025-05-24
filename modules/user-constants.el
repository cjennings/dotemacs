;;; user-constants.el --- User Constants -*- lexical-binding: t; -*-

;;; Commentary:

;; User file locations are defined here. A file or directory is created if it
;; doesn't exist. Note the keybindings at the bottom for quick access.

;;; Code:

;; ------------------------ Directory And File Constants -----------------------

;; DIRECTORIES
(defconst emacs-init-file (concat user-emacs-directory "init.el")
  "The location of Emacs's main init file.")

(defconst emacs-early-init-file (concat user-emacs-directory "early-init.el")
  "The location of Emacs's early init file.")

(defconst user-home-dir (getenv "HOME")
  "The user's home directory per the environment variable.")

(defconst books-dir     (concat user-home-dir "/sync/books/")
  "The location of org-roam files.")

(defconst code-dir      (concat user-home-dir "/code/")
  "Code repositories are located in this directory.")

(defconst dl-dir        (concat user-home-dir "/downloads/")
  "Location of the general downloads directory.")

(defconst pix-dir       (concat user-home-dir "/pictures/")
  "Location of where pictures and images are stored.")

(defconst projects-dir  (concat user-home-dir "/projects/")
  "Non-code projects and repositories are located in this directory.")

(defconst videos-dir    (concat user-home-dir "/videos/")
  "Location of where videos are stored.")

(defconst mail-dir      (concat user-home-dir ".mail/")
  "Root directory where the mail folders are located.")

(defconst sync-dir      (concat user-home-dir "/sync/org/")
  "This directory is synchronized across machines.")

(defconst roam-dir      (concat sync-dir "roam/")
  "The location of org-roam files.")

(defconst journals-dir  (concat roam-dir "journal/")
  "The location of org-roam dailies or journals files.")

(defconst drill-dir     (concat sync-dir "drill/")
  "The location of org-drill org files.")

(defconst snippets-dir  (concat sync-dir "snippets/")
  "The location of ya-snippet snippets.")

(defconst video-recordings-dir "~/videos/recordings"
  "The location to save the ffmpeg recordings.")


;; FILES
(defvar schedule-file   (concat sync-dir "schedule.org")
  "The location of the org file containing scheduled events.")

(defvar inbox-file      (concat roam-dir "inbox.org")
  "The location of the org file that serves as the task inbox.")

(defvar contacts-file   (concat sync-dir "contacts.org")
  "The location of the org file containing org-contacts information.")

(defvar reference-file   (concat sync-dir "reference.org")
  "The location of the org file containing reference information.")

(defvar article-file    (concat sync-dir "article-inbox.org")
  "The location of the org file containing new clipped pages to review.")

(defvar article-archive (concat sync-dir "article-archive.org")
  "The location of the org file that stores saved articples to keep.")
                                        ;
(defvar ledger-file     (concat sync-dir  "main.ledger")
  "The location of the user's ledger file.")

(defvar macros-file     (concat sync-dir "macros.el")
  "The location of the macros file for recorded saved macros via M-f3.")

(defvar authinfo-file   (concat user-home-dir "/.authinfo.gpg")
  "The location of the encrypted .authinfo or .netrc file.")

;; ------------------------- Verify Or Create Functions ------------------------

(defun cj/verify-or-create-dir (dir)
  "Verify the directory DIR exists; create it if it doesn't."
  (unless (file-directory-p dir)
    (make-directory dir t)
    (message "Warning: Directory %s not found, so created it" dir)))

(defun cj/verify-or-create-file (file)
  "Verify the file FILE exists; create it if it doesn't."
  (unless (file-exists-p file)
    (write-region "" nil file)
    (message "Warning: File %s not found, so created it" file)))

;; ------------------------- Verify Or Create Constants ------------------------

(mapc 'cj/verify-or-create-dir (list sync-dir
									 roam-dir
									 journals-dir
									 video-recordings-dir
									 snippets-dir))

(mapc 'cj/verify-or-create-file (list schedule-file
                                      inbox-file
                                      contacts-file
                                      article-file
                                      article-archive
                                      ledger-file
									  macros-file))

(provide 'user-constants)
;;; user-constants.el ends here.
