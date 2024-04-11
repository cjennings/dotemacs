;;; user-constants.el --- User Constants -*- lexical-binding: t; -*-

;;; Commentary:
;; user file locations are defined here.
;; if they aren't found, they are created.

;;; Code:

;; ------------------------ Directory And File Constants -----------------------

(defconst emacs-init-file (concat user-emacs-directory "init.el")
  "The location of Emacs's main init file.")

(defconst emacs-early-init-file (concat user-emacs-directory "early-init.el")
  "The location of Emacs's early init file.")

(defconst code-dir        (concat user-home-dir "/code/")
  "Code repositories are located in this directory.")

(defconst projects-dir    (concat user-home-dir "/projects/")
  "Non-code projects and repositories are located in this directory.")

(defconst sync-dir        (concat user-home-dir "/sync/org/")
  "This directory is synchronized across machines.")

(defconst roam-dir        (concat sync-dir "roam/")
  "The location of org-roam files.")

(defconst drill-dir        (concat sync-dir "drill/")
  "The location of org-drill org files.")

(defconst snippets-dir    (concat sync-dir "snippets/")
  "The location of ya-snippet snippets.")


(defvar schedule-file     (concat sync-dir "schedule.org")
  "The location of the org file containing scheduled events.")

(defvar inbox-file        (concat roam-dir "inbox.org")
  "The location of the org file that serves as the task inbox.")

(defvar contacts-file     (concat sync-dir "contacts.org")
  "The location of the org file containing org-contacts information.")

(defvar article-file      (concat sync-dir "article-inbox.org")
  "The location of the org file containing new clipped pages to review.")

(defvar article-archive   (concat sync-dir "article-archive.org")
  "The location of the org file that stores saved articples to keep.")
                                        ;
(defvar ledger-file       (concat sync-dir  "main.ledger")
  "The location of the user's ledger file.")

(defvar macros-file       (concat sync-dir "macros.el")
  "The location of the macros file for recorded saved macros via M-f3.")

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
