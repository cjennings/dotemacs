;;; local-repository.el --- local repository functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; local package-mirror workflow, a command-loaded deferral
;;   candidate.
;; Top-level side effects: none.
;; Runtime requires: elpa-mirror.
;; Direct test load: yes.
;;
;;; Code:

(require 'elpa-mirror nil t)  ;; optional; cj/update-localrepo-repository fails at call-time if absent

;; ------------------------------ Utility Function -----------------------------


(defun car-member (value list)
  "Check if VALUE exists as the car of any cons cell in LIST."
  (member value (mapcar #'car list)))

;; ------------------------------- Customizations ------------------------------

(defgroup localrepo nil
  "Local last-known-good package repository."
  :group 'package)

(defcustom localrepo-repository-id "localrepo"
  "The name used to identify the local repository internally.

Used for the package-archive and package-archive-priorities lists."
  :type 'string
  :group 'localrepo)

(defcustom localrepo-repository-priority 100
  "The value for the local repository in the package-archive-priority list.

A higher value means higher priority. If you want your local packages to be
preferred, this must be a higher number than any other repositories."
  :type 'integer
  :group 'localrepo)

(defcustom localrepo-repository-location
  (concat user-emacs-directory "/.localrepo")
  "The location of the local repository.

It's a good idea to keep this with the rest of your configuration files and
keep them in source control."
  :type 'directory
  :group 'localrepo)

(defun cj/update-localrepo-repository ()
  "Update the local repository with currently installed packages."
  (interactive)
  (elpamr-create-mirror-for-installed localrepo-repository-location t))

(defun localrepo-initialize ()
"Add the repository to the package archives, then gives it a high priority."
  (unless (car-member localrepo-repository-id package-archives)
	(add-to-list 'package-archives
				 (cons localrepo-repository-id localrepo-repository-location)))

  (unless (car-member localrepo-repository-id package-archive-priorities)
	(add-to-list 'package-archive-priorities
				 (cons localrepo-repository-id localrepo-repository-priority))))

(provide 'local-repository)
;;; local-repository.el ends here.
