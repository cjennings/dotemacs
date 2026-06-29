;;; local-repository.el --- Local package archive helpers -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; local package mirror commands can autoload.
;; Top-level side effects: none.
;; Runtime requires: elpa-mirror when updating the mirror.
;; Direct test load: yes.
;;
;; Adds the checked-in local package archive to package-archives with high
;; priority, and provides a command to refresh that archive from installed
;; packages via elpa-mirror.

;;; Code:

(require 'elpa-mirror nil t)  ;; optional; cj/update-localrepo-repository fails at call-time if absent

(declare-function elpamr-create-mirror-for-installed "elpa-mirror")

;; ------------------------------ Utility Function -----------------------------


(defun localrepo--car-member (value list)
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
  (unless (localrepo--car-member localrepo-repository-id package-archives)
	(add-to-list 'package-archives
				 (cons localrepo-repository-id localrepo-repository-location)))

  (unless (localrepo--car-member localrepo-repository-id package-archive-priorities)
	(add-to-list 'package-archive-priorities
				 (cons localrepo-repository-id localrepo-repository-priority))))

(provide 'local-repository)
;;; local-repository.el ends here.
