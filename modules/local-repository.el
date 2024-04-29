;;; local-repository.el --- local repository functionality -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elpa-mirror)

;; ------------------------------ Utility Function -----------------------------


(defun car-member (value list)
  "Check if VALUE exists as the car of any cons cell in LIST."
  (member value (mapcar #'car list)))

;; ------------------------------- Customizations ------------------------------

(defcustom localrepo-repository-id "localrepo"
  "The name used to identify the local repository internally.
Used for the package-archive and package-archive-priorities lists.")

(defcustom localrepo-repository-priority 100
  "The value for the local repository in the package-archive-priority list.
A higher value means higher priority. If you want your local packages to be
preferred, this must be a higher number than any other repositories.")

(defcustom localrepo-repository-location
  (concat user-emacs-directory "/.localrepo")
  "The location of the local repository.
It's a good idea to keep this with the rest of your configuration files and
keep them in source control.")

(defun localrepo-update-repository ()
  "Update the local repository with currently installed packages."
  (interactive)
  (elpamr-create-mirror-for-installed localrepo-repository-location t))

(defun localrepo-initialize ()
"Add the repository to the package archives, then gives it a high priority."
  (unless (car-member localrepo-repository-id package-archives)
	(add-to-list 'package-archives
				 (localrepo-repository-id . localrepo-repository-location)))

  (unless (car-member localrepo-repository-id package-archive-priorities)
	(add-to-list 'package-archive-priorities
				 (localrepo-repository-id . localrepo-repository-priority))))

(provide 'local-repository)
;;; local-repository.el ends here.
