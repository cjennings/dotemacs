;;; local-repository.el --- Local package archive helpers -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; the mirror-refresh command can autoload.
;; Top-level side effects: none.
;; Runtime requires: elpa-mirror when updating the mirror.
;; Direct test load: yes.
;;
;; Provides a command to refresh the checked-in local package archive from the
;; installed packages via elpa-mirror.  Adding that archive to package-archives
;; is owned by early-init.el (see `localrepo-location'); this module only
;; refreshes it.

;;; Code:

(require 'elpa-mirror nil t)  ;; optional; cj/update-localrepo-repository fails at call-time if absent

(declare-function elpamr-create-mirror-for-installed "elpa-mirror")
(defvar localrepo-location)  ;; defconst in early-init.el: the archive path

(defun cj/update-localrepo-repository ()
  "Update the local repository with currently installed packages.
Targets `localrepo-location', the archive path early-init.el sets up."
  (interactive)
  (elpamr-create-mirror-for-installed localrepo-location t))

(provide 'local-repository)
;;; local-repository.el ends here.
