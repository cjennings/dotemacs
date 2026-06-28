;;; mu4e-org-contacts-setup.el --- Setup mu4e with org-contacts -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Thin activation wrapper for mu4e-org-contacts-integration. If mu4e is loaded,
;; enable org-contacts completion and disable mu4e's internal contact collector
;; so completion has one source of truth.

;;; Code:

(defvar mu4e-compose-complete-only-personal)
(defvar mu4e-compose-complete-only-after)
(declare-function cj/activate-mu4e-org-contacts-integration "mu4e-org-contacts-integration")

;; Load the integration module.  Activation only runs when the module loaded
;; cleanly AND mu4e is present; otherwise this file is a no-op so the rest
;; of the config can load without mu4e installed.
(when (require 'mu4e-org-contacts-integration nil t)
  (when (featurep 'mu4e)
    (cj/activate-mu4e-org-contacts-integration)))

;; Optional: If you want to use org-contacts as the primary source,
;; you might want to disable mu4e's contact caching to save memory
(with-eval-after-load 'mu4e
  ;; Disable mu4e's internal contact collection
  (setq mu4e-compose-complete-only-personal nil)
  (setq mu4e-compose-complete-only-after nil))

(provide 'mu4e-org-contacts-setup)
;;; mu4e-org-contacts-setup.el ends here