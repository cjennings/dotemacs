;;; mu4e-org-contacts-setup.el --- Setup mu4e with org-contacts -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Simple setup file to enable org-contacts integration with mu4e.
;; Add this to your mail-config.el or load it after both mu4e and org-contacts.

;;; Code:

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