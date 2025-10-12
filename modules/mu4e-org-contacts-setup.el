;;; mu4e-org-contacts-setup.el --- Setup mu4e with org-contacts -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Simple setup file to enable org-contacts integration with mu4e.
;; Add this to your mail-config.el or load it after both mu4e and org-contacts.

;;; Code:

;; Load the integration module
(require 'mu4e-org-contacts-integration)

;; Activate the integration
(cj/activate-mu4e-org-contacts-integration)

;; Optional: If you want to use org-contacts as the primary source,
;; you might want to disable mu4e's contact caching to save memory
(with-eval-after-load 'mu4e
  ;; Disable mu4e's internal contact collection
  (setq mu4e-compose-complete-only-personal nil)
  (setq mu4e-compose-complete-only-after nil))

(provide 'mu4e-org-contacts-setup)
;;; mu4e-org-contacts-setup.el ends here