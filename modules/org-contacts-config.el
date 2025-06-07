;;; org-contacts-config.el --- Org Contacts Customizations -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; Configuration for Org Contacts.

;;; Code:

(use-package org-contacts
  :after (org mu4e)
  :defer .5
  :bind ("C-z C" . org-contacts) ; starts contacts search
  :config
  (setq org-contacts-files (cons contacts-file '()))

  (add-to-list 'org-capture-templates
			   '("c" "Contact" entry (file+headline contacts-file "Contacts")
				 "*%?\n:PROPERTIES:\n:ADDRESS: \n:PHONE: \n:EMAIL: \n:BIRTHDAY: \n:NOTES: \n:END:" :prepend t))

  (setq mu4e-contacts-file contacts-file)

  (add-to-list 'mu4e-headers-actions
			   '("add contact" . mu4e-action-add-org-contact))
  (add-to-list 'mu4e-view-actions
			   '("add contact" . mu4e-action-add-org-contact)))

(provide 'org-contacts-config)
;;; org-contacts-config.el ends here.
