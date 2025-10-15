;;; org-contacts-config.el --- Org Contacts Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Configuration for org-contacts, providing contact management within org-mode.
;; Integrates with mu4e for email address completion and org-roam for linking
;; contacts to projects and notes.
;;
;; Email completion functionality has been moved to mu4e-org-contacts-integration.el

;;; Code:

(require 'user-constants)

;; --------------------------- Org Agenda Integration --------------------------

(with-eval-after-load 'org-agenda
  ;; Remove the direct hook first (in case it's already added)
  (remove-hook 'org-agenda-finalize-hook 'org-contacts-anniversaries)

  ;; Add a wrapper function that ensures proper context
  (defun cj/org-contacts-anniversaries-safe ()
	"Safely call org-contacts-anniversaries with required bindings."
	(require 'diary-lib)
	;; These need to be dynamically bound for diary functions
	(defvar date)
	(defvar entry)
	(defvar original-date)
	(let ((date (calendar-current-date))
		  (entry "")
		  (original-date (calendar-current-date)))
	  (ignore-errors
		(org-contacts-anniversaries))))

  ;; Use the safe wrapper instead
  (add-hook 'org-agenda-finalize-hook 'cj/org-contacts-anniversaries-safe))

;; ----------------------- Org-Contacts Capture Template -----------------------

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
			   '("C" "Contact" entry (file+headline contacts-file "Contacts")
				 "* %(cj/org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(cj/org-contacts-template-email)
:PHONE: %^{Phone(s) - separate multiple with commas}
:ADDRESS: %^{Address}
:COMPANY: %^{Company}
:TITLE: %^{Title/Position}
:BIRTHDAY: %^{Birthday (YYYY-MM-DD)}
:END:
%^{Notes}
Added: %U"
				 :empty-lines 1)))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
			   '("C" "Contact" entry (file+headline contacts-file "Contacts")
				 "* %(cj/org-contacts-template-name)

Added: %U")))

(defun cj/org-contacts-template-name ()
  "Get name for contact template from context."
  (let ((name (when (boundp 'cj/contact-name) cj/contact-name)))
	(or name
		(when (eq major-mode 'mu4e-headers-mode)
		  (mu4e-message-field (mu4e-message-at-point) :from-or-to))
		(when (eq major-mode 'mu4e-view-mode)
		  (mu4e-message-field mu4e~view-message :from-or-to))
		(read-string "Name: "))))

(defun cj/org-contacts-template-email ()
  "Get email for contact template from context."
  (let ((email (when (boundp 'cj/contact-email) cj/contact-email)))
	(or email
		(when (eq major-mode 'mu4e-headers-mode)
		  (let ((from (mu4e-message-field (mu4e-message-at-point) :from)))
			(when from (cdr (car from)))))
		(when (eq major-mode 'mu4e-view-mode)
		  (let ((from (mu4e-message-field mu4e~view-message :from)))
			(when from (cdr (car from)))))
		(read-string "Email: "))))

;;; ------------------------- Quick Contact Functions ---------------------------

(defun cj/org-contacts-find ()
  "Find and open a contact."
  (interactive)
  (find-file contacts-file)
  (goto-char (point-min))
  (let ((contact (completing-read "Contact: "
								  (org-map-entries
								   (lambda () (nth 4 (org-heading-components)))
								   nil (list contacts-file)))))
	(goto-char (point-min))
	(search-forward contact)
	(org-fold-show-entry)
	(org-reveal)))

(defun cj/org-contacts-new ()
  "Create a new contact."
  (interactive)
  (org-capture nil "C"))

(defun cj/org-contacts-view-all ()
  "View all contacts in a column view."
  (interactive)
  (find-file contacts-file)
  (org-columns))

;;; -------------------------- Org-Roam Integration -----------------------------

;; (with-eval-after-load 'org-roam
;;   (defun cj/org-contacts-link-to-roam ()
;; 	"Link current contact to an org-roam node."
;; 	(interactive)
;; 	(when (eq major-mode 'org-mode)
;; 	  (let ((contact-name (org-entry-get (point) "ITEM")))
;; 		(org-set-property "ROAM_REFS"
;; 						  (org-roam-node-id
;; 						   (org-roam-node-read nil nil nil nil
;; 											   :initial-input contact-name)))))))

;;; ----------------------------- Birthday Agenda --------------------------------

(with-eval-after-load 'org-agenda
  ;; Add birthdays to agenda
  (setq org-agenda-include-diary t)

  ;; Custom agenda command for upcoming birthdays
  (add-to-list 'org-agenda-custom-commands
			   '("b" "Birthdays and Anniversaries"
				 ((tags-todo "BIRTHDAY|ANNIVERSARY"
							 ((org-agenda-overriding-header "Upcoming Birthdays and Anniversaries")
							  (org-agenda-sorting-strategy '(time-up))))))))

;;; ---------------------------- Core Contact Data Functions ---------------------------

(defun cj/org-contacts--props-matching (entry pattern)
  "Return all property values from ENTRY whose keys match PATTERN (a regexp)."
  (let ((props (nth 2 entry)))
	(delq nil
		  (mapcar (lambda (prop)
					(when (string-match-p pattern (car prop))
					  (cdr prop)))
				  props))))

(defun cj/get-all-contact-emails ()
  "Retrieve all contact emails from org-contacts database.
Returns a list of formatted strings like \"Name <email@example.com>\".
This is the core function used by the mu4e integration module."
  (let ((contacts (org-contacts-db)))
	(delq nil
		  (mapcan (lambda (e)
					(let* ((name (car e))
						   ;; This returns a LIST of email strings
						   (email-strings (cj/org-contacts--props-matching e "EMAIL")))
					  ;; Need mapcan here to handle the list
					  (mapcan (lambda (email-str)
								(when (and email-str (string-match-p "[^[:space:]]" email-str))
								  (mapcar (lambda (email)
											(format "%s <%s>" name (string-trim email)))
										  (split-string email-str "[,;[:space:]]+" t))))
							  email-strings)))
				  contacts))))

;; Simple insertion function for use outside of mu4e
(defun cj/insert-contact-email ()
  "Select and insert a contact's email address at point.
For use outside of mu4e compose buffers. In mu4e, the integration
module provides more sophisticated completion."
  (interactive)
  (let* ((items (cj/get-all-contact-emails))
		 (selected (completing-read "Contact: " items nil t)))
	(insert selected)))

;;; -------------------------------- Org Contacts --------------------------------

(use-package org-contacts
  :after (org mu4e)
  :custom
  (org-contacts-files (list contacts-file))
  :config
  (require 'mu4e)
  ;; Basic settings
  (setq org-contacts-icon-use-gravatar nil)  ; Don't fetch gravatars

  ;; Birthday and anniversary handling
  (setq org-contacts-birthday-format "It's %l's birthday today! 🎂")
  (setq org-contacts-anniversary-format "%l's anniversary 💑")

  ;; Email address formatting
  (setq org-contacts-email-link-description-format "%s <%e>")

  (setq mu4e-org-contacts-file contacts-file)
  (add-to-list 'mu4e-headers-actions
			   '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
			   '("org-contact-add" . mu4e-action-add-org-contact) t)

  ;; Disable mu4e's built-in completion in favor of our custom solution
  (setq mu4e-compose-complete-addresses nil))

;;; ---------------------------- Org-Contacts Keymap ----------------------------

;; Keymap for `org-contacts' commands
(defvar cj/org-contacts-map
  (let ((map (make-sparse-keymap)))
	(define-key map "f" 'cj/org-contacts-find)     ;; find contact
	(define-key map "n" 'cj/org-contacts-new)      ;; new contact
	(define-key map "e" 'cj/insert-contact-email)  ;; inserts email from org-contact
	(define-key map "v" 'cj/org-contacts-view-all) ;; view all contacts
	map)
  "Keymap for `org-contacts' commands.")

;; Bind the org-contacts map to the C-c C prefix
(global-set-key (kbd "C-c C") cj/org-contacts-map)

(provide 'org-contacts-config)
;;; org-contacts-config.el ends here
