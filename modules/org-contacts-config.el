;;; org-contacts-config.el --- Org Contacts Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Configuration for org-contacts, providing contact management within org-mode.
;; Integrates with mu4e for email address completion and org-roam for linking
;; contacts to projects and notes.

;;; Code:

(require 'user-constants)

;;; -------------------------------- Org Contacts --------------------------------

(use-package org-contacts
  :after org
  :custom
  (org-contacts-files (list contacts-file))
  :config
  ;; Basic settings
  (setq org-contacts-icon-use-gravatar nil)  ; Don't fetch gravatars
  (setq org-contacts-matcher "EMAIL<>\"\"|PHONE<>\"\"|ADDRESS<>\"\"")

  ;; Birthday and anniversary handling
  (setq org-contacts-birthday-format "🎂 It's %l's birthday today! 🎂")
  (setq org-contacts-anniversary-format "💑 %l's anniversary 💑")

  ;; Integration with completion frameworks
  (setq org-contacts-complete-functions
		'(org-contacts-complete-name
		  org-contacts-complete-email
		  org-contacts-complete-tags))

  ;; Email address formatting
  (setq org-contacts-email-link-description-format "%s <%e>")

  ;; Enable vCard export
  (require 'org-vcard nil t))


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
  (add-hook 'org-agenda-finalize-hook 'cj/org-contacts-anniversaries-safe)

  ;; Keep your other settings
  (setq org-contacts-anniversary-agenda-days 7))

;;; ---------------------------- Capture Templates ------------------------------

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
			   '("C" "Contact" entry (file+headline contacts-file "Contacts")
				 "* %(cj/org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(cj/org-contacts-template-email)
:PHONE:
:ADDRESS:
:BIRTHDAY:  %^{YYYY-MM-DD}
:NOTE: %^{Note}
:END:
Added: %U")))

;; Helper functions for capture templates
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
	(org-show-entry)))

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

(with-eval-after-load 'org-roam
  (defun cj/org-contacts-link-to-roam ()
	"Link current contact to an org-roam node."
	(interactive)
	(when (eq major-mode 'org-mode)
	  (let ((contact-name (org-entry-get (point) "ITEM")))
		(org-set-property "ROAM_REFS"
						  (org-roam-node-id
						   (org-roam-node-read nil nil nil nil
											   :initial-input contact-name)))))))

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

;;; ---------------------------- Insert Contact Email ---------------------------

(defun cj/org-contacts--prop (entry key)
  "Return property KEY from org-contacts ENTRY. KEY is an uppercase string, e.g. \"EMAIL\"."
  (let* ((props (nth 2 entry))
		 (cell (assoc key props)))
	(and cell (cdr cell))))

(defun cj/insert-contact-email ()
  "Select and insert a contact's email address at point."
  (interactive)
  (let* ((contacts (org-contacts-db))
		 (items (delq nil
					  (mapcar (lambda (e)
								(let* ((name (car e))
									   (email (cj/org-contacts--prop e "EMAIL")))
								  (when (and email (string-match-p "[^[:space:]]" email))
									(format "%s <%s>" name
											(car (split-string email "[,;[:space:]]+" t))))))
							  contacts)))
		 (selected (completing-read "Contact: " items nil t)))
	(insert selected)))


;;; ----------------------------- Mu4e Integration ------------------------------

(with-eval-after-load 'mu4e
  (setq mu4e-org-contacts-file contacts-file)
  (add-to-list 'mu4e-headers-actions
			   '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
			   '("org-contact-add" . mu4e-action-add-org-contact) t))

;; WIP: replaced the below with native mu4e functions
;; (add-to-list 'mu4e-headers-actions
;;             '("add contact" . cj/mu4e-action-add-org-contact) t)
;; (add-to-list 'mu4e-view-actions
;;             '("add contact" . cj/mu4e-action-add-org-contact) t))

;; (defun cj/mu4e-action-add-org-contact (msg)
;;   "Add contact from email MSG to org-contacts."
;;   (let* ((from (plist-get msg :from))
;;       (name (or (car (car from)) ""))
;;       (email (cdr (car from)))
;;       (subject (plist-get msg :subject)))
;;  (let ((cj/contact-name name)
;;        (cj/contact-email email)
;;        (cj/contact-note (or subject "")))
;;    (org-capture nil "C"))))

(defun cj/setup-org-contacts-completion ()
  "Setup org-contacts as the completion source for email addresses."
  ;; Clear any existing completion functions first
  (setq-local completion-at-point-functions nil)
  ;; Add our custom completion function
  (add-hook 'completion-at-point-functions
			#'cj/org-contacts-company-complete
			nil t))

;; Apply to all relevant modes
(add-hook 'message-mode-hook #'cj/setup-org-contacts-completion)
(add-hook 'mu4e-compose-mode-hook #'cj/setup-org-contacts-completion)
(add-hook 'org-msg-edit-mode-hook #'cj/setup-org-contacts-completion)

;; Also ensure company-mode uses the right backend with proper filtering
(with-eval-after-load 'company
  (defun cj/mu4e-compose-company-setup ()
	;; Use capf with our custom completion function
	(setq-local company-backends '(company-capf))
	(setq-local company-idle-delay 0.2)
	(setq-local company-minimum-prefix-length 1)  ; Changed to 1 for better responsiveness
	;; Ensure company filters properly
	(setq-local company-transformers '(company-sort-by-occurrence))
	;; Make sure company respects our filtering
	(setq-local company-require-match nil)
	(setq-local company-frontends '(company-pseudo-tooltip-frontend
									company-echo-metadata-frontend)))

  (add-hook 'mu4e-compose-mode-hook #'cj/mu4e-compose-company-setup)
  (add-hook 'org-msg-edit-mode-hook #'cj/mu4e-compose-company-setup))

;;; ---------------------------- Org-Contacts Keymap ----------------------------

;; Keymap for `org-contacts' commands
(defvar cj/org-contacts-map
  (let ((map (make-sparse-keymap)))
	(define-key map "f" 'cj/org-contacts-find)     ;; find contact
	(define-key map "n" 'cj/org-contacts-new)      ;; new contact
	(define-key map "i" 'cj/insert-contact-email)  ;; inserts email from org-contact
	(define-key map "v" 'cj/org-contacts-view-all) ;; view all contacts
	map)
  "Keymap for `org-contacts' commands.")

;; Bind the org-contacts map to the C-c C prefix
(global-set-key (kbd "C-c C") cj/org-contacts-map)



(provide 'org-contacts-config)
;;; org-contacts-config.el ends here.
