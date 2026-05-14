;;; test-org-contacts-config-commands.el --- Tests for org-contacts-config commands + helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the capture-finalize hook and the email-string
;; parser.  This file covers:
;;
;;   cj/org-contacts-template-name
;;   cj/org-contacts-template-email
;;   cj/org-contacts-new
;;   cj/org-contacts--props-matching
;;   cj/get-all-contact-emails
;;   cj/insert-contact-email
;;
;; mu4e / org-contacts / org-capture primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-contacts-config)

;;; cj/org-contacts-template-name

(ert-deftest test-org-contacts-template-name-falls-back-to-read-string ()
  "Boundary: outside mu4e the name comes from `read-string'."
  (with-temp-buffer
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Alice")))
      (should (equal (cj/org-contacts-template-name) "Alice")))))

(ert-deftest test-org-contacts-template-name-prefers-mu4e-headers ()
  "Normal: in mu4e-headers-mode the from-or-to field wins."
  (with-temp-buffer
    (setq major-mode 'mu4e-headers-mode)
    (cl-letf (((symbol-function 'mu4e-message-at-point)
               (lambda () 'fake-msg))
              ((symbol-function 'mu4e-message-field)
               (lambda (_msg field) (when (eq field :from-or-to) "Bob")))
              ((symbol-function 'read-string)
               (lambda (&rest _) "wrong")))
      (should (equal (cj/org-contacts-template-name) "Bob")))))

;;; cj/org-contacts-template-email

(ert-deftest test-org-contacts-template-email-falls-back-to-read-string ()
  "Boundary: outside mu4e the email comes from `read-string'."
  (with-temp-buffer
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "alice@example.com")))
      (should (equal (cj/org-contacts-template-email) "alice@example.com")))))

(ert-deftest test-org-contacts-template-email-mu4e-headers-extracts-from-address ()
  "Normal: in mu4e-headers-mode the email comes from the :from field."
  (with-temp-buffer
    (setq major-mode 'mu4e-headers-mode)
    (cl-letf (((symbol-function 'mu4e-message-at-point)
               (lambda () 'fake-msg))
              ((symbol-function 'mu4e-message-field)
               (lambda (_msg field)
                 (when (eq field :from) '(("Bob" . "bob@example.com")))))
              ((symbol-function 'read-string)
               (lambda (&rest _) "wrong")))
      (should (equal (cj/org-contacts-template-email) "bob@example.com")))))

;;; cj/org-contacts-new

(ert-deftest test-org-contacts-new-invokes-capture-with-key-C ()
  "Normal: the new-contact wrapper delegates to `org-capture' with key \"C\"."
  (let ((key nil))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (_arg k) (setq key k))))
      (cj/org-contacts-new))
    (should (equal key "C"))))

;;; cj/org-contacts--props-matching

(ert-deftest test-org-contacts-props-matching-returns-matching-values ()
  "Normal: returns the values whose keys match the pattern."
  (let ((entry (list "Alice" 'marker
                     '(("EMAIL" . "a@x.com")
                       ("EMAIL_WORK" . "alice@corp")
                       ("PHONE" . "+1-555-0100")))))
    (should (equal (sort (cj/org-contacts--props-matching entry "EMAIL")
                         #'string<)
                   '("a@x.com" "alice@corp")))))

(ert-deftest test-org-contacts-props-matching-empty-when-no-match ()
  "Boundary: no props match the pattern -> empty list."
  (let ((entry (list "Alice" 'marker '(("PHONE" . "+1-555-0100")))))
    (should (null (cj/org-contacts--props-matching entry "EMAIL")))))

;;; cj/get-all-contact-emails

(ert-deftest test-org-contacts-get-all-emails-formats-as-name-and-bracket ()
  "Normal: contacts come back as 'Name <email>' strings, one per email."
  (cl-letf (((symbol-function 'org-contacts-db)
             (lambda ()
               (list (list "Alice" 'marker '(("EMAIL" . "a@x.com")))
                     (list "Bob"   'marker '(("EMAIL" . "b@x.com")))))))
    (let ((all (cj/get-all-contact-emails)))
      (should (member "Alice <a@x.com>" all))
      (should (member "Bob <b@x.com>" all)))))

(ert-deftest test-org-contacts-get-all-emails-expands-multi-email-strings ()
  "Boundary: an EMAIL value containing multiple emails expands to a list."
  (cl-letf (((symbol-function 'org-contacts-db)
             (lambda ()
               (list (list "Carol" 'marker
                           '(("EMAIL" . "c1@x.com, c2@x.com")))))))
    (let ((all (cj/get-all-contact-emails)))
      (should (member "Carol <c1@x.com>" all))
      (should (member "Carol <c2@x.com>" all)))))

;;; cj/insert-contact-email

(ert-deftest test-org-contacts-insert-contact-email-inserts-selection ()
  "Normal: insert-contact-email puts the chosen candidate into the buffer."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/get-all-contact-emails)
               (lambda () '("Alice <a@x.com>" "Bob <b@x.com>")))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Alice <a@x.com>")))
      (cj/insert-contact-email))
    (should (equal (buffer-string) "Alice <a@x.com>"))))

(provide 'test-org-contacts-config-commands)
;;; test-org-contacts-config-commands.el ends here
