;;; test-mu4e-org-contacts-integration.el --- Tests for mu4e org-contacts completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Characterization tests for the org-contacts completion glue.  The module
;; loads without mu4e or org-contacts (both are required with noerror), and the
;; tests stub the header-field predicate, the mode actions, and the contact
;; source, so they run headless with no package dependency.  Coverage centers
;; on the two highest-value behaviors: the capf only fires inside a header
;; field of a compose buffer, and TAB dispatches to completion / org-cycle /
;; indent by context.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mu4e-org-contacts-integration)

;;; cj/org-contacts-completion-at-point

(ert-deftest test-mu4e-org-contacts-capf-nil-outside-header ()
  "Boundary: the capf declines when point is not in a header field."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
              ((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () nil)))
      (should-not (cj/org-contacts-completion-at-point)))))

(ert-deftest test-mu4e-org-contacts-capf-nil-outside-compose-mode ()
  "Boundary: the capf declines outside the mu4e/org-msg compose modes."
  (with-temp-buffer
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil))
              ((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () t)))
      (should-not (cj/org-contacts-completion-at-point)))))

(ert-deftest test-mu4e-org-contacts-capf-returns-spec-in-header ()
  "Normal: in a header field with contacts the capf returns bounds and a table."
  (with-temp-buffer
    (insert "To: ali")
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
              ((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () t))
              ((symbol-function 'cj/get-all-contact-emails)
               (lambda () '("Alice <alice@example.com>" "Bob <bob@example.com>"))))
      (let ((result (cj/org-contacts-completion-at-point)))
        (should result)
        (should (integerp (nth 0 result)))
        (should (= (point) (nth 1 result)))
        ;; the completion start sits just after the "To:" prefix
        (should (string= "ali" (buffer-substring-no-properties
                                (nth 0 result) (nth 1 result))))
        (should (member "Alice <alice@example.com>"
                        (all-completions "" (nth 2 result))))))))

(ert-deftest test-mu4e-org-contacts-capf-nil-when-no-contacts ()
  "Boundary: a header field with no contacts yields no completion."
  (with-temp-buffer
    (insert "To: ")
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
              ((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () t))
              ((symbol-function 'cj/get-all-contact-emails) (lambda () nil)))
      (should-not (cj/org-contacts-completion-at-point)))))

;;; cj/mu4e-org-contacts-tab-complete

(ert-deftest test-mu4e-org-contacts-tab-completes-in-header ()
  "Normal: TAB in a header field triggers completion-at-point."
  (let ((capf-calls 0) (org-cycled nil) (indented nil))
    (cl-letf (((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () t))
              ((symbol-function 'completion-at-point) (lambda () (cl-incf capf-calls)))
              ((symbol-function 'org-cycle) (lambda (&rest _) (setq org-cycled t)))
              ((symbol-function 'indent-for-tab-command)
               (lambda (&rest _) (setq indented t))))
      (cj/mu4e-org-contacts-tab-complete)
      (should (= capf-calls 1))
      (should-not org-cycled)
      (should-not indented))))

(ert-deftest test-mu4e-org-contacts-tab-cycles-in-org-msg-body ()
  "Normal: TAB in the org-msg body (not a header) calls org-cycle."
  (with-temp-buffer
    (setq major-mode 'org-msg-edit-mode)
    (let ((org-cycled nil) (capf-called nil))
      (cl-letf (((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () nil))
                ((symbol-function 'org-cycle) (lambda (&rest _) (setq org-cycled t)))
                ((symbol-function 'completion-at-point)
                 (lambda () (setq capf-called t))))
        (cj/mu4e-org-contacts-tab-complete)
        (should org-cycled)
        (should-not capf-called)))))

(ert-deftest test-mu4e-org-contacts-tab-indents-elsewhere ()
  "Boundary: TAB outside a header and outside the org-msg body indents."
  (with-temp-buffer
    (setq major-mode 'fundamental-mode)
    (let ((indented nil))
      (cl-letf (((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () nil))
                ((symbol-function 'indent-for-tab-command)
                 (lambda (&rest _) (setq indented t))))
        (cj/mu4e-org-contacts-tab-complete)
        (should indented)))))

;;; cj/mu4e-org-contacts-comma-complete

(ert-deftest test-mu4e-org-contacts-comma-inserts-spaced-in-header ()
  "Normal: a comma in a header field inserts a comma and a space."
  (with-temp-buffer
    (insert "To: alice@example.com")
    (cl-letf (((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () t))
              ((symbol-function 'completion-at-point) #'ignore))
      (cj/mu4e-org-contacts-comma-complete)
      (should (string-suffix-p ", " (buffer-string))))))

(ert-deftest test-mu4e-org-contacts-comma-plain-outside-header ()
  "Boundary: a comma outside a header field inserts a bare comma."
  (with-temp-buffer
    (insert "some body text")
    (cl-letf (((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () nil)))
      (cj/mu4e-org-contacts-comma-complete)
      (should (string-suffix-p "text," (buffer-string)))
      (should-not (string-suffix-p ", " (buffer-string))))))

;;; cj/mu4e-org-contacts-insert-email

(ert-deftest test-mu4e-org-contacts-insert-email-noop-outside-header ()
  "Boundary: direct insert does nothing and does not prompt outside a header."
  (with-temp-buffer
    (insert "body")
    (cl-letf (((symbol-function 'mail-abbrev-in-expansion-header-p) (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (error "should not prompt outside a header"))))
      (cj/mu4e-org-contacts-insert-email)
      (should (string= "body" (buffer-string))))))

(provide 'test-mu4e-org-contacts-integration)
;;; test-mu4e-org-contacts-integration.el ends here
