;;; test-mail-config.el --- Tests for mail-config keymap wiring -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that mail-config wires the attachment-save commands (defined in
;; `mu4e-attachments') into its `cj/email-map' prefix keymap.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mail-config)

(ert-deftest test-mail-config-email-map-attachment-bindings ()
  "Normal: the email prefix map routes to the attachment-save commands."
  (should (eq (lookup-key cj/email-map (kbd "S"))
              #'cj/mu4e-save-all-attachments))
  (should (eq (lookup-key cj/email-map (kbd "s"))
              #'cj/mu4e-save-attachment-here))
  (should (eq (lookup-key cj/email-map (kbd "m"))
              #'cj/mu4e-save-some-attachments)))

(provide 'test-mail-config)
;;; test-mail-config.el ends here
