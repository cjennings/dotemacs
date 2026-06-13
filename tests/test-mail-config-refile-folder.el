;;; test-mail-config-refile-folder.el --- Tests for refile-folder dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `cj/mu4e--refile-folder-for-maildir', the per-message refile
;; (archive) target dispatch.  cmail has a real synced Archive folder; the
;; Gmail-backed accounts (gmail, dmail) have none, so refiling them must signal
;; rather than move mail into an unsynced, phantom folder (silent mail loss).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mail-config)

(ert-deftest test-mail-config-refile-cmail-returns-archive ()
  "Normal: a cmail message refiles into the synced /cmail/Archive folder."
  (should (string= (cj/mu4e--refile-folder-for-maildir "/cmail/INBOX")
                   "/cmail/Archive"))
  (should (string= (cj/mu4e--refile-folder-for-maildir "/cmail/Sent")
                   "/cmail/Archive")))

(ert-deftest test-mail-config-refile-gmail-signals ()
  "Error: gmail has no synced archive folder, so refile signals rather than
moving mail into a phantom folder."
  (should-error (cj/mu4e--refile-folder-for-maildir "/gmail/INBOX")
                :type 'user-error))

(ert-deftest test-mail-config-refile-dmail-signals ()
  "Error: dmail (Gmail-backed) has no synced archive folder; refile signals."
  (should-error (cj/mu4e--refile-folder-for-maildir "/dmail/INBOX")
                :type 'user-error))

(ert-deftest test-mail-config-refile-nil-maildir-signals ()
  "Boundary: a message with no maildir cannot be refiled; signal."
  (should-error (cj/mu4e--refile-folder-for-maildir nil)
                :type 'user-error))

(provide 'test-mail-config-refile-folder)
;;; test-mail-config-refile-folder.el ends here
