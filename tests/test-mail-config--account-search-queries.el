;;; test-mail-config--account-search-queries.el --- Tests for the mail account-nav helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/--mail-account-search-queries (pure: account name -> the four mu4e search
;; strings) and cj/--mail-make-account-map (builds the per-account nav keymap)
;; replace three near-identical defvar-keymap blocks that differed only by
;; maildir prefix.  The map test invokes each binding with mu4e-search mocked,
;; which also verifies each loop-built closure captured its own query.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mail-config)

(ert-deftest test-mail-account-search-queries-cmail ()
  "Normal: the four searches are scoped to the account's INBOX maildir."
  (should (equal (cj/--mail-account-search-queries "cmail")
                 '(("i" . "maildir:/cmail/INBOX")
                   ("u" . "maildir:/cmail/INBOX AND flag:unread AND NOT flag:trashed")
                   ("s" . "maildir:/cmail/INBOX AND flag:flagged")
                   ("l" . "maildir:/cmail/INBOX AND size:5M..999M")))))

(ert-deftest test-mail-account-search-queries-prefix-varies ()
  "Boundary: only the maildir prefix changes between accounts."
  (should (equal (cdr (assoc "i" (cj/--mail-account-search-queries "dmail")))
                 "maildir:/dmail/INBOX"))
  (should (equal (cdr (assoc "i" (cj/--mail-account-search-queries "gmail")))
                 "maildir:/gmail/INBOX")))

(ert-deftest test-mail-make-account-map-binds-four-keys ()
  "Normal: the built keymap binds i/u/s/l to commands."
  (let ((map (cj/--mail-make-account-map "cmail")))
    (dolist (key '("i" "u" "s" "l"))
      (should (commandp (keymap-lookup map key))))))

(ert-deftest test-mail-make-account-map-closures-capture-distinct-queries ()
  "Normal: each binding runs its own account-scoped search (no closure leak).
mu4e-search is mocked to capture the query each command passes."
  (let ((searched '()))
    (cl-letf (((symbol-function 'mu4e-search)
               (lambda (q) (push q searched))))
      (let ((map (cj/--mail-make-account-map "dmail")))
        (funcall (keymap-lookup map "i"))
        (funcall (keymap-lookup map "u"))))
    (should (member "maildir:/dmail/INBOX" searched))
    (should (member "maildir:/dmail/INBOX AND flag:unread AND NOT flag:trashed"
                    searched))))

(provide 'test-mail-config--account-search-queries)
;;; test-mail-config--account-search-queries.el ends here
