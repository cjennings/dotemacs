;;; test-calendar-sync--deferred-start.el --- Deferred auto-start tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; calendar-sync arms its auto-sync on the first org-agenda use instead of at
;; load, so a cold gpg-agent is not prompted for the authinfo passphrase at
;; startup (the :secret-host feed URLs decrypt authinfo.gpg).  These tests cover
;; the one-shot helper: it starts sync once and removes itself, even when the
;; start call errors.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calendar-sync)

;; org-agenda need not be loaded under `make test'; declare the hook special so
;; the dynamic `let' bindings below shadow it cleanly.
(defvar org-agenda-mode-hook)

(ert-deftest test-calendar-sync-deferred-start-fires-once-and-unhooks ()
  "Normal: the one-shot starts sync once and removes itself from the hook."
  (let ((started 0)
        (org-agenda-mode-hook (list #'calendar-sync--auto-start-on-first-agenda)))
    (cl-letf (((symbol-function 'calendar-sync-start)
               (lambda (&rest _) (setq started (1+ started)))))
      (calendar-sync--auto-start-on-first-agenda))
    (should (= started 1))
    (should-not (member #'calendar-sync--auto-start-on-first-agenda
                        org-agenda-mode-hook))))

(ert-deftest test-calendar-sync-deferred-start-unhooks-even-when-start-errors ()
  "Error: a start failure still leaves the hook removed, so it cannot re-fire."
  (let ((org-agenda-mode-hook (list #'calendar-sync--auto-start-on-first-agenda)))
    (cl-letf (((symbol-function 'calendar-sync-start)
               (lambda (&rest _) (error "boom"))))
      (should-error (calendar-sync--auto-start-on-first-agenda)))
    (should-not (member #'calendar-sync--auto-start-on-first-agenda
                        org-agenda-mode-hook))))

(provide 'test-calendar-sync--deferred-start)
;;; test-calendar-sync--deferred-start.el ends here
