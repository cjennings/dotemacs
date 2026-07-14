;;; test-mail-config-transport.el --- Tests for mail transport setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies mail transport debug logging is opt-in and external mail
;; executables are validated before command variables are assigned.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mail-config)

(defmacro test-mail-config--with-executables (executables &rest body)
  "Run BODY with `executable-find' returning paths from EXECUTABLES.
EXECUTABLES is an alist of program name strings to executable paths."
  (declare (indent 1))
  `(let (test-mail-config--warnings)
     (cl-letf (((symbol-function 'executable-find)
                (lambda (program &rest _)
                  (cdr (assoc program ,executables))))
               ((symbol-function 'display-warning)
                (lambda (type message &rest _args)
                  (push (cons type message) test-mail-config--warnings))))
       ,@body)))

(ert-deftest test-mail-config-transport-smtpmail-debug-defaults-off ()
  "SMTP transport debug logging should default to disabled."
  (test-mail-config--with-executables '(("msmtp" . "/usr/bin/msmtp"))
    (let ((cj/smtpmail-debug-enabled nil)
          (smtpmail-debug-info :before-load))
      (cj/mail-configure-smtpmail)
      (should-not smtpmail-debug-info))))

(ert-deftest test-mail-config-transport-smtpmail-debug-toggle ()
  "Troubleshooting command should update both SMTP debug variables."
  (let ((cj/smtpmail-debug-enabled nil)
        (smtpmail-debug-info nil))
    (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
      (cj/set-smtpmail-debug t)
      (should cj/smtpmail-debug-enabled)
      (should smtpmail-debug-info)
      (cj/set-smtpmail-debug nil)
      (should-not cj/smtpmail-debug-enabled)
      (should-not smtpmail-debug-info))))

(ert-deftest test-mail-config-transport-msmtp-present-configures-sendmail ()
  "When msmtp exists, sendmail transport variables should be configured."
  (test-mail-config--with-executables '(("msmtp" . "/usr/bin/msmtp"))
    (let (sendmail-program send-mail-function message-send-mail-function
                           message-sendmail-envelope-from)
      (cj/mail-configure-smtpmail)
      (should (equal sendmail-program "/usr/bin/msmtp"))
      (should (eq send-mail-function 'message-send-mail-with-sendmail))
      (should (eq message-send-mail-function 'message-send-mail-with-sendmail))
      (should (eq message-sendmail-envelope-from 'header))
      (should-not test-mail-config--warnings))))

(ert-deftest test-mail-config-transport-msmtp-missing-warns-and-disables-program ()
  "When msmtp is missing, sendmail-program should be nil and a warning emitted."
  (test-mail-config--with-executables nil
    (let ((sendmail-program "/old/msmtp"))
      (cj/mail-configure-smtpmail)
      (should-not sendmail-program)
      (should (equal test-mail-config--warnings
                     '((mail-config . "msmtp not found; SMTP mail sending unavailable")))))))

(ert-deftest test-mail-config-transport-msmtp-missing-sets-descriptive-fallback ()
  "Error: with msmtp absent, the send functions get a descriptive fallback.
The old behavior left `message-send-mail-function' nil (the top-level defvar
pre-empts message.el's default), so the first send died with \"invalid
function: nil\".  The fallback must be installed on both send variables and
must signal a `user-error' that names msmtp."
  (test-mail-config--with-executables nil
    (let (send-mail-function message-send-mail-function)
      (cj/mail-configure-smtpmail)
      (should (eq send-mail-function #'cj/mail--send-mail-unavailable))
      (should (eq message-send-mail-function #'cj/mail--send-mail-unavailable))
      (should-error (cj/mail--send-mail-unavailable) :type 'user-error)
      (condition-case err
          (cj/mail--send-mail-unavailable)
        (user-error
         (should (string-match-p "msmtp" (cadr err))))))))

(ert-deftest test-mail-config-transport-mbsync-present-builds-command ()
  "When mbsync exists, build the mu4e sync command."
  (test-mail-config--with-executables '(("mbsync" . "/usr/bin/mbsync"))
    (should (equal (cj/mail--mbsync-command) "/usr/bin/mbsync -a"))
    (should-not test-mail-config--warnings)))

(ert-deftest test-mail-config-transport-mbsync-path-with-spaces-is-quoted ()
  "The mu4e sync command should quote unusual executable paths."
  (test-mail-config--with-executables '(("mbsync" . "/opt/mail tools/mbsync"))
    (should (equal (cj/mail--mbsync-command) "/opt/mail\\ tools/mbsync -a"))))

(ert-deftest test-mail-config-transport-mbsync-missing-returns-nil-and-warns ()
  "When mbsync is missing, no unusable sync command should be assigned."
  (test-mail-config--with-executables nil
    (should-not (cj/mail--mbsync-command))
    (should (equal test-mail-config--warnings
                   '((mail-config . "mbsync not found; mu4e mail synchronization unavailable"))))))

(provide 'test-mail-config-transport)
;;; test-mail-config-transport.el ends here
