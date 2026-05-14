;;; test-mail-config-helpers.el --- Tests for mail-config small helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover transport setup + the email keymap.  This file
;; covers the three small command helpers:
;;
;;   cj/toggle-smtpmail-debug
;;   cj/disable-auto-composition
;;   cj/mu4e-mark-all-headers

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mail-config)

;; Top-level defvar so let-bindings reach the dynamic var under
;; lexical scope.
(defvar smtpmail-debug-info nil)

;;; cj/toggle-smtpmail-debug

(ert-deftest test-mail-config-toggle-smtpmail-debug-flips-from-off ()
  "Normal: toggle from disabled flips both flags on."
  (let ((cj/smtpmail-debug-enabled nil)
        (smtpmail-debug-info nil))
    (cl-letf (((symbol-function 'message) #'ignore))
      (cj/toggle-smtpmail-debug))
    (should cj/smtpmail-debug-enabled)
    (should smtpmail-debug-info)))

(ert-deftest test-mail-config-toggle-smtpmail-debug-flips-from-on ()
  "Normal: toggle from enabled flips both flags off."
  (let ((cj/smtpmail-debug-enabled t)
        (smtpmail-debug-info t))
    (cl-letf (((symbol-function 'message) #'ignore))
      (cj/toggle-smtpmail-debug))
    (should-not cj/smtpmail-debug-enabled)
    (should-not smtpmail-debug-info)))

;;; cj/disable-auto-composition

(ert-deftest test-mail-config-disable-auto-composition-turns-off ()
  "Normal: disable-auto-composition invokes auto-composition-mode with -1."
  (let (received-arg)
    (cl-letf (((symbol-function 'auto-composition-mode)
               (lambda (arg) (setq received-arg arg))))
      (cj/disable-auto-composition))
    (should (eq received-arg -1))))

;;; cj/mu4e-mark-all-headers

(ert-deftest test-mail-config-mark-all-headers-passes-something-and-true ()
  "Normal: mark-all-headers delegates to `mu4e-headers-mark-for-each-if'
with the something tag and a predicate that always returns non-nil."
  (let (mark-arg predicate-arg)
    (cl-letf (((symbol-function 'mu4e-headers-mark-for-each-if)
               (lambda (mark pred)
                 (setq mark-arg mark
                       predicate-arg pred))))
      (cj/mu4e-mark-all-headers))
    (should (equal mark-arg '(something)))
    (should (functionp predicate-arg))
    (should (funcall predicate-arg 'any-msg 'any-param))
    (should (funcall predicate-arg nil nil))))

(provide 'test-mail-config-helpers)
;;; test-mail-config-helpers.el ends here
