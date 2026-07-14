;;; test-slack-config--notify.el --- Slack notification hardening tests -*- lexical-binding: t; -*-

;;; Commentary:
;; The config audit found `cj/slack-notify' missing signel's hardening: no
;; body truncation (giant toasts), no whitespace collapse, no sound gating,
;; and no `notifications-notify' fallback when the notify script is absent
;; (the raw `start-process' error was swallowed by the condition-case, so
;; the notification silently vanished).  This mirrors signel's shape in
;; place; the shared cj/messenger-notify extraction belongs to the
;; messenger-unification task.
;;
;; The slack package's own predicates (`slack-im-p', `slack-message-minep',
;; `slack-message-mentioned-p') are package boundaries and are mocked; the
;; formatter and routing logic run real.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'slack-config)

;;; ------------------------------ body formatter -----------------------------

(ert-deftest test-slack-config-format-notify-body-collapses-whitespace ()
  "Normal: whitespace runs (including newlines) become single spaces."
  (should (equal (cj/slack--format-notify-body "a  b\nc\t\td")
                 "a b c d")))

(ert-deftest test-slack-config-format-notify-body-truncates-long ()
  "Boundary: text over the max truncates to max length ending in an ellipsis."
  (let ((long (make-string 500 ?x)))
    (let ((formatted (cj/slack--format-notify-body long)))
      (should (= (length formatted) cj/slack--notify-body-max))
      (should (string-suffix-p "…" formatted)))))

(ert-deftest test-slack-config-format-notify-body-empty ()
  "Boundary: empty and whitespace-only input format to the empty string."
  (should (equal (cj/slack--format-notify-body "") ""))
  (should (equal (cj/slack--format-notify-body " \n\t ") "")))

;;; ----------------------------- delivery routing ----------------------------

(ert-deftest test-slack-config-send-notification-script-silent-by-default ()
  "Normal: with the notify script on PATH and sound off, --silent is passed."
  (let ((argv nil) (cj/slack-notify-sound nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (prog &rest _) (when (equal prog "notify") "/bin/notify")))
              ((symbol-function 'start-process)
               (lambda (_name _buf &rest args) (setq argv args))))
      (cj/slack--send-notification "Slack: general" "hello"))
    (should (equal argv '("/bin/notify" "info" "Slack: general" "hello" "--silent")))))

(ert-deftest test-slack-config-send-notification-sound-enabled ()
  "Boundary: with sound enabled, --silent is not passed."
  (let ((argv nil) (cj/slack-notify-sound t))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (prog &rest _) (when (equal prog "notify") "/bin/notify")))
              ((symbol-function 'start-process)
               (lambda (_name _buf &rest args) (setq argv args))))
      (cj/slack--send-notification "Slack: general" "hello"))
    (should (equal argv '("/bin/notify" "info" "Slack: general" "hello")))))

(ert-deftest test-slack-config-send-notification-fallback-without-script ()
  "Error: with no notify script, delivery falls back to notifications-notify."
  (let ((fallback nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'notifications-notify)
               (lambda (&rest args) (setq fallback args))))
      (cj/slack--send-notification "Slack: general" "hello"))
    (should (equal (plist-get fallback :title) "Slack: general"))
    (should (equal (plist-get fallback :body) "hello"))))

;;; --------------------------- predicate wiring ------------------------------

(defmacro test-slack-notify--with-message (minep im-p mentioned-p &rest body)
  "Run BODY with the slack package predicates mocked to the given values.
Also mocks room/body accessors and captures delivery into `sent'."
  (declare (indent 3))
  `(let ((sent nil))
     (cl-letf (((symbol-function 'slack-message-minep) (lambda (&rest _) ,minep))
               ((symbol-function 'slack-im-p) (lambda (&rest _) ,im-p))
               ((symbol-function 'slack-message-mentioned-p) (lambda (&rest _) ,mentioned-p))
               ((symbol-function 'slack-room-display-name) (lambda (&rest _) "general"))
               ((symbol-function 'slack-message-body) (lambda (&rest _) "the message"))
               ((symbol-function 'cj/slack--send-notification)
                (lambda (title body) (setq sent (list title body)))))
       (cj/slack-notify 'msg 'room 'team)
       ,@body)))

(ert-deftest test-slack-config-notify-dm-notifies ()
  "Normal: a DM from someone else raises a notification."
  (test-slack-notify--with-message nil t nil
    (should (equal sent '("Slack: general" "the message")))))

(ert-deftest test-slack-config-notify-mention-notifies ()
  "Normal: an @mention in a channel raises a notification."
  (test-slack-notify--with-message nil nil t
    (should (equal sent '("Slack: general" "the message")))))

(ert-deftest test-slack-config-notify-own-message-silent ()
  "Boundary: your own message never notifies, even in a DM."
  (test-slack-notify--with-message t t t
    (should-not sent)))

(ert-deftest test-slack-config-notify-plain-channel-silent ()
  "Boundary: a channel message with no mention stays silent."
  (test-slack-notify--with-message nil nil nil
    (should-not sent)))

(provide 'test-slack-config--notify)
;;; test-slack-config--notify.el ends here
