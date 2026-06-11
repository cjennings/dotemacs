;;; test-signel-notify-function.el --- Tests for signel's notify-function dispatch -*- lexical-binding: t -*-

;;; Commentary:
;; signel's receive handler (signel.el in the fork at ~/code/signel)
;; raised notifications through a hardwired `notifications-notify'
;; call.  The notification slice (docs/design/signal-client.org,
;; "Notification slice" addendum) replaces that with
;; `signel-notify-function', a customization point called with
;; CHAT-ID, SENDER, and BODY so a config layer can add suppression or
;; route through an external notifier.  These tests cover the
;; dispatch: text, sticker, and attachment bodies reach the function
;; with the right arguments, and the default preserves the plain
;; `notifications-notify' behavior.
;;
;; `signel--handle-receive' is exercised directly with synthetic
;; envelope alists; buffer/dashboard side effects are stubbed.  No
;; live process needed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/code/signel")))
(require 'signel)

(defun test-signel-notify--receive (envelope)
  "Run `signel--handle-receive' on ENVELOPE, capturing notify calls.
Returns the list of (CHAT-ID SENDER BODY) argument lists the handler
passed to `signel-notify-function', oldest first.  Buffer and
dashboard side effects are stubbed out."
  (let (calls)
    (cl-letf (((symbol-function 'signel--insert-msg) (lambda (&rest _) nil))
              ((symbol-function 'signel--dashboard-refresh) (lambda () nil))
              ((symbol-function 'signel--get-buffer)
               (lambda (_) (current-buffer))))
      (let ((signel-notify-function
             (lambda (chat-id sender body)
               (push (list chat-id sender body) calls)))
            (signel-auto-open-buffer nil))
        (signel--handle-receive `((envelope . ,envelope)))))
    (nreverse calls)))

(ert-deftest test-signel-notify-function-text-message ()
  "Normal: a text dataMessage calls the function with chat-id, sender, text."
  (should (equal (test-signel-notify--receive
                  '((sourceNumber . "+15551234567")
                    (sourceName . "Alice")
                    (dataMessage . ((message . "hi there")))))
                 '(("+15551234567" "Alice" "hi there")))))

(ert-deftest test-signel-notify-function-sticker-placeholder ()
  "Boundary: a sticker with no text gets the [Sticker] placeholder body."
  (should (equal (test-signel-notify--receive
                  '((sourceNumber . "+15551234567")
                    (sourceName . "Alice")
                    (dataMessage . ((sticker . ((packId . "p1")))))))
                 '(("+15551234567" "Alice" "[Sticker]")))))

(ert-deftest test-signel-notify-function-attachment-placeholder ()
  "Boundary: an attachment with no text gets the [Attachment] placeholder."
  (should (equal (test-signel-notify--receive
                  '((sourceNumber . "+15551234567")
                    (sourceName . "Alice")
                    (dataMessage . ((attachments . [((id . "a1"))])))))
                 '(("+15551234567" "Alice" "[Attachment]")))))

(ert-deftest test-signel-notify-function-no-data-no-call ()
  "Boundary: an envelope with no dataMessage never calls the function."
  (should-not (test-signel-notify--receive
               '((sourceNumber . "+15551234567")
                 (sourceName . "Alice")
                 (typingMessage . ((action . "STARTED")))))))

(ert-deftest test-signel-notify-function-default-preserves-behavior ()
  "Normal: the default value raises a plain notifications-notify toast."
  (should (eq signel-notify-function #'signel--notify-default))
  (let (calls)
    (cl-letf (((symbol-function 'notifications-notify)
               (lambda (&rest args) (push args calls) nil)))
      (signel--notify-default "+15551234567" "Alice" "hi"))
    (should (= (length calls) 1))
    (let ((args (car calls)))
      (should (equal (plist-get args :title) "Signel: Alice"))
      (should (equal (plist-get args :body) "hi")))))

(provide 'test-signel-notify-function)
;;; test-signel-notify-function.el ends here
