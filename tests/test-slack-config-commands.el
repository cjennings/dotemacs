;;; test-slack-config-commands.el --- Tests for slack-config commands + helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-slack-config-reactions.el' covers the
;; `cj/slack--safe-reaction-echo-description' advice and the
;; `cj/slack-message-add-reaction' guard.  This file covers the rest:
;;
;;   cj/slack--get-credential
;;   cj/slack-start
;;   cj/slack-stop
;;   cj/slack--reaction-candidates
;;   cj/slack-select-reaction
;;   cj/slack-notify
;;   cj/slack-test-notify
;;   cj/slack-mark-read-and-bury
;;   cj/slack-close-all-buffers
;;
;; All slack/emms/notify primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'slack-config)

;; Dynamic vars slack would normally own.
(defvar slack-teams nil)
(defvar slack-current-buffer nil)

;;; cj/slack--get-credential

(ert-deftest test-slack-get-credential-returns-string-secret ()
  "Normal: a string secret comes back as-is."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) '((:secret "tok-xoxb")))))
    (should (equal (cj/slack--get-credential "TOKEN") "tok-xoxb"))))

(ert-deftest test-slack-get-credential-unwraps-function-secret ()
  "Normal: a function secret is funcall'd."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) (list (list :secret (lambda () "tok-fn"))))))
    (should (equal (cj/slack--get-credential "TOKEN") "tok-fn"))))

(ert-deftest test-slack-get-credential-returns-nil-when-not-found ()
  "Boundary: no auth-source entry returns nil."
  (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
    (should-not (cj/slack--get-credential "TOKEN"))))

;;; cj/slack-start

(ert-deftest test-slack-start-errors-when-token-missing ()
  "Error: no token signals user-error before reaching slack-register-team."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
            ((symbol-function 'cj/slack--get-credential)
             (lambda (key) (when (equal key "COOKIE") "cookie-val"))))
    (should-error (cj/slack-start) :type 'user-error)))

(ert-deftest test-slack-start-errors-when-cookie-missing ()
  "Error: token present but cookie missing -> user-error."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
            ((symbol-function 'cj/slack--get-credential)
             (lambda (key) (when (equal key "TOKEN") "token-val"))))
    (should-error (cj/slack-start) :type 'user-error)))

(ert-deftest test-slack-start-registers-team-and-starts ()
  "Normal: with both credentials, register-team + slack-start are called."
  (let ((slack-teams nil)
        (registered nil)
        (started nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'cj/slack--get-credential)
               (lambda (k) (if (equal k "TOKEN") "tok" "cookie")))
              ((symbol-function 'slack-register-team)
               (lambda (&rest args) (setq registered args)))
              ((symbol-function 'slack-start)
               (lambda () (setq started t))))
      (cj/slack-start))
    (should registered)
    (should started)
    ;; Token + cookie carried through.
    (should (equal (plist-get registered :token) "tok"))
    (should (equal (plist-get registered :cookie) "cookie"))))

;;; cj/slack-stop

(ert-deftest test-slack-stop-closes-ws-and-messages ()
  "Normal: stop calls slack-ws-close and surfaces a confirmation message."
  (let ((closed nil)
        (msg nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'slack-ws-close)
               (lambda () (setq closed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg (apply #'format fmt args)))))
      (cj/slack-stop))
    (should closed)
    (should (string-match-p "disconnected" msg))))

;;; cj/slack--reaction-candidates

(ert-deftest test-slack-reaction-candidates-includes-other-suffix ()
  "Normal: candidate list ends with the (Other...) escape hatch."
  (let ((cands (cj/slack--reaction-candidates)))
    (should (assoc "Other..." cands))
    (should (eq (cdr (assoc "Other..." cands)) :other))
    ;; First entry comes from the curated list.
    (should (> (length cands) 5))))

;;; cj/slack-select-reaction

(ert-deftest test-slack-select-reaction-returns-emoji-name ()
  "Normal: picking a candidate returns the emoji slug."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_p collection &rest _)
               ;; pick the entry whose cdr is "thumbsup".
               (car (cl-find-if (lambda (cell) (equal (cdr cell) "thumbsup"))
                                collection)))))
    (should (equal (cj/slack-select-reaction 'fake-team) "thumbsup"))))

(ert-deftest test-slack-select-reaction-other-delegates-to-input ()
  "Boundary: picking \"Other...\" calls `slack-message-reaction-input'."
  (let ((called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _c &rest _) "Other..."))
              ((symbol-function 'slack-message-reaction-input)
               (lambda (team) (setq called team) "manual-reaction")))
      (cj/slack-select-reaction 'fake-team))
    (should (eq called 'fake-team))))

;;; cj/slack-notify

(ert-deftest test-slack-notify-fires-process-for-im-message ()
  "Normal: an IM message (not from me) triggers `start-process notify'."
  (let ((proc-args nil))
    (cl-letf (((symbol-function 'slack-message-minep) (lambda (&rest _) nil))
              ((symbol-function 'slack-im-p) (lambda (_) t))
              ((symbol-function 'slack-message-mentioned-p) (lambda (&rest _) nil))
              ((symbol-function 'slack-room-display-name)
               (lambda (&rest _) "alice"))
              ((symbol-function 'slack-message-body)
               (lambda (&rest _) "hello"))
              ((symbol-function 'start-process)
               (lambda (&rest args) (setq proc-args args) 'fake-proc)))
      (cj/slack-notify 'msg 'room 'team))
    (should proc-args)
    ;; "Slack: alice" lands in the title slot; "hello" lands in the body slot.
    (should (cl-find-if (lambda (a) (and (stringp a) (string-match-p "alice" a)))
                        proc-args))
    (should (member "hello" proc-args))))

(ert-deftest test-slack-notify-skips-self-messages ()
  "Boundary: a message I sent doesn't fire a notification."
  (let ((fired nil))
    (cl-letf (((symbol-function 'slack-message-minep) (lambda (&rest _) t))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq fired t) 'fake-proc)))
      (cj/slack-notify 'msg 'room 'team))
    (should-not fired)))

(ert-deftest test-slack-notify-skips-non-im-without-mention ()
  "Boundary: a non-IM, non-mention message doesn't fire."
  (let ((fired nil))
    (cl-letf (((symbol-function 'slack-message-minep) (lambda (&rest _) nil))
              ((symbol-function 'slack-im-p) (lambda (_) nil))
              ((symbol-function 'slack-message-mentioned-p) (lambda (&rest _) nil))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq fired t) 'fake-proc)))
      (cj/slack-notify 'msg 'room 'team))
    (should-not fired)))

;;; cj/slack-test-notify

(ert-deftest test-slack-test-notify-fires-start-process ()
  "Normal: test-notify spawns the notify pipeline."
  (let ((args nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest a) (setq args a) 'fake)))
      (cj/slack-test-notify))
    (should args)
    (should (member "Slack: Test" args))))

;;; cj/slack-mark-read-and-bury

(ert-deftest test-slack-mark-read-and-bury-buries-current-buffer ()
  "Normal: invoking in a Slack buffer calls update-mark-request + bury."
  (let ((slack-current-buffer 'fake-buf)
        (marked nil)
        (buried nil))
    (cl-letf (((symbol-function 'slack-buffer-latest-ts)
               (lambda (_) "1234.5678"))
              ((symbol-function 'slack-buffer-update-mark-request)
               (lambda (_buf ts) (setq marked ts)))
              ((symbol-function 'bury-buffer)
               (lambda (&rest _) (setq buried t))))
      (cj/slack-mark-read-and-bury))
    (should (equal marked "1234.5678"))
    (should buried)))

(ert-deftest test-slack-mark-read-and-bury-without-slack-buffer-just-buries ()
  "Boundary: outside Slack, the function only buries."
  (let ((slack-current-buffer nil)
        (buried nil)
        (marked nil))
    (cl-letf (((symbol-function 'slack-buffer-update-mark-request)
               (lambda (&rest _) (setq marked t)))
              ((symbol-function 'bury-buffer)
               (lambda (&rest _) (setq buried t))))
      (cj/slack-mark-read-and-bury))
    (should-not marked)
    (should buried)))

;;; cj/slack-close-all-buffers

(ert-deftest test-slack-close-all-buffers-counts-and-kills ()
  "Normal: buffers with `slack-current-buffer' local get killed; a
buffer that doesn't carry the local is left alone; the closing message
reports the count.

Asserts identity, not the absolute total -- ERT-internal buffers from
prior tests in the same session can land in `(buffer-list)' and the
function's pure-predicate filter is the contract under test, not the
exact set of buffers Emacs happens to have alive."
  (let ((slack-a (generate-new-buffer "*test-slack-close-a*"))
        (slack-b (generate-new-buffer "*test-slack-close-b*"))
        (plain   (generate-new-buffer "*test-slack-close-plain*"))
        (msg nil)
        (killed-names nil))
    (with-current-buffer slack-a (setq-local slack-current-buffer 'team-a))
    (with-current-buffer slack-b (setq-local slack-current-buffer 'team-b))
    ;; `plain' deliberately has no buffer-local slack-current-buffer.
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) nil))
                  ((symbol-function 'kill-buffer)
                   (lambda (b) (push (buffer-name b) killed-names)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (cj/slack-close-all-buffers))
      (dolist (b (list slack-a slack-b plain))
        (when (buffer-live-p b) (kill-buffer b))))
    (should (member "*test-slack-close-a*" killed-names))
    (should (member "*test-slack-close-b*" killed-names))
    (should-not (member "*test-slack-close-plain*" killed-names))
    (should (string-match-p "^Closed [0-9]+ Slack buffer" msg))))

(provide 'test-slack-config-commands)
;;; test-slack-config-commands.el ends here
