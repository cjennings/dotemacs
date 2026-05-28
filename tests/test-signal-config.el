;;; test-signal-config.el --- Tests for signal-config -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for the pure helper layer of `signal-config': contact-list
;; parsing for the contact picker, and the notify-when-not-viewing
;; predicate.  These need neither signal-cli nor a linked account.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

;; signel is the fork at ~/code/signel; signal-config wires it via
;; use-package but the connection-guard/fetch tests need the symbols
;; available directly.
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/code/signel")))
(require 'signel)

(require 'signal-config)

;;; cj/signal--jstr

(ert-deftest test-signal-config-jstr-string ()
  "Normal: a non-blank string passes through unchanged."
  (should (equal (cj/signal--jstr "hi") "hi")))

(ert-deftest test-signal-config-jstr-rejects-nonstrings ()
  "Boundary/Error: nil, empty, whitespace, and non-string sentinels become nil."
  (should-not (cj/signal--jstr nil))
  (should-not (cj/signal--jstr ""))
  (should-not (cj/signal--jstr "   "))
  (should-not (cj/signal--jstr :null))
  (should-not (cj/signal--jstr 42)))

;;; cj/signal--contact-display-name
;; Field priority: nickName, then nickGiven+nickFamily, then top-level name,
;; then top-level given+family, then profile given+family, then username.

(ert-deftest test-signal-config-display-name-prefers-name ()
  "Normal: the top-level combined name wins over the given/family parts."
  (should (equal (cj/signal--contact-display-name
                  '((name . "Alice Anderson") (givenName . "Ali") (familyName . "A")))
                 "Alice Anderson")))

(ert-deftest test-signal-config-display-name-nickname-wins ()
  "Normal: a nickName overrides the contact name."
  (should (equal (cj/signal--contact-display-name
                  '((nickName . "Edster") (name . "Eve Edwards")
                    (givenName . "Eve") (familyName . "Edwards")))
                 "Edster")))

(ert-deftest test-signal-config-display-name-nickname-parts ()
  "Boundary: nickGivenName+nickFamilyName combine when nickName is unset."
  (should (equal (cj/signal--contact-display-name
                  '((nickGivenName . "DJ") (nickFamilyName . "Cool") (name . "Daniel")))
                 "DJ Cool")))

(ert-deftest test-signal-config-display-name-toplevel-given-family ()
  "Boundary: with no name, top-level givenName+familyName combine."
  (should (equal (cj/signal--contact-display-name
                  '((name) (givenName . "Bob") (familyName . "Brown")))
                 "Bob Brown")))

(ert-deftest test-signal-config-display-name-profile-fallback ()
  "Boundary: with no name or top-level parts, profile given/family is the fallback."
  (should (equal (cj/signal--contact-display-name
                  '((name) (givenName) (familyName)
                    (profile . ((givenName . "Carol") (familyName)))))
                 "Carol")))

(ert-deftest test-signal-config-display-name-username-fallback ()
  "Boundary: username is the last name source."
  (should (equal (cj/signal--contact-display-name
                  '((name) (username . "dave.42")))
                 "dave.42")))

(ert-deftest test-signal-config-display-name-none ()
  "Error: no usable name yields nil."
  (should-not (cj/signal--contact-display-name
               '((name) (givenName) (familyName)
                 (profile . ((givenName) (familyName))))))
  (should-not (cj/signal--contact-display-name '((number . "+15551112222")))))

;;; cj/signal--parse-contacts

(defconst test-signal-config--contacts-json
  "[
 {\"number\":\"+15551112222\",\"uuid\":\"uuid-a\",\"name\":\"Alice Anderson\",\"givenName\":\"Alice\",\"familyName\":\"Anderson\",\"nickName\":null,\"nickGivenName\":null,\"nickFamilyName\":null,\"username\":null,\"profile\":{\"givenName\":null,\"familyName\":null}},
 {\"number\":\"+15553334444\",\"uuid\":null,\"name\":null,\"givenName\":\"Bob\",\"familyName\":\"Brown\",\"nickName\":null,\"username\":null,\"profile\":{\"givenName\":null,\"familyName\":null}},
 {\"number\":\"+15555556666\",\"uuid\":\"uuid-c\",\"name\":null,\"givenName\":null,\"familyName\":null,\"nickName\":null,\"username\":null,\"profile\":{\"givenName\":\"Carol\",\"familyName\":null}},
 {\"number\":null,\"uuid\":\"uuid-d\",\"name\":null,\"givenName\":null,\"familyName\":null,\"username\":null,\"profile\":{\"givenName\":null,\"familyName\":null}},
 {\"number\":\"+15557778888\",\"uuid\":\"uuid-e\",\"name\":\"Eve Edwards\",\"givenName\":\"Eve\",\"familyName\":\"Edwards\",\"nickName\":\"Edster\",\"username\":null,\"profile\":{\"givenName\":null,\"familyName\":null}}
]"
  "Synthetic fixture mirroring the signal-cli 0.14.4.1 `listContacts' shape:
top-level name/givenName/familyName and nickName fields, with a profile
sub-object whose name fields are usually null.  Field layout was confirmed
against a live linked account on 2026-05-26; the values here are fake.")

(ert-deftest test-signal-config-parse-contacts-normal ()
  "Normal: top-level name, top-level parts, profile fallback, uuid fallback, nickname."
  (let* ((result (json-read-from-string test-signal-config--contacts-json))
         (pairs (cj/signal--parse-contacts result)))
    (should (equal pairs
                   '(("Alice Anderson (+15551112222)" . "+15551112222")
                     ("Bob Brown (+15553334444)" . "+15553334444")
                     ("Carol (+15555556666)" . "+15555556666")
                     ("Edster (+15557778888)" . "+15557778888")
                     ("uuid-d" . "uuid-d"))))))

(ert-deftest test-signal-config-parse-contacts-empty ()
  "Boundary: an empty result yields nil for both vector and nil input."
  (should-not (cj/signal--parse-contacts []))
  (should-not (cj/signal--parse-contacts nil)))

(ert-deftest test-signal-config-parse-contacts-accepts-list-and-vector ()
  "Boundary: vector and list result sequences parse identically."
  (let ((entry '((number . "+15551112222") (name . "Al"))))
    (should (equal (cj/signal--parse-contacts (vector entry))
                   (cj/signal--parse-contacts (list entry))))))

(ert-deftest test-signal-config-parse-contacts-drops-recipientless ()
  "Error: a contact with neither number nor uuid is dropped."
  (should-not (cj/signal--parse-contacts
               (list '((name . "Ghost") (number) (uuid))))))

;;; cj/signal--suppress-notify-p

(ert-deftest test-signal-config-suppress-when-viewing-focused ()
  "Normal: viewing the chat buffer with focus suppresses the notification."
  (should (cj/signal--suppress-notify-p
           "+15551112222" "*Signel: +15551112222*" t)))

(ert-deftest test-signal-config-no-suppress-other-buffer ()
  "Boundary: a different selected buffer does not suppress."
  (should-not (cj/signal--suppress-notify-p
               "+15551112222" "*scratch*" t)))

(ert-deftest test-signal-config-no-suppress-unfocused ()
  "Boundary: viewing the chat but with the frame unfocused still notifies."
  (should-not (cj/signal--suppress-notify-p
               "+15551112222" "*Signel: +15551112222*" nil)))

(ert-deftest test-signal-config-no-suppress-nil-viewing ()
  "Error: a nil viewing-buffer name does not suppress."
  (should-not (cj/signal--suppress-notify-p "+15551112222" nil t)))

;;; cj/signel--ensure-started

(ert-deftest test-signal-config-ensure-started-live-process-noop ()
  "Normal: with a live signel process, ensure-started returns without
calling `signel-start' or the pre-warm fetch."
  (let ((start-called nil)
        (fetch-called nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'get-process) (lambda (_) 'fake-proc))
              ((symbol-function 'signel-start)
               (lambda () (setq start-called t)))
              ((symbol-function 'cj/signel--fetch-contacts)
               (lambda (&rest _) (setq fetch-called t))))
      (cj/signel--ensure-started)
      (should-not start-called)
      (should-not fetch-called))))

(ert-deftest test-signal-config-ensure-started-starts-when-account-set ()
  "Normal: with `signel-account' set and no live process, ensure-started
calls `signel-start' to bring the daemon up."
  (let ((start-called nil)
        (signel-account "+15555550100"))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'get-process) (lambda (_) nil))
              ((symbol-function 'signel-start)
               (lambda () (setq start-called t)))
              ((symbol-function 'cj/signel--fetch-contacts)
               (lambda (&rest _) nil)))
      (cj/signel--ensure-started)
      (should start-called))))

(ert-deftest test-signal-config-ensure-started-prewarms-on-start ()
  "Normal: when ensure-started actually starts the daemon, it triggers a
pre-warm fetch so the picker cache is warm on first invocation."
  (let ((fetch-called nil)
        (signel-account "+15555550100"))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'get-process) (lambda (_) nil))
              ((symbol-function 'signel-start) (lambda () nil))
              ((symbol-function 'cj/signel--fetch-contacts)
               (lambda (&rest _) (setq fetch-called t))))
      (cj/signel--ensure-started)
      (should fetch-called))))

(ert-deftest test-signal-config-ensure-started-errors-when-no-account ()
  "Error: with `signel-account' nil, ensure-started signals a user-error
naming the remedy (set the account in the private config) instead of
starting an account-less daemon."
  (let ((signel-account nil))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'get-process) (lambda (_) nil)))
      (should-error (cj/signel--ensure-started) :type 'user-error))))

(ert-deftest test-signal-config-ensure-started-requires-signel-first ()
  "Error: ensure-started must `require' signel BEFORE reading any of
its private variables, so a `void-variable' error cannot fire when
called before signel has been autoloaded.  Captures the bug where
`signel--process-name' was forward-declared in signal-config but not
yet bound at runtime because the `use-package' autoload had not fired.

Asserts ordering, not just presence: a future refactor that moves the
`require' below the `cond' would still execute the require eventually
but the variable read in the cond would fire void-variable first.
The test fails if `require' isn't the first call inside the function."
  (let ((call-order nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push (list 'require feature) call-order)
                 t))
              ((symbol-function 'process-live-p)
               (lambda (_) (push 'process-live-p call-order) t))
              ((symbol-function 'get-process)
               (lambda (_) (push 'get-process call-order) 'fake-proc)))
      (cj/signel--ensure-started)
      (should (equal (car (reverse call-order)) '(require signel))))))

;;; cj/signel--fetch-contacts + cj/signel--contact-cache

(ert-deftest test-signal-config-fetch-contacts-issues-list-contacts-rpc ()
  "Normal: fetch-contacts sends a `listContacts' RPC and registers a
success callback so the response routes back."
  (let (sent-method sent-callback)
    (cl-letf (((symbol-function 'signel--send-rpc)
               (lambda (method _params _target callback)
                 (setq sent-method method
                       sent-callback callback)
                 1)))
      (cj/signel--fetch-contacts))
    (should (equal sent-method "listContacts"))
    (should (functionp sent-callback))))

(ert-deftest test-signal-config-fetch-contacts-callback-populates-cache ()
  "Normal: on a successful result, the callback parses the contact list
and stores the (LABEL . RECIPIENT) alist in `cj/signel--contact-cache'."
  (let (sent-callback)
    (cl-letf (((symbol-function 'signel--send-rpc)
               (lambda (_method _params _target callback)
                 (setq sent-callback callback) 1)))
      (setq cj/signel--contact-cache nil)
      (cj/signel--fetch-contacts)
      (funcall sent-callback
               [((number . "+15555550100") (givenName . "Alice"))]))
    (should (equal cj/signel--contact-cache
                   '(("Alice (+15555550100)" . "+15555550100"))))))

(ert-deftest test-signal-config-fetch-contacts-empty-result-clears-cache ()
  "Boundary: an empty listContacts result populates the cache as nil,
distinct from a failure path (which never invokes the success callback)."
  (let (sent-callback)
    (cl-letf (((symbol-function 'signel--send-rpc)
               (lambda (_method _params _target callback)
                 (setq sent-callback callback) 1)))
      (setq cj/signel--contact-cache '(("stale" . "+10000000000")))
      (cj/signel--fetch-contacts)
      (funcall sent-callback []))
    (should-not cj/signel--contact-cache)))

;;; cj/signel-refresh-contacts

(ert-deftest test-signal-config-refresh-contacts-clears-and-refetches ()
  "Normal: `cj/signel-refresh-contacts' clears the cache and triggers a
fresh fetch so a stale entry can't survive a user-driven refresh."
  (let ((fetch-called nil))
    (setq cj/signel--contact-cache '(("stale" . "+10000000000")))
    (cl-letf (((symbol-function 'cj/signel--fetch-contacts)
               (lambda (&rest _) (setq fetch-called t))))
      (cj/signel-refresh-contacts))
    (should-not cj/signel--contact-cache)
    (should fetch-called)))

;;; cj/signel-message picker

(ert-deftest test-signal-config-message-warm-cache-picks-contact ()
  "Normal: with a warm cache, picking a contact label opens that
recipient's chat buffer."
  (let ((chosen-recipient nil)
        (signel-account "+15555550100"))
    (setq cj/signel--contact-cache
          '(("Alice (+15555550200)" . "+15555550200")))
    (cl-letf (((symbol-function 'cj/signel--ensure-started) (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Alice (+15555550200)"))
              ((symbol-function 'signel-chat)
               (lambda (r) (setq chosen-recipient r))))
      (cj/signel-message))
    (should (equal chosen-recipient "+15555550200"))))

(ert-deftest test-signal-config-message-warm-cache-picks-note-to-self ()
  "Normal: the pinned `Note to Self' entry resolves to `signel-account'
so a self-message lands in the Signal Note-to-Self thread."
  (let ((chosen-recipient nil)
        (signel-account "+15555550100"))
    (setq cj/signel--contact-cache
          '(("Alice (+15555550200)" . "+15555550200")))
    (cl-letf (((symbol-function 'cj/signel--ensure-started) (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Note to Self"))
              ((symbol-function 'signel-chat)
               (lambda (r) (setq chosen-recipient r))))
      (cj/signel-message))
    (should (equal chosen-recipient "+15555550100"))))

(ert-deftest test-signal-config-message-cold-cache-fetch-resolves-in-time ()
  "Normal: cold cache, fetch's after-callback fires inside the bounded
wait, picker proceeds with the now-warm cache."
  (let ((chosen-recipient nil)
        (signel-account "+15555550100")
        (cj/signel-fetch-timeout 1.0))
    (setq cj/signel--contact-cache nil)
    (cl-letf (((symbol-function 'cj/signel--ensure-started) (lambda () nil))
              ((symbol-function 'cj/signel--fetch-contacts)
               (lambda (&optional after-cb)
                 (setq cj/signel--contact-cache
                       '(("Bob (+15555550300)" . "+15555550300")))
                 (when after-cb (funcall after-cb))))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Bob (+15555550300)"))
              ((symbol-function 'signel-chat)
               (lambda (r) (setq chosen-recipient r))))
      (cj/signel-message))
    (should (equal chosen-recipient "+15555550300"))))

(ert-deftest test-signal-config-message-cold-cache-timeout-errors ()
  "Error: cold cache, fetch never resolves, picker user-errors before
the bounded wait would let Emacs hang on a dead daemon."
  (let ((signel-account "+15555550100")
        (cj/signel-fetch-timeout 0.1))
    (setq cj/signel--contact-cache nil)
    (cl-letf (((symbol-function 'cj/signel--ensure-started) (lambda () nil))
              ((symbol-function 'cj/signel--fetch-contacts)
               (lambda (&rest _) nil)))
      (should-error (cj/signel-message) :type 'user-error))))

;;; cj/signel-message-self

(ert-deftest test-signal-config-message-self-calls-signel-chat-with-account ()
  "Normal: the direct self-message command opens a chat buffer addressed
to `signel-account', skipping the picker entirely."
  (let ((chosen-recipient nil)
        (signel-account "+15555550100"))
    (cl-letf (((symbol-function 'cj/signel--ensure-started) (lambda () nil))
              ((symbol-function 'signel-chat)
               (lambda (r) (setq chosen-recipient r))))
      (cj/signel-message-self))
    (should (equal chosen-recipient "+15555550100"))))

;;; cj/signel-prefix-map (C-; M)

(ert-deftest test-signal-config-prefix-map-has-expected-bindings ()
  "Normal: the signel C-; M prefix map binds m / s / d / q / SPC to the
commands the workflow spec names."
  (should (eq (keymap-lookup cj/signel-prefix-map "m")
              #'cj/signel-message))
  (should (eq (keymap-lookup cj/signel-prefix-map "s")
              #'cj/signel-message-self))
  (should (eq (keymap-lookup cj/signel-prefix-map "d")
              #'signel-dashboard))
  (should (eq (keymap-lookup cj/signel-prefix-map "q")
              #'signel-stop))
  (should (eq (keymap-lookup cj/signel-prefix-map "SPC")
              #'cj/signel-connect)))

;;; display-buffer-alist entry for *Signel: ...* chat buffers

(ert-deftest test-signal-config-chat-buffer-display-rule-uses-bottom-30 ()
  "Normal: signal-config registers a `display-buffer-alist' entry that
matches `*Signel: <id>*' buffers, routes them through
`display-buffer-at-bottom', and sets `window-height' to 0.3 so the
chat docks to the bottom 30% of the frame."
  (let ((entry (seq-find (lambda (e) (equal (car e) "\\`\\*Signel: "))
                         display-buffer-alist)))
    (should entry)
    (should (memq 'display-buffer-at-bottom (cadr entry)))
    (should (equal 0.3 (cdr (assq 'window-height (cddr entry)))))))

(ert-deftest test-signal-config-chat-buffer-display-rule-matches-buffer-name ()
  "Boundary: the registered regex matches a realistic chat buffer name
\(phone-number id and group-id) and does not match unrelated buffers."
  (let* ((entry (seq-find (lambda (e) (equal (car e) "\\`\\*Signel: "))
                          display-buffer-alist))
         (regex (car entry)))
    (should regex)
    (should (string-match-p regex "*Signel: +15555550100*"))
    (should (string-match-p regex "*Signel: groupid-abc*"))
    (should-not (string-match-p regex "*signel-log*"))
    (should-not (string-match-p regex "scratch"))))

(provide 'test-signal-config)
;;; test-signal-config.el ends here
