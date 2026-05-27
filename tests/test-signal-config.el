;;; test-signal-config.el --- Tests for signal-config -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for the pure helper layer of `signal-config': contact-list
;; parsing for the contact picker, and the notify-when-not-viewing
;; predicate.  These need neither signal-cli nor a linked account.

;;; Code:

(require 'ert)
(require 'json)
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

(provide 'test-signal-config)
;;; test-signal-config.el ends here
