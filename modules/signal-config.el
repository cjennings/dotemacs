;;; signal-config.el --- Signal client (forked signel) configuration -*- lexical-binding: t -*-

;;; Commentary:
;; cj/-namespaced configuration and helpers layered on the forked `signel'
;; package, a Signal client that drives signal-cli over JSON-RPC.
;;
;; This file currently holds the pure, signal-cli-independent helper layer
;; that the fork edits and `use-package' wiring build on:
;;   - contact-list parsing for a completing-read contact picker, and
;;   - the predicate that suppresses a notification for the chat the user
;;     is actively viewing.
;; Both are unit-tested without a linked account.  The use-package wiring,
;; keybindings, and the signel fork edits that call these helpers land once
;; signal-cli is installed and the device is linked.

;;; Code:

(require 'seq)

(defun cj/signal--jstr (value)
  "Return VALUE if it is a non-blank string, else nil.
Normalizes a JSON field that may arrive as nil, the empty string, or a
null sentinel symbol into a plain string-or-nil."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))
       value))

(defun cj/signal--combine-name (given family)
  "Join GIVEN and FAMILY name parts into a trimmed full name, or nil.
Either part may be nil, the empty string, or a JSON null sentinel."
  (let ((parts (delq nil (list (cj/signal--jstr given) (cj/signal--jstr family)))))
    (cj/signal--jstr (mapconcat #'identity parts " "))))

(defun cj/signal--contact-display-name (contact)
  "Return a display name for CONTACT, or nil when none is set.
CONTACT is one entry alist from signal-cli `listContacts'.  Picks the
first set source in priority order: the nickname (combined nickName, or
nickGivenName+nickFamilyName), the stored contact name, the top-level
givenName+familyName, the profile givenName+familyName, then username.
signal-cli 0.14 puts givenName/familyName at the top level; the profile
sub-object's name fields are usually null, so it is the deeper fallback."
  (let ((profile (alist-get 'profile contact)))
    (seq-find
     #'cj/signal--jstr
     (list (cj/signal--jstr (alist-get 'nickName contact))
           (cj/signal--combine-name (alist-get 'nickGivenName contact)
                                    (alist-get 'nickFamilyName contact))
           (cj/signal--jstr (alist-get 'name contact))
           (cj/signal--combine-name (alist-get 'givenName contact)
                                    (alist-get 'familyName contact))
           (cj/signal--combine-name (alist-get 'givenName profile)
                                    (alist-get 'familyName profile))
           (cj/signal--jstr (alist-get 'username contact))))))

(defun cj/signal--parse-contacts (result)
  "Parse RESULT from signal-cli `listContacts' into a completing-read alist.
RESULT is the JSON-RPC result value: a sequence (list or vector) of
contact alists.  Returns an alist of (LABEL . RECIPIENT) sorted by LABEL,
where RECIPIENT is the contact's phone number (falling back to its UUID)
and LABEL is \"Name (recipient)\" when a name is known, or the bare
recipient otherwise.  Contacts with no usable recipient are dropped."
  (let (pairs)
    (dolist (contact (append result nil))
      (let ((recipient (or (cj/signal--jstr (alist-get 'number contact))
                           (cj/signal--jstr (alist-get 'uuid contact))))
            (name (cj/signal--contact-display-name contact)))
        (when recipient
          (push (cons (if name (format "%s (%s)" name recipient) recipient)
                      recipient)
                pairs))))
    (sort pairs (lambda (a b) (string-lessp (car a) (car b))))))

(defun cj/signal--chat-buffer-name (id)
  "Return the chat buffer name `signel' uses for chat ID."
  (format "*Signel: %s*" id))

(defun cj/signal--suppress-notify-p (chat-id viewing-buffer-name frame-focused)
  "Return non-nil when a notification for CHAT-ID should be suppressed.
Suppress only while the user is actively viewing that chat: the chat
buffer named by `cj/signal--chat-buffer-name' is VIEWING-BUFFER-NAME and
FRAME-FOCUSED is non-nil.  A nil VIEWING-BUFFER-NAME or an unfocused
frame never suppresses."
  (and frame-focused
       (stringp viewing-buffer-name)
       (string= viewing-buffer-name (cj/signal--chat-buffer-name chat-id))))

(defun cj/signal--frame-focused-p ()
  "Return non-nil when the selected frame currently has input focus.
Treats an unknown focus state as focused."
  (if (fboundp 'frame-focus-state)
      (let ((state (frame-focus-state)))
        (if (eq state 'unknown) t state))
    t))

(defun cj/signal--should-notify-p (chat-id)
  "Return non-nil when an incoming message for CHAT-ID should notify.
Notify unless the user is actively viewing that chat in the selected
window of a focused frame."
  (not (cj/signal--suppress-notify-p
        chat-id
        (buffer-name (window-buffer (selected-window)))
        (cj/signal--frame-focused-p))))

;;; signel — fork integration

(defcustom cj/signal-private-config-file
  (expand-file-name "signal-config.local.el" user-emacs-directory)
  "Private signal-config file, loaded when readable.
This is the place to set `signel-account' to the linked phone number so
the number stays out of the version-controlled (and publicly mirrored)
config.  A phone number is an identifier rather than a credential, so it
lives here rather than in authinfo, which avoids a GPG prompt at connect
time."
  :type 'file
  :group 'signel)

(use-package signel
  :load-path "~/code/signel"
  :ensure nil
  :commands (signel-start signel-stop signel-chat signel-dashboard)
  :custom
  ;; Don't let an incoming message steal a window by auto-popping its chat
  ;; buffer; surface arrivals through notifications instead (see child task
  ;; "Notify only for the unviewed conversation").
  (signel-auto-open-buffer nil)
  :config
  (when (file-readable-p cj/signal-private-config-file)
    (load cj/signal-private-config-file nil t)))

(provide 'signal-config)
;;; signal-config.el ends here
