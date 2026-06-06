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
(require 'keybindings)  ;; provides cj/custom-keymap + cj/register-prefix-map

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

;; Chat buffers (named `*Signel: <id>*') open in the bottom 30% of the
;; frame rather than wherever display-buffer's fallback rule picks.
;; The fork's `signel-chat' uses `pop-to-buffer', so this entry applies.
(add-to-list
 'display-buffer-alist
 '("\\`\\*Signel: "
   (display-buffer-reuse-window display-buffer-at-bottom)
   (window-height . 0.3)
   (reusable-frames . nil)))

;;; Connection guard, contact fetch, and cache

;; Forward declarations: signel.el is loaded by the use-package above (with
;; :load-path on the fork), but the byte-compiler doesn't see those symbols
;; statically.  Declaring them keeps the compile clean without changing
;; runtime behavior.
(defvar signel-account)
(defvar signel--process-name)
(declare-function signel-start "signel" ())
(declare-function signel--send-rpc "signel" (method params &optional target-buffer success-callback))

(defvar cj/signel--contact-cache nil
  "Cached `(LABEL . RECIPIENT)' alist for the contact picker.
Populated by `cj/signel--fetch-contacts' on first invocation (or after a
`cj/signel-refresh-contacts'), and cleared on `signel-stop' / restart so
a stale list can't survive a reconnect.  In-memory only.")

(defcustom cj/signel-fetch-timeout 3.0
  "Seconds the picker blocks on `accept-process-output' for a cold-cache fetch.
On warm cache the picker opens instantly; on cold cache it kicks off a
fetch and waits up to this many seconds for the RPC result before
reporting a `user-error' so a dead or wedged daemon can't hang Emacs."
  :type 'number
  :group 'signel)

(defun cj/signel--ensure-started ()
  "Ensure the signel daemon is live, starting it if needed.
Three branches:
- The process is already live -- no-op, return nil.
- `signel-account' is set but no live process exists -- call `signel-start'
  and pre-warm the contact cache with a background `listContacts' fetch so
  the picker is instant on first use.
- `signel-account' is nil -- `user-error' naming the remedy (set the
  account in `cj/signal-private-config-file').

If startup launches but the RPC handshake exits before the first response,
the subsequent `signel--send-rpc' call (in the pre-warm or any later
fetch) signals through its own error path; check =*signel-log*= and
=*signel-stderr*= for detail and link the account manually.

Loads the `signel' feature explicitly before reading any of its
private variables: the use-package above autoloads only on
`signel-start' / `signel-stop' / `signel-chat' / `signel-dashboard',
so without this require the first branch's read of `signel--process-name'
fires a void-variable error before the autoload would trigger."
  (require 'signel)
  (cond
   ((process-live-p (get-process signel--process-name))
    nil)
   ((null signel-account)
    (user-error
     "signel-account is unset.  Set it in %s (or your private config) and link the device manually with `signal-cli link', then retry"
     cj/signal-private-config-file))
   (t
    (signel-start)
    (cj/signel--fetch-contacts))))

(defun cj/signel--fetch-contacts (&optional after-callback)
  "Fetch the contact list from signal-cli and populate `cj/signel--contact-cache'.
Issues a `listContacts' RPC and registers a success callback that runs
the result through `cj/signal--parse-contacts' (the verified parser) and
stores the resulting `(LABEL . RECIPIENT)' alist in the cache.  An empty
result populates the cache as nil; a failure goes through the dispatch
error path and never invokes the callback, so the prior cache survives.

AFTER-CALLBACK, when non-nil, is invoked with no arguments after the
cache has been populated -- the picker uses this to unblock its
bounded-wait on cold caches."
  (signel--send-rpc
   "listContacts" nil nil
   (lambda (result)
     (setq cj/signel--contact-cache (cj/signal--parse-contacts result))
     (when after-callback (funcall after-callback)))))

(defun cj/signel-refresh-contacts ()
  "Clear the picker's contact cache and refetch it from signal-cli.
Use when a contact added or renamed on the phone hasn't shown up in the
picker yet; this forces a fresh `listContacts' rather than reading the
cached snapshot."
  (interactive)
  (setq cj/signel--contact-cache nil)
  (cj/signel--fetch-contacts))

;;; Picker, self-message, and connect

(declare-function signel-chat "signel" (recipient))
(declare-function signel-dashboard "signel" ())
(declare-function signel-stop "signel" ())

(defun cj/signel-connect ()
  "Connect to signal-cli, starting the daemon if needed.
Thin interactive wrapper around `cj/signel--ensure-started' so the
keymap has a friendly verb to bind."
  (interactive)
  (cj/signel--ensure-started)
  (message "Signel connected."))

(defun cj/signel-message ()
  "Pick a Signal contact by name and open the chat buffer.
Ensures the daemon is connected first (auto-starts and pre-warms on
cold start, or errors with the remedy if the account isn't set).  Uses
the cached contact list when warm; on a cold cache, kicks off a fetch
and waits up to `cj/signel-fetch-timeout' seconds for the result before
raising a `user-error' so a dead daemon can't hang Emacs.  The picker
offers a pinned \"Note to Self\" entry plus every Signal contact, and
opens the chosen recipient in `signel-chat'."
  (interactive)
  (cj/signel--ensure-started)
  (unless cj/signel--contact-cache
    (let ((done nil)
          (deadline (+ (float-time) cj/signel-fetch-timeout)))
      (cj/signel--fetch-contacts (lambda () (setq done t)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.1))
      (unless done
        (user-error
         "Signal contact fetch timed out after %.1fs; try again or run M-x cj/signel-refresh-contacts (see *signel-log* for detail)"
         cj/signel-fetch-timeout))))
  (let* ((note-self (cons "Note to Self" signel-account))
         (candidates (cons note-self cj/signel--contact-cache))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (display-sort-function . identity)
                        (cycle-sort-function . identity))
                    (complete-with-action action candidates string pred))))
         (label (completing-read "Signal recipient: " table nil t))
         (recipient (cdr (assoc label candidates))))
    (when recipient
      (signel-chat recipient))))

(defun cj/signel-message-self ()
  "Open a Signal chat buffer addressed to Note to Self.
Resolves to `signel-account' (the linked phone number).  Sending to it
lands in the Signal Note-to-Self thread on the phone; manual-verify
that on first use."
  (interactive)
  (cj/signel--ensure-started)
  (unless signel-account
    (user-error "signel-account is unset; cannot send to self"))
  (signel-chat signel-account))

(defvar cj/signel-prefix-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "m" #'cj/signel-message)
    (keymap-set map "s" #'cj/signel-message-self)
    (keymap-set map "d" #'signel-dashboard)
    (keymap-set map "q" #'signel-stop)
    (keymap-set map "SPC" #'cj/signel-connect)
    map)
  "Signel \"Messages\" prefix keymap, bound under `C-; M'.
Leaves =l= unbound for now -- the future =cj/signel-link= command lands
in a later pass.  See =docs/design/signal-client.org= scope summary.")

;; Register the messages prefix under C-; M via the documented helper.
;; keybindings.el owns cj/custom-keymap; the (require 'keybindings) above
;; guarantees it is loaded before this runs, so no load-order guard is
;; needed.  This is the same pattern every other feature module uses.
(cj/register-prefix-map "M" cj/signel-prefix-map "signal messages")

(provide 'signal-config)
;;; signal-config.el ends here
