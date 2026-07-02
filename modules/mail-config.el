;;; mail-config.el --- Settings for Mu4e Email -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: daily mail; registers cj/email-map. mu4e/org-msg are heavy, so a
;;   command-loaded deferral candidate for Phase 5.
;; Top-level side effects: registers cj/email-map under cj/custom-keymap, one
;;   add-hook, two advice-add, one global key, package config.
;; Runtime requires: user-constants, system-lib, mu4e-attachments, keybindings.
;; Direct test load: yes (requires keybindings; mu4e package must be present).
;;
;; I found Aime Bertrand's blog post to be an excellent walkthrough of how to
;; setup a Mu4e config.
;;
;; https://macowners.club/posts/email-emacs-mu4e-macos/
;;
;; Attachment saving lives in the `mu4e-attachments' module; its C-; e
;; bindings are wired into `cj/email-map' below.
;;
;; Crash Fix:
;; auto-composition-mode is disabled in mu4e-headers-mode to prevent a
;; HarfBuzz SIGSEGV crash. Email subjects containing emoji trigger glyph
;; shaping via arabic-shape-gstring → hb_shape_full, which segfaults.
;; Disabling composition in headers is safe (no ligatures needed there).
;;
;;; Code:

(require 'user-constants)
(require 'system-lib)
(require 'mu4e-attachments)
(require 'keybindings)  ;; provides cj/custom-keymap

;; cj/custom-keymap's real binding is in keybindings.el, which init.el loads
;; first. The use-package org-msg :preface below wraps in eval-and-compile, so
;; standalone byte-compile evaluates `keymap-set` on it at compile time. This
;; shim defines cj/custom-keymap then; defvar is a no-op once the real keymap
;; is in place at load time.
(eval-and-compile
  (defvar cj/custom-keymap (make-sparse-keymap)))

(defvar smtpmail-debug-info nil)
(defvar sendmail-program nil)
(defvar send-mail-function nil)
(defvar message-send-mail-function nil)
(defvar message-sendmail-envelope-from nil)

(declare-function mu4e-message-field "mu4e-message")

;; ----------------------------- Declarations ----------------------------------
;; mu4e/org-msg load lazily, so the byte-compiler can't see these package
;; functions and variables when this module is compiled standalone.  Declare
;; them to silence free-variable / undefined-function warnings without forcing
;; an eager require (which would defeat lazy loading).  The cj/... entries are
;; forward references: defined later in this file's `:config' block, or in
;; mu4e-org-contacts-integration (required at load time inside that block).

(declare-function mu4e-headers-mark-for-each-if "mu4e-mark")
(declare-function mu4e-search "mu4e-search")
(declare-function mu4e-view-refresh "mu4e-view")
(declare-function message-add-header "message")
(declare-function org-msg-edit-mode "org-msg")
(declare-function no-auto-fill "mail-config")
(declare-function cj/disable-company-in-mu4e-compose "mail-config")
(declare-function cj/disable-ispell-in-email-headers "mail-config")
(declare-function cj/activate-mu4e-org-contacts-integration
                  "mu4e-org-contacts-integration")

;; Package variables assigned in the lazy `:config' blocks below.
(defvar mu4e-compose-keep-self-cc)
(defvar mu4e-root-maildir)
(defvar mu4e-show-images)
(defvar org-msg-extra-css)

;; Refile (archive) target dispatch.  A per-context `mu4e-refile-folder' string
;; is unsafe: mu4e context :vars are sticky, so a value set when one context is
;; active leaks into a later context that doesn't set its own -- archiving one
;; account's mail into another's folder.  A single function evaluated per
;; message at refile time avoids that.  Only cmail has a real synced Archive
;; folder; the Gmail-backed accounts (gmail, dmail) sync no archive maildir, so
;; refiling them would move mail into an unsynced, server-invisible folder
;; (silent loss) -- signal instead.
(defun cj/mu4e--refile-folder-for-maildir (maildir)
  "Return the refile (archive) folder for MAILDIR, or signal when none exists.
MAILDIR is a mu4e :maildir string such as \"/cmail/INBOX\"."
  (cond
   ((not (stringp maildir))
    (user-error "Cannot refile: message has no maildir"))
   ((string-prefix-p "/cmail" maildir) "/cmail/Archive")
   (t
    (user-error "No archive folder syncs for this account; refile disabled to avoid moving mail into an unsynced folder"))))

(defun cj/mu4e--refile-folder (msg)
  "Refile-folder function for `mu4e-refile-folder'.
Dispatch on MSG's maildir via `cj/mu4e--refile-folder-for-maildir'."
  (cj/mu4e--refile-folder-for-maildir (and msg (mu4e-message-field msg :maildir))))

(defcustom cj/smtpmail-debug-enabled nil
  "Non-nil means enable verbose SMTP transport debug logging.

Keep this nil during normal startup. SMTP debug output is useful for
troubleshooting mail delivery problems, but it can expose sensitive mail
transport details in debug buffers."
  :type 'boolean
  :group 'mail)

(defun cj/set-smtpmail-debug (enabled)
  "Set SMTP transport debug logging according to ENABLED."
  (interactive
   (list (y-or-n-p "Enable SMTP transport debug logging? ")))
  (setq cj/smtpmail-debug-enabled enabled)
  (setq smtpmail-debug-info enabled)
  (message "SMTP transport debug logging %s"
           (if enabled "enabled" "disabled")))

(defun cj/toggle-smtpmail-debug ()
  "Toggle verbose SMTP transport debug logging for troubleshooting."
  (interactive)
  (cj/set-smtpmail-debug (not smtpmail-debug-info)))

(defun cj/mail--mbsync-command ()
  "Return the mu4e mail sync command, or nil if mbsync is unavailable."
  (when-let ((mbsync (cj/executable-find-or-warn
                      "mbsync" "mu4e mail synchronization" 'mail-config)))
    (concat (shell-quote-argument mbsync) " -a")))

(defun cj/mail-configure-smtpmail ()
  "Configure SMTP mail transport when msmtp is available."
  (setq smtpmail-debug-info cj/smtpmail-debug-enabled)
  (if-let ((msmtp (cj/executable-find-or-warn
                   "msmtp" "SMTP mail sending" 'mail-config)))
      (setq sendmail-program msmtp
            send-mail-function 'message-send-mail-with-sendmail
            message-send-mail-function 'message-send-mail-with-sendmail
            message-sendmail-envelope-from 'header)
    (setq sendmail-program nil)))

;; -------------------- HarfBuzz Crash Fix: Disable Composition ---------------
;; Disable auto-composition in mu4e headers to prevent SIGSEGV from HarfBuzz
;; when shaping emoji characters in email subjects. See Commentary above.

(defun cj/disable-auto-composition ()
  "Disable `auto-composition-mode' in the current buffer."
  (auto-composition-mode -1))

(add-hook 'mu4e-headers-mode-hook #'cj/disable-auto-composition)

;; ------------------------------ Mark All Headers -----------------------------
;; convenience function to mark all headers for an action

(defun cj/mu4e-mark-all-headers ()
  "Mark all headers for a later action.

Prompts user for the action when executing."
  (interactive)
  (mu4e-headers-mark-for-each-if
   (cons 'something nil)
   (lambda (_msg _param) t)))

;; ---------------------------------- SMTPmail ---------------------------------
;; send mail to smtp host from smtpmail temp buffer.

(use-package smtpmail
  :ensure nil ;; built-in
  :defer .5
  :config
  (cj/mail-configure-smtpmail))

;; --------------------------------- Mu4e Email --------------------------------

(autoload 'mu4e "mu4e" "Launch mu4e email client." t)
(keymap-global-set "C-c m" #'mu4e)

;; mu4e's main view uses a display-buffer-full-frame action (see
;; mu4e-display-buffer), which deletes the window split on launch.  Per that
;; function's own docstring, the supported way to change it is
;; display-buffer-alist.  Route the main buffer to the current window instead
;; (reuse a window already showing it first), so launching mu4e in a split
;; leaves the rest of the layout intact.  Registered eagerly rather than in
;; mu4e's deferred :config so it applies on the first launch.
(add-to-list 'display-buffer-alist
             '("\\`\\*mu4e-main\\*\\'"
               (display-buffer-reuse-window display-buffer-same-window)
               (inhibit-same-window . nil)))

;; Keep global font-lock out of the mu4e buffers.  mu4e paints header lines, the
;; main menu, and view headers with manual `face' text properties; global
;; font-lock strips them (the same failure the dashboard hit), leaving the
;; buffers unthemed.  Excluding these modes keeps mu4e's faces.
(cj/exclude-from-global-font-lock 'mu4e-headers-mode 'mu4e-main-mode 'mu4e-view-mode)

(use-package mu4e
  :ensure nil  ;; mu4e gets installed by installing 'mu' via the system package manager
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :commands (mu4e mu4e-update-index)
  :bind
  ((:map mu4e-headers-mode-map
    ("M" . cj/mu4e-mark-all-headers)
    ("D" . mu4e-headers-mark-for-trash)
    ("d" . mu4e-headers-mark-for-delete))
   (:map mu4e-view-mode-map
    ("r" . mu4e-compose-wide-reply)
    ("R" . mu4e-compose-reply)))
  :hook
  (mu4e-view-mode . turn-on-visual-line-mode)
  :config
  (setq gnus-blocked-images "http")                                       ;; block external images (i.e., 1 px trackers)
  (setq mail-user-agent 'mu4e-user-agent)                                 ;; default to mu4e for email
  (setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n") ;; helps show up properly in Outlook/Gmail threads
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  ;; Kill the compose buffer on send/exit so compose buffers don't accumulate.
  ;; Single home for this policy: org-msg only reads this variable (to decide
  ;; whether to widen-and-undo its edits) and never sets it, so the value set
  ;; here governs both plain mu4e and org-msg compose buffers.
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-change-filenames-when-moving t)                              ;; avoid gmail dup UID issues: https://goo.gl/RTCgVa
  (setq mu4e-completing-read-function 'completing-read)                   ;; use generic completing read, rather than ido
  (setq mu4e-compose-context-policy 'ask)                                 ;; ask for context if no context matches

  ;; (setq mu4e-compose-format-flowed t)                                       ;; plain text mails must flow correctly for recipients
  (setq mu4e-compose-keep-self-cc t)                                      ;; keep me in the cc list
  (with-suppressed-warnings ((obsolete mu4e-compose-signature-auto-include)
                             (free-vars mu4e-compose-signature-auto-include))
    (setq mu4e-compose-signature-auto-include nil))                       ;; don't include signature by default
  (setq mu4e-confirm-quit nil)                                            ;; don't ask when quitting
  (setq mu4e-context-policy 'pick-first)                                  ;; start with the first (default) context
  (setq mu4e-headers-auto-update nil)                                     ;; updating headers buffer on email is too jarring
  (setq mu4e-root-maildir mail-dir)                                       ;; root directory for all email accounts
  (with-suppressed-warnings ((obsolete mu4e-maildir)
                             (free-vars mu4e-maildir))
    (setq mu4e-maildir mail-dir))                                         ;; same as above (for newer mu4e)
  (setq mu4e-sent-messages-behavior 'delete)                              ;; don't save to "Sent", IMAP does this already
  (setq mu4e-show-images t)                                               ;; show embedded images
  ;; (setq mu4e-update-interval 600)                                      ;; check for new mail every 10 minutes (600 seconds)
  ;; TEMPORARILY DISABLED: Causing password prompts that interrupt work
  (setq mu4e-hide-index-messages t)                                       ;; don't show indexing messages buffer
  (setq mu4e-headers-from-or-to-prefix '("" . "➜ "))

  ;; Format=flowed for better plain text email handling
  ;; This will be automatically disabled when org-msg is active
  (setq mu4e-compose-format-flowed t)

  (with-suppressed-warnings ((obsolete mu4e-html2text-command)
                             (free-vars mu4e-html2text-command))
    (setq mu4e-html2text-command 'mu4e-shr2text))  ;; email conversion to html via shr2text
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-get-mail-command (cj/mail--mbsync-command))                  ;; command to sync mail
  (with-suppressed-warnings ((obsolete mu4e-user-mail-address-list)
                             (free-vars mu4e-user-mail-address-list))
    (setq mu4e-user-mail-address-list '("c@cjennings.net"
                                        "craigmartinjennings@gmail.com"
                                        "craig.jennings@deepsat.com")))
  (setq mu4e-index-update-error-warning nil) ;; don't warn me about spurious sync issues

  ;; ------------------------------ Mu4e Contexts ------------------------------

  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail.com"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address        . "craigmartinjennings@gmail.com")
                  (user-full-name           . "Craig Jennings")
                  (mu4e-drafts-folder       . "/gmail/Drafts")
                  (mu4e-sent-folder         . "/gmail/Sent")
                  (mu4e-starred-folder      . "/gmail/Starred")
                  (mu4e-trash-folder        . "/gmail/Trash")))

         (make-mu4e-context
          :name "cjennings.net"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/cmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address    . "c@cjennings.net")
                  (user-full-name       . "Craig Jennings")
                  (mu4e-drafts-folder   . "/cmail/Drafts")
                  (mu4e-sent-folder     . "/cmail/Sent")
                  (mu4e-trash-folder    . "/cmail/Trash")))

         (make-mu4e-context
          :name "deepsat.com"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/dmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address        . "craig.jennings@deepsat.com")
                  (user-full-name           . "Craig Jennings")
                  (mu4e-drafts-folder       . "/dmail/Drafts")
                  (mu4e-sent-folder         . "/dmail/Sent")
                  (mu4e-starred-folder      . "/dmail/Starred")
                  (mu4e-trash-folder        . "/dmail/Trash")))))

  ;; Refile target is computed per message (see `cj/mu4e--refile-folder'), not
  ;; set per context, because mu4e context :vars are sticky and would leak one
  ;; account's archive folder into another.  cmail archives to /cmail/Archive;
  ;; gmail/dmail signal rather than move mail into an unsynced folder.
  (setq mu4e-refile-folder #'cj/mu4e--refile-folder)

  (setq mu4e-maildir-shortcuts
        '(("/cmail/Inbox"       . ?i)
          ("/cmail/Sent"        . ?s)
          ("/gmail/Inbox"       . ?I)
          ("/gmail/Sent"        . ?S)
          ("/dmail/Inbox"       . ?d)
          ("/dmail/Sent"        . ?D)))

  ;; ------------------------------ Mu4e Bookmarks -----------------------------

  ;; Landing-page bookmarks: just unread per account, for at-a-glance triage.
  ;; Keys match the account letter (b<letter>): bc cmail, bg gmail, bd deepsat.
  ;; Full account/folder navigation lives under C-; e (cj/email-map).
  (setq mu4e-bookmarks
        `((:name "cjennings unread"
                 :query "maildir:/cmail/INBOX AND flag:unread AND NOT flag:trashed"
                 :key ?c)
          (:name "gmail.com unread"
                 :query "maildir:/gmail/INBOX AND flag:unread AND NOT flag:trashed"
                 :key ?g)
          (:name "deepsat.com unread"
                 :query "maildir:/dmail/INBOX AND flag:unread AND NOT flag:trashed"
                 :key ?d)))

  (defun no-auto-fill ()
    "Turn off `auto-fill-mode'."
    (auto-fill-mode -1))
  (add-hook 'mu4e-compose-mode-hook #'no-auto-fill)

  ;; Always BCC myself
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
  (defun cj/add-cc-bcc-header ()
    "Add CC: and BCC: to myself header."
    (save-excursion (message-add-header
                     (concat "CC: " "\n")
                     ;; pre hook above changes user-mail-address.
                     (concat "Bcc: " user-mail-address "\n"))))
  (add-hook 'mu4e-compose-mode-hook 'cj/add-cc-bcc-header)

  ;; use imagemagick to render images, if available
  (when (fboundp 'imagemagick-register-types)
	(imagemagick-register-types))

  ;; ------------------------------ HTML Settings ------------------------------
  ;; also see org-msg below

  ;; Prefer HTML over plain text when both are available
  (with-suppressed-warnings ((obsolete mu4e-view-prefer-html)
                             (free-vars mu4e-view-prefer-html))
    (setq mu4e-view-prefer-html t))

  ;; Use a better HTML renderer with more control
  (with-suppressed-warnings ((obsolete mu4e-html2text-command)
                             (free-vars mu4e-html2text-command))
    (setq mu4e-html2text-command
		  (cond
		   ;; Best option: pandoc (if available)
		   ((executable-find "pandoc")
		    "pandoc -f html -t plain --reference-links")
		   ;; Good option: w3m (better tables/formatting)
		   ((executable-find "w3m")
		    "w3m -dump -T text/html -cols 72 -o display_link_number=true")
		   ;; Fallback: built-in shr
		   (t 'mu4e-shr2text))))

  ;; Configure shr (built-in HTML renderer) for better display
  (setq shr-use-colors nil)          ; Don't use colors in terminal
  (setq shr-use-fonts nil)           ; Don't use variable fonts
  (setq shr-max-image-proportion 0.7) ; Limit image size
  (setq shr-width 72)                ; Set width for HTML rendering
  (setq shr-bullet "• ")             ; Nice bullet points

  ;; Image policy: remote HTTP images are blocked by default (tracking
  ;; pixels) via `gnus-blocked-images' "http" above; embedded/attached
  ;; images render inline via `mu4e-show-images'; sizing is governed by
  ;; `shr-max-image-proportion'. The per-message override is
  ;; `cj/mu4e-toggle-remote-images' below. (The old mu4e-view-show-images /
  ;; mu4e-view-image-max-width pair was dropped: obsolete since mu4e 1.7,
  ;; ignored by the shr-based view.)

  ;; ------------------------------- View Actions ------------------------------
  ;; define view and article menus

  (defun cj/search-for-sender (msg)
	"Search for messages sent by the sender of the message at point."
	(mu4e-search
	 (concat "from:"
			 (mu4e-contact-email (car (mu4e-message-field msg :from))))))

  ;; Custom function to toggle remote content and bind it in view mode
  (defun cj/mu4e-toggle-remote-images ()
	"Toggle display of remote images in the current message.
Buffer-local, so the override lasts only for this message view.
Echoes the effective state so there's no guessing what a refresh did."
	(interactive)
	(require 'mu4e-view)
	(setq-local gnus-blocked-images
				(if (equal gnus-blocked-images "http")
					nil
				  "http"))
	(mu4e-view-refresh)
	(message "Remote images: %s (this message only)"
			 (if (equal gnus-blocked-images "http") "blocked" "shown")))

  ;; first letter is the keybinding
  (setq mu4e-headers-actions
		'(("asave attachment"   . mu4e-view-save-attachments)
		  ("csave contact"      . mu4e-action-add-org-contact)
		  ("ssearch for sender" . cj/search-for-sender)
		  ("tshow this thread"  . mu4e-action-show-thread)
		  ("vview in browser"   . mu4e-action-view-in-browser)))

  ;; first letter is the keybinding
  (setq mu4e-view-actions
		'(("asave attachments"     . mu4e-view-save-attachments)
		  ("csave contact"         . mu4e-action-add-org-contact)
		  ("itoggle remote images" . cj/mu4e-toggle-remote-images)
		  ("lsave to attach later" . mu4e-action-capture-message)
		  ("vview in browser"      . mu4e-action-view-in-browser)))
 (setq mu4e-compose-complete-addresses nil)

  ;; ---------------------------- Address Completion ---------------------------

  ;; Disable company-mode in compose buffers
  (defun cj/disable-company-in-mu4e-compose ()
	"Disable company mode in mu4e compose buffers."
	(company-mode -1))

  (add-hook 'mu4e-compose-mode-hook #'cj/disable-company-in-mu4e-compose)

  ;; NOTE: Key bindings for TAB and comma are now handled by
  ;; mu4e-org-contacts-integration module which provides:
  ;; - Smart TAB completion in email headers
  ;; - Comma-triggered completion
  ;; - Integration with org-contacts database

  ;; Also disable company in org-msg-edit-mode
  (with-eval-after-load 'org-msg
	(add-hook 'org-msg-edit-mode-hook #'cj/disable-company-in-mu4e-compose))

  ;; Don't spell-check email addresses and headers
  (defun cj/disable-ispell-in-email-headers ()
	"Disable ispell in email header fields."
	(make-local-variable 'ispell-skip-region-alist)
	(add-to-list 'ispell-skip-region-alist
				 '("^To:\\|^Cc:\\|^Bcc:" . "^[^:]*$"))
	(add-to-list 'ispell-skip-region-alist
				 '("^From:" . "^[^:]*$"))
	(add-to-list 'ispell-skip-region-alist
				 '("^Subject:" . "^[^:]*$")))

  (add-hook 'mu4e-compose-mode-hook #'cj/disable-ispell-in-email-headers)
  (add-hook 'message-mode-hook #'cj/disable-ispell-in-email-headers)
  (add-hook 'org-msg-edit-mode-hook #'cj/disable-ispell-in-email-headers)

  (require 'mu4e-org-contacts-integration)
  (cj/activate-mu4e-org-contacts-integration)) ;; end use-package mu4e


;; ----------------------- Account Navigation Keymaps --------------------------
;; The C-; e c/d/g submaps jump to each account's inbox views.  Built from one
;; template so the maildir prefix is the only per-account difference.

;; eval-and-compile so the builder is defined when org-msg's :preface (below)
;; calls it during byte-compilation, not only at load.
(eval-and-compile
  (defun cj/--mail-account-search-queries (account)
    "Return an alist of (KEY . QUERY) mu4e searches for ACCOUNT's inbox.
ACCOUNT is the maildir account name (\"cmail\", \"dmail\", \"gmail\").  The four
entries scope inbox / unread / flagged / large searches to that account's
INBOX maildir."
    (let ((base (format "maildir:/%s/INBOX" account)))
      (list (cons "i" base)
            (cons "u" (concat base " AND flag:unread AND NOT flag:trashed"))
            (cons "s" (concat base " AND flag:flagged"))
            (cons "l" (concat base " AND size:5M..999M")))))

  (defun cj/--mail-make-account-map (account)
    "Build a mu4e navigation keymap for ACCOUNT (a maildir account name).
Keys i/u/s/l run the inbox/unread/flagged/large searches from
`cj/--mail-account-search-queries', each scoped to ACCOUNT."
    (let ((map (make-sparse-keymap)))
      (dolist (entry (cj/--mail-account-search-queries account) map)
        (let ((query (cdr entry)))
          (keymap-set map (car entry)
                      (lambda () (interactive) (mu4e-search query))))))))

;; ---------------------------------- Org-Msg ----------------------------------
;; user composes org mode; recipient receives html

(use-package org-msg
  :vc (:url "https://github.com/jeremy-compostella/org-msg" :rev :newest)
  :defer 1
  :after (org mu4e)
  :preface
	(defvar cj/mail-cmail-map (cj/--mail-make-account-map "cmail")
	  "cmail account navigation.")
	(defvar cj/mail-dmail-map (cj/--mail-make-account-map "dmail")
	  "deepsat account navigation.")
	(defvar cj/mail-gmail-map (cj/--mail-make-account-map "gmail")
	  "gmail account navigation.")
	(defvar-keymap cj/email-map
	  :doc "Email operations and account navigation"
	  "A" #'org-msg-attach-attach
	  "D" #'org-msg-attach-delete
	  "m" #'cj/mu4e-save-some-attachments
	  "S" #'cj/mu4e-save-all-attachments
	  "c" cj/mail-cmail-map
	  "d" cj/mail-dmail-map
	  "g" cj/mail-gmail-map
	  "s" #'cj/mu4e-save-attachment-here)
	(cj/register-prefix-map "e" cj/email-map)
	(with-eval-after-load 'which-key
	  (which-key-add-key-based-replacements
        "C-; e" "email menu"
        "C-; e A" "attach file"
        "C-; e D" "delete attachment"
        "C-; e m" "select attachments"
        "C-; e S" "save all attachments"
        "C-; e c" "cmail"
        "C-; e c i" "cmail inbox"
        "C-; e c u" "cmail unread"
        "C-; e c s" "cmail starred"
        "C-; e c l" "cmail large"
        "C-; e d" "deepsat"
        "C-; e d i" "deepsat inbox"
        "C-; e d u" "deepsat unread"
        "C-; e d s" "deepsat starred"
        "C-; e d l" "deepsat large"
        "C-; e g" "gmail"
        "C-; e g i" "gmail inbox"
        "C-; e g u" "gmail unread"
        "C-; e g s" "gmail starred"
        "C-; e g l" "gmail large"
        "C-; e s" "save attachment"))
  :bind
  ;; more intuitive keybinding for attachments
  (:map org-msg-edit-mode-map
		("C-c C-a" . org-msg-attach-attach)
		("C-c C-d" . org-msg-attach-delete))
  :config
  ;; inline CSS, no postamble, no TOC, no stars or footers
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil")

  ;; hide org markup, show inline images
  (setq org-msg-startup "hidestars inlineimages")

  ;; new and html emails get the option for both text and html,
  ;; text emails get text only replies
  (setq org-msg-default-alternatives
		'((new      . (text html))
		  (reply-to-html    . (text html))
		  (reply-to-text    . (text))))

  ;; Convert Org Citations to Blockquote
  (setq org-msg-convert-citation t)

  ;; enforce css usage; default renders too small
  (setq org-msg-enforce-css t)

  ;; Override just the problematic styles with important tags
  (setq org-msg-extra-css
		(concat
		 "<style type=\"text/css\">\n"
		 "body { font-size: 14px !important; line-height: 1.6 !important; }\n"
		 "p { font-size: 14px !important; margin: 10px 0 !important; }\n"
		 "li { font-size: 14px !important; }\n"
		 "pre { font-size: 13px !important; }\n"
		 "code { font-size: 13px !important; }\n"
		 "</style>"))

  ;; turn on org-msg in all compose buffers
  (org-msg-mode +1))

(advice-add #'mu4e-compose-reply
			:after (lambda (&rest _) (org-msg-edit-mode)))
(advice-add #'mu4e-compose-wide-reply
			:after (lambda (&rest _) (org-msg-edit-mode)))

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c m" "mu4e email"))

(provide 'mail-config)
;;; mail-config.el ends here
