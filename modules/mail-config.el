;;; mail-config --- Settings for Mu4e Email -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; I found Aime Bertrand's blog post to be an excellent walkthrough of how to
;; setup a Mu4e config.
;;
;; https://macowners.club/posts/email-emacs-mu4e-macos/
;;
;; on saving attachments:
;; After running mu4e-view-save-attachments,
;; - invoke embark-act-all in the completion menu
;; - followed by RET (mu4e-view-save-attachments) to save all attachments
;;
;; - or TAB (vertico-insert)
;; - followed by , (comma) next to each file you want to save,
;; - then RET (vertico-exit), to save selected attachments.
;;
;;; Code:

(require 'user-constants)

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
  (setq sendmail-program (executable-find "msmtp"))
  (setq send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)
  (setq smtpmail-debug-info t))

;; --------------------------------- Mu4e Email --------------------------------

(use-package mu4e
  :ensure nil  ;; mu4e gets installed by installing 'mu' via the system package manager
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :commands (mu4e mu4e-update-index)
  :bind
  ("C-c m". mu4e)
  (:map mu4e-headers-mode-map
        ("M" . cj/mu4e-mark-all-headers)
        ("D" . mu4e-headers-mark-for-trash)
        ("d" . mu4e-headers-mark-for-delete))
  (:map mu4e-view-mode-map
        ("r" . mu4e-compose-wide-reply)
		("R" . mu4e-compose-reply))
  :hook
  (mu4e-view-mode . turn-on-visual-line-mode)
  :config
  (setq gnus-blocked-images "http")                                       ;; block external images (i.e., 1 px trackers)
  (setq mail-user-agent 'mu4e-user-agent)                                 ;; default to mu4e for email
  (setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n") ;; helps show up properly in Outlook/Gmail threads
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-kill-buffer-on-exit t)                                    ;; don't keep message buffers around
  (setq mu4e-change-filenames-when-moving t)                              ;; avoid gmail dup UID issues: https://goo.gl/RTCgVa
  (setq mu4e-completing-read-function 'completing-read)                   ;; use generic completing read, rather than ido
  (setq mu4e-compose-context-policy 'ask)                                 ;; ask for context if no context matches

  ;; (setq mu4e-compose-format-flowed t)                                       ;; plain text mails must flow correctly for recipients
  (setq mu4e-compose-keep-self-cc t)                                      ;; keep me in the cc list
  (setq mu4e-compose-signature-auto-include nil)                          ;; don't include signature by default
  (setq mu4e-confirm-quit nil)                                            ;; don't ask when quitting
  (setq mu4e-context-policy 'pick-first)                                  ;; start with the first (default) context
  (setq mu4e-headers-auto-update nil)                                     ;; updating headers buffer on email is too jarring
  (setq mu4e-root-maildir mail-dir)                                       ;; root directory for all email accounts
  (setq mu4e-maildir mail-dir)                                            ;; same as above (for newer mu4e)
  (setq mu4e-sent-messages-behavior 'delete)                              ;; don't save to "Sent", IMAP does this already
  (setq mu4e-show-images t)                                               ;; show embedded images
  (setq mu4e-update-interval 600)                                         ;; check for new mail every 10 minutes (600 seconds)
  (setq mu4e-hide-index-messages t)                                       ;; don't show indexing messages buffer

  ;; Format=flowed for better plain text email handling
  ;; This will be automatically disabled when org-msg is active
  (setq mu4e-compose-format-flowed t)

  (setq mu4e-html2text-command 'mu4e-shr2text)  ;; email conversion to html via shr2text
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))    ;; command to sync mail
  (setq mu4e-user-mail-address-list '("c@cjennings.net" "craigmartinjennings@gmail.com"))
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
                  (mu4e-sent-folder     . "/cmail/Sent")))))

  (setq mu4e-maildir-shortcuts
        '(("/cmail/Inbox"       . ?i)
          ("/cmail/Sent"        . ?s)
          ("/gmail/Inbox"       . ?I)
          ("/gmail/Sent"        . ?S)))

  ;; ------------------------------ Mu4e Bookmarks -----------------------------

  (setq mu4e-bookmarks
        `((:name "cjennings inbox"
                 :query "maildir:/cmail/INBOX"
                 :key ?i)
          (:name "cjennings unread"
                 :query "maildir:/cmail/INBOX AND flag:unread AND NOT flag:trashed"
                 :key ?u)
          (:name "cjennings starred"
                 :query "maildir:/cmail/INBOX AND flag:flagged"
                 :key ?s)
          (:name "cjennings large"
                 :query "maildir:/cmail/INBOX AND size:5M..999M"
                 :key ?l)
          (:name "gmail.com inbox"
                 :query "maildir:/gmail/INBOX"
                 :key ?I)
          (:name "gmail.com unread"
                 :query "maildir:/gmail/INBOX AND flag:unread AND NOT flag:trashed"
                 :key ?U)
          (:name "gmail.com starred"
                 :query "maildir:/gmail/INBOX AND flag:flagged"
                 :key ?S)
          (:name "gmail.com large"
                 :query "maildir:/gmail/INBOX AND size:5M..999M"
                 :key ?L)))

  (defun no-auto-fill ()
    "Turn off \'auto-fill-mode\'."
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
  (setq mu4e-view-prefer-html t)

  ;; Use a better HTML renderer with more control
  (setq mu4e-html2text-command
		(cond
		 ;; Best option: pandoc (if available)
		 ((executable-find "pandoc")
		  "pandoc -f html -t plain --reference-links")
		 ;; Good option: w3m (better tables/formatting)
		 ((executable-find "w3m")
		  "w3m -dump -T text/html -cols 72 -o display_link_number=true")
		 ;; Fallback: built-in shr
		 (t 'mu4e-shr2text)))

  ;; Configure shr (built-in HTML renderer) for better display
  (setq shr-use-colors nil)          ; Don't use colors in terminal
  (setq shr-use-fonts nil)           ; Don't use variable fonts
  (setq shr-max-image-proportion 0.7) ; Limit image size
  (setq shr-width 72)                ; Set width for HTML rendering
  (setq shr-bullet "• ")             ; Nice bullet points

  ;; Block remote images by default (privacy/security)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-image-max-width 800)

  ;; ------------------------------- View Actions ------------------------------
  ;; define view and article menus

  (defun cj/search-for-sender (msg)
	"Search for messages sent by the sender of the message at point."
	(mu4e-search
	 (concat "from:"
			 (mu4e-contact-email (car (mu4e-message-field msg :from))))))

  ;; Custom function to toggle remote content and bind it in view mode
  (defun cj/mu4e-toggle-remote-images ()
	"Toggle display of remote images in current message."
	(interactive)
	(require 'mu4e-view)
	(setq-local gnus-blocked-images
				(if (equal gnus-blocked-images "http")
					nil
				  "http"))
	(mu4e-view-refresh))

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


;; ---------------------------------- Org-Msg ----------------------------------
;; user composes org mode; recipient receives html

(use-package org-msg
  ;; :vc (:url "https://github.com/cjennings/org-msg" :rev :newest)
  :load-path "/home/cjennings/code/org-msg"
  :defer 1
  :after (org mu4e)
  :preface
	(defvar-keymap cj/email-map
	  :doc "Keymap for email operations"
	  "a" #'org-msg-attach-attach
	  "d" #'org-msg-attach-delete)
	(keymap-set cj/custom-keymap "e" cj/email-map)
	(with-eval-after-load 'which-key
	  (which-key-add-key-based-replacements
        "C-; e" "email menu"
        "C-; e a" "attach file"
        "C-; e d" "delete attachment"))
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

  ;; always kill buffers on exit
  (setq message-kill-buffer-on-exit nil)

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
