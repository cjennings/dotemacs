;;; mail-config --- Settings for Mu4e Email -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; I found Aime Bertrand's blog post to be an excellent walkthrough of how to
;; setup a Mu4e config.

;; https://macowners.club/posts/email-emacs-mu4e-macos/

;;; Code:

;; ----------------------- Mu4e View Save All Attachments ----------------------
;; replacement for the extract function which saves all attachments

(defun cj/mu4e-view-save-all-attachments (&optional msg)
  "Save all attachments from the current email.
Prompt user for directory, creating if necessary, select all attachments in the
email, and save them in the specified directory. The optional MSG is only
provided when calling this function from mu4e's action context. This function is
intended to be used as a `mu4e-view-action' and/or bound to a key in
mu4e-view-mode-map."
  (interactive)
  (let ((msg (or msg (mu4e-message-at-point))))
	(let* ((parts (mu4e-view-mime-parts))
		   ;; build cons list of candidate mime parts
		   (candidates (seq-map
						(lambda (attachment-part)
						  (cons ;; (filename . annotation)
						   (plist-get attachment-part :filename)
						   attachment-part))
						;; scope to parts with a filename
						(seq-filter
						 (lambda (part) (plist-get part :attachment-like))
						 parts)))
		   (candidates (or candidates (mu4e-warn "No attachments for this message")))
		   (files (mapcar 'car candidates)) ;; Select all attachments
		   (custom-dir (read-directory-name "Save to directory: ")))
	  (unless (file-exists-p custom-dir)
		(make-directory custom-dir t)) ;; t creates parent dirs if needed
	  ;; iterate over each file
	  (seq-do
	   (lambda (fname)
		 (let* ((part (cdr (assoc fname candidates)))
				;; build unique full file path
				(path (mu4e--uniqify-file-name
					   (mu4e-join-paths
						custom-dir
						(plist-get part :filename)))))
		   ;; save the file
		   (mm-save-part-to-file (plist-get part :handle) path)))
	   files))))

;; ------------------------- Mark All Headers ------------------------
;; convenience function to mark all headers for an action

(defun cj/mu4e-mark-all-headers ()
  "Mark all headers for a later action.
Prompts user for the action when executing."
  (interactive)
  (mu4e-headers-mark-for-each-if
   (cons 'something nil)
   (lambda (_msg _param) t)))

;;; ------------------ Smtpmail & Easy PG Assistant -----------------
;; send mail to smtp host from smtpmail temp buffer.
(use-package smtpmail
  :ensure nil ;; built-in
  :defer .5
  :config
  (setq message-kill-buffer-on-exit t) ;; don't keep compose buffers after sending
  (setq sendmail-program (executable-find "msmtp"))
  (setq send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)
  (setq smtpmail-debug-info t))

;; --------------------------------- Mu4e Email --------------------------------

(use-package mu4e
  :ensure nil  ;; mu4e gets installed by installing 'mu' via the system package manager
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer .5
  :bind
  ("C-c m". mu4e)
  (:map mu4e-headers-mode-map
        ("M" . cj/mu4e-mark-all-headers)
        ("D" . mu4e-headers-mark-for-trash)
        ("d" . mu4e-headers-mark-for-delete))
  (:map mu4e-view-mode-map
        ("r" . mu4e-compose-wide-reply)
        ("R" . mu4e-compose-reply)
		("e" . cj/mu4e-view-save-all-attachments))
  :hook
  (mu4e-view-mode . turn-on-visual-line-mode)
  :config
  (setq gnus-blocked-images "http")                                         ;; block external images
  (setq mail-user-agent 'mu4e-user-agent)                                   ;; default to mu4e for email
  (setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")   ;; helps show up properly in Outlook/Gmail threads
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-kill-buffer-on-exit t)                                      ;; don't keep message buffers around
  (setq message-signature-file (concat user-emacs-directory "signature"))   ;; look for the signature here
  (setq mu4e-change-filenames-when-moving t)                                ;; avoid gmail dup UID issues: https://goo.gl/RTCgVa
  (setq mu4e-completing-read-function 'completing-read)                     ;; use generic completing read, rather than ido
  (setq mu4e-compose-context-policy 'ask)                                   ;; ask for context if no context matches
  (setq mu4e-compose-format-flowed t)                                       ;; plain text mails must flow correctly for recipients
  (setq mu4e-compose-keep-self-cc t)                                        ;; keep me in the cc list
  (setq mu4e-compose-signature-auto-include nil)                            ;; don't include signature by default
  (setq mu4e-confirm-quit nil)                                              ;; don't ask when quitting
  (setq mu4e-context-policy 'pick-first)                                    ;; start with the first (default) context
  (setq mu4e-headers-auto-update nil)                                       ;; updating headers buffer on email is too jarring
  (setq mu4e-root-maildir mail-dir)                                         ;; root directory for all email accounts
  (setq mu4e-sent-messages-behavior 'delete)                                ;; don't save to "Sent", IMAP does this already
  (setq mu4e-show-images t)                                                 ;; show embedded images
  (setq mu4e-update-interval nil)                                           ;; don't update automatically

  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))    ;; command to sync mail
  (setq mu4e-user-mail-address-list '("c@cjennings.net" "craigmartinjennings@gmail.com"))
  (setq mu4e-index-update-error-warning nil) ;; don't warn me about spurious sync issues

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

  ;; bookmarks and their shortcuts
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

  (add-hook 'mu4e-compose-mode-hook
            (defun cj/set-mu4e-compose ()
              "My settings for message composition."
              (set-fill-column 72)))

  ;; add save-all-attachments to view actions list
  (add-to-list 'mu4e-view-actions
               '("save all attachments" . cj/mu4e-view-save-all-attachments))

  ;; Always BCC myself
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
  (defun cj/add-header ()
    "Add CC: and Bcc: to myself header."
    (save-excursion (message-add-header
                     (concat "CC: " "\n")
                     ;; pre hook above changes user-mail-address.
                     (concat "Bcc: " user-mail-address "\n"))))
  (add-hook 'mu4e-compose-mode-hook 'cj/add-header)

  ;; remap the awkward mml-attach-file to the quicker mail-add-attachment
  (define-key mu4e-compose-mode-map [remap mml-attach-file] 'mail-add-attachment)

  ;; don't allow openwith to mess with your attachments
  (add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)

  ;; use imagemagick to render images, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; xwidgets not able to be built into emacs on linux
  ;; ;; view in xwidget html rendererer
  ;; (add-to-list 'mu4e-headers-actions
  ;;               '("xWidget" . mu4e-action-view-with-xwidget) t)
  ;; (add-to-list 'mu4e-view-actions
  ;;               '("xWidget" . mu4e-action-view-with-xwidget) t))

  ) ;; end use-package mu4e

(defun no-auto-fill ()
  "Turn off \'auto-fill-mode\'."
  (auto-fill-mode -1))
(add-hook 'mu4e-compose-mode-hook #'no-auto-fill)

;; ----------------------------- Compose Mode Hydra ----------------------------
;; WIP: menu available in compose mode

(defhydra hydra-mu4e-compose (:color blue :timeout 10 :hint nil)
  "Compose Mode Menu\n\n"
  ("q" quit-window                   "Quit" :column "")
  ("a" mail-add-attachment           "Add Attachment" :column "")
  ("r" message-new-line-and-reformat "Newline and Reformat" :column "")
  ("d" message-delete-not-region     "Delete Outside Region" :column ""))

(defun mu4e-compose-mode-hook-hydra-setup ()
  "Create hydra/menu keybinding when entering compose mode."
  (local-set-key (kbd "C-c ?") 'hydra-mu4e-compose/body))
(add-hook 'mu4e-compose-mode-hook 'mu4e-compose-mode-hook-hydra-setup)

(provide 'mail-config)
;;; mail-config.el ends here
