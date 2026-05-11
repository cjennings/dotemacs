;;; mail-config --- Settings for Mu4e Email -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; I found Aime Bertrand's blog post to be an excellent walkthrough of how to
;; setup a Mu4e config.
;;
;; https://macowners.club/posts/email-emacs-mu4e-macos/
;;
;; Attachment saving:
;; - C-; e S saves all attachments from the current mu4e view message.
;; - C-; e s prompts for one attachment and saves it.
;; Both commands prompt for a destination directory.
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
(require 'seq)

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
(defvar mu4e-uniquify-save-file-name-function)
(defvar-local cj/mu4e-attachment-selection-directory nil
  "Destination directory for the current attachment selection buffer.")
(defvar-local cj/mu4e-attachment-selection-entries nil
  "Attachment selection entries for the current selection buffer.")

(declare-function mm-save-part-to-file "mm-decode" (handle filename))
(declare-function mu4e-join-paths "mu4e-helpers" (directory &rest components))
(declare-function mu4e-view-mime-parts "mu4e-mime-parts" ())

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

;; --------------------------- Attachment Saving ------------------------------

(defun cj/mu4e--attachment-parts (&optional parts)
  "Return attachment-like MIME PARTS for the current mu4e view message.
When PARTS is nil, read parts from `mu4e-view-mime-parts'."
  (seq-filter (lambda (part) (plist-get part :attachment-like))
              (or parts
                  (progn
                    (unless (fboundp 'mu4e-view-mime-parts)
                      (require 'mu4e-mime-parts))
                    (mu4e-view-mime-parts)))))

(defun cj/mu4e--attachment-duplicate-filenames (parts)
  "Return filenames that appear more than once in PARTS."
  (let ((counts (make-hash-table :test 'equal))
        duplicates)
    (dolist (part parts)
      (let ((filename (plist-get part :filename)))
        (puthash filename (1+ (gethash filename counts 0)) counts)))
    (maphash (lambda (filename count)
               (when (> count 1)
                 (push filename duplicates)))
             counts)
    duplicates))

(defun cj/mu4e--attachment-label (part duplicate-filenames)
  "Return a completion label for PART.
DUPLICATE-FILENAMES is a list of filenames that need part-index disambiguation."
  (let ((filename (or (plist-get part :filename) "unnamed-attachment")))
    (if (member filename duplicate-filenames)
        (format "%s <part %s>" filename (plist-get part :part-index))
      filename)))

(defun cj/mu4e--attachment-candidates (parts)
  "Return completion candidates for attachment PARTS.
The result is an alist of display labels to MIME part plists."
  (let ((duplicates (cj/mu4e--attachment-duplicate-filenames parts)))
    (mapcar (lambda (part)
              (cons (cj/mu4e--attachment-label part duplicates) part))
            parts)))

(defun cj/mu4e--attachment-default-directory (parts)
  "Return a sensible default save directory for attachment PARTS."
  (file-name-as-directory
   (or (plist-get (car parts) :target-dir)
       (expand-file-name "~/Downloads/"))))

(defun cj/mu4e--read-attachment-directory (parts)
  "Prompt for a destination directory for attachment PARTS."
  (file-name-as-directory
   (read-directory-name "Save attachments to: "
                        (cj/mu4e--attachment-default-directory parts))))

(defun cj/mu4e--ensure-attachment-save-functions ()
  "Load mu4e MIME support when attachment save helpers need it."
  (unless (and (boundp 'mu4e-uniquify-save-file-name-function)
               (fboundp 'mu4e-join-paths))
    (require 'mu4e-mime-parts)))

(defun cj/mu4e--save-attachment-part (part directory)
  "Save attachment PART to DIRECTORY and return the final path."
  (cj/mu4e--ensure-attachment-save-functions)
  (let ((handle (plist-get part :handle)))
    (unless handle
      (user-error "Attachment has no MIME handle: %s"
                  (or (plist-get part :filename) "<unnamed>")))
    (let* ((path (funcall mu4e-uniquify-save-file-name-function
                          (mu4e-join-paths directory
                                           (plist-get part :filename)))))
      (mm-save-part-to-file handle path)
      path)))

(defun cj/mu4e--save-attachment-parts (parts directory)
  "Save attachment PARTS to DIRECTORY and return the saved paths."
  (mapcar (lambda (part)
            (cj/mu4e--save-attachment-part part directory))
          parts))

(defun cj/mu4e-save-all-attachments ()
  "Prompt for a directory and save all attachments in the current mu4e message."
  (interactive)
  (let ((parts (cj/mu4e--attachment-parts)))
    (unless parts
      (user-error "No attachments for this message"))
    (let* ((directory (cj/mu4e--read-attachment-directory parts))
           (paths (cj/mu4e--save-attachment-parts parts directory)))
      (message "Saved %d attachment%s to %s"
               (length paths)
               (if (= (length paths) 1) "" "s")
               directory)
      paths)))

(defun cj/mu4e-save-attachment-here ()
  "Prompt for one attachment and a directory, then save that attachment."
  (interactive)
  (let ((parts (cj/mu4e--attachment-parts)))
    (unless parts
      (user-error "No attachments for this message"))
    (let* ((directory (cj/mu4e--read-attachment-directory parts))
           (candidates (cj/mu4e--attachment-candidates parts))
           (choice (completing-read "Save attachment: " candidates nil t))
           (part (cdr (assoc choice candidates)))
           (path (cj/mu4e--save-attachment-part part directory)))
      (message "Saved attachment to %s" path)
      path)))

(defvar cj/mu4e-attachment-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'cj/mu4e-attachment-selection-toggle)
    (define-key map (kbd "a") #'cj/mu4e-attachment-selection-mark-all)
    (define-key map (kbd "u") #'cj/mu4e-attachment-selection-unmark-all)
    (define-key map (kbd "s") #'cj/mu4e-attachment-selection-save-marked)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `cj/mu4e-attachment-selection-mode'.")

(define-derived-mode cj/mu4e-attachment-selection-mode special-mode "Mail Attachments"
  "Mode for selecting mu4e attachments to save.")

(defun cj/mu4e--attachment-selection-entry-at-point ()
  "Return the attachment selection entry at point."
  (or (get-text-property (point) 'cj/mu4e-attachment-entry)
      (get-text-property (line-beginning-position) 'cj/mu4e-attachment-entry)
      (user-error "No attachment on this line")))

(defun cj/mu4e--attachment-selection-render ()
  "Render the current attachment selection buffer."
  (let ((inhibit-read-only t)
        (point-line (line-number-at-pos)))
    (erase-buffer)
    (insert (format "Save attachments to: %s\n\n"
                    cj/mu4e-attachment-selection-directory))
    (insert "RET toggle   a mark all   u unmark all   s save marked   q quit\n\n")
    (dolist (entry cj/mu4e-attachment-selection-entries)
      (let* ((part (plist-get entry :part))
             (mark (if (plist-get entry :selected) "[x]" "[ ]"))
             (label (plist-get entry :label))
             (mime-type (or (plist-get part :mime-type) ""))
             (size (if-let ((bytes (plist-get part :decoded-size-approx)))
                       (file-size-human-readable bytes)
                     "")))
        (insert
         (propertize
          (format "%s %-40s %-24s %s\n" mark label mime-type size)
          'cj/mu4e-attachment-entry entry))))
    (goto-char (point-min))
    (forward-line (max 0 (1- point-line)))))

(defun cj/mu4e--attachment-selection-setup (parts directory)
  "Populate the current selection buffer with attachment PARTS and DIRECTORY."
  (setq cj/mu4e-attachment-selection-directory directory)
  (setq cj/mu4e-attachment-selection-entries
        (mapcar (lambda (candidate)
                  (list :label (car candidate)
                        :part (cdr candidate)
                        :selected nil))
                (cj/mu4e--attachment-candidates parts)))
  (cj/mu4e--attachment-selection-render))

(defun cj/mu4e-attachment-selection-toggle ()
  "Toggle the attachment entry at point."
  (interactive)
  (let ((entry (cj/mu4e--attachment-selection-entry-at-point)))
    (setf (plist-get entry :selected)
          (not (plist-get entry :selected)))
    (cj/mu4e--attachment-selection-render)))

(defun cj/mu4e-attachment-selection-mark-all ()
  "Mark all attachments in the selection buffer."
  (interactive)
  (dolist (entry cj/mu4e-attachment-selection-entries)
    (setf (plist-get entry :selected) t))
  (cj/mu4e--attachment-selection-render))

(defun cj/mu4e-attachment-selection-unmark-all ()
  "Unmark all attachments in the selection buffer."
  (interactive)
  (dolist (entry cj/mu4e-attachment-selection-entries)
    (setf (plist-get entry :selected) nil))
  (cj/mu4e--attachment-selection-render))

(defun cj/mu4e-attachment-selection-save-marked ()
  "Save marked attachments from the selection buffer."
  (interactive)
  (let ((parts (mapcar (lambda (entry) (plist-get entry :part))
                       (seq-filter (lambda (entry)
                                     (plist-get entry :selected))
                                   cj/mu4e-attachment-selection-entries))))
    (unless parts
      (user-error "No attachments selected"))
    (let ((paths (cj/mu4e--save-attachment-parts
                  parts cj/mu4e-attachment-selection-directory)))
      (message "Saved %d attachment%s to %s"
               (length paths)
               (if (= (length paths) 1) "" "s")
               cj/mu4e-attachment-selection-directory)
      paths)))

(defun cj/mu4e--open-attachment-selection-buffer (parts directory)
  "Open an attachment selection buffer for PARTS and DIRECTORY."
  (let ((buffer (get-buffer-create "*mu4e attachments*")))
    (with-current-buffer buffer
      (cj/mu4e-attachment-selection-mode)
      (cj/mu4e--attachment-selection-setup parts directory))
    (pop-to-buffer buffer)))

(defun cj/mu4e-save-some-attachments ()
  "Prompt for a directory and open a buffer to select attachments to save."
  (interactive)
  (let ((parts (cj/mu4e--attachment-parts)))
    (unless parts
      (user-error "No attachments for this message"))
    (let ((directory (cj/mu4e--read-attachment-directory parts)))
      (cj/mu4e--open-attachment-selection-buffer parts directory))))

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
  ;; (setq mu4e-update-interval 600)                                      ;; check for new mail every 10 minutes (600 seconds)
  ;; TEMPORARILY DISABLED: Causing password prompts that interrupt work
  (setq mu4e-hide-index-messages t)                                       ;; don't show indexing messages buffer
  (setq mu4e-headers-from-or-to-prefix '("" . "➜ "))

  ;; Format=flowed for better plain text email handling
  ;; This will be automatically disabled when org-msg is active
  (setq mu4e-compose-format-flowed t)

  (setq mu4e-html2text-command 'mu4e-shr2text)  ;; email conversion to html via shr2text
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-get-mail-command (cj/mail--mbsync-command))                  ;; command to sync mail
  (setq mu4e-user-mail-address-list '("c@cjennings.net"
                                      "craigmartinjennings@gmail.com"
                                      "craig.jennings@deepsat.com"))
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
                  (mu4e-sent-folder     . "/cmail/Sent")))

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
  :vc (:url "https://github.com/jeremy-compostella/org-msg" :rev :newest)
  :defer 1
  :after (org mu4e)
  :preface
	(defvar-keymap cj/mail-cmail-map
	  :doc "cmail account navigation"
	  "i" (lambda () (interactive) (mu4e-search "maildir:/cmail/INBOX"))
	  "u" (lambda () (interactive) (mu4e-search "maildir:/cmail/INBOX AND flag:unread AND NOT flag:trashed"))
	  "s" (lambda () (interactive) (mu4e-search "maildir:/cmail/INBOX AND flag:flagged"))
	  "l" (lambda () (interactive) (mu4e-search "maildir:/cmail/INBOX AND size:5M..999M")))
	(defvar-keymap cj/mail-dmail-map
	  :doc "deepsat account navigation"
	  "i" (lambda () (interactive) (mu4e-search "maildir:/dmail/INBOX"))
	  "u" (lambda () (interactive) (mu4e-search "maildir:/dmail/INBOX AND flag:unread AND NOT flag:trashed"))
	  "s" (lambda () (interactive) (mu4e-search "maildir:/dmail/INBOX AND flag:flagged"))
	  "l" (lambda () (interactive) (mu4e-search "maildir:/dmail/INBOX AND size:5M..999M")))
	(defvar-keymap cj/mail-gmail-map
	  :doc "gmail account navigation"
	  "i" (lambda () (interactive) (mu4e-search "maildir:/gmail/INBOX"))
	  "u" (lambda () (interactive) (mu4e-search "maildir:/gmail/INBOX AND flag:unread AND NOT flag:trashed"))
	  "s" (lambda () (interactive) (mu4e-search "maildir:/gmail/INBOX AND flag:flagged"))
	  "l" (lambda () (interactive) (mu4e-search "maildir:/gmail/INBOX AND size:5M..999M")))
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
	(keymap-set cj/custom-keymap "e" cj/email-map)
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
