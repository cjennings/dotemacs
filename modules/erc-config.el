;;; erc-config --- Preferences for Emacs Relay Chat (IRC Client) -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Enhanced ERC configuration with multi-server support.
;;
;; Main keybindings:
;; - C-c e C           : Select and connect to a specific server
;; - C-c e c           : Join a channel on current server
;; - C-c e b           : Switch between ERC buffers across all servers
;; - C-c C-q           : Quit current channel
;; - C-c C-Q           : Quit ERC altogether

;;; Code:

;; Keymap for ERC commands
(defvar cj/erc-command-map
  (let ((map (make-sparse-keymap)))
	(define-key map "C" 'cj/erc-connect-server-with-completion) ;; Connect to server (capital C)
	(define-key map "c" 'cj/erc-join-channel-with-completion)   ;; join channel (lowercase c)
	(define-key map "b" 'cj/erc-switch-to-buffer-with-completion) ;; switch Buffer
	(define-key map "l" 'cj/erc-connected-servers)              ;; print connected servers in echo area
	(define-key map "q" 'erc-part-from-channel)                 ;; quit channel
	(define-key map "Q" 'erc-quit-server)                       ;; Quit ERC entirely
	map)
  "Keymap for ERC-related commands.")

(global-set-key (kbd "C-c e") cj/erc-command-map)

;; ------------------------------------ ERC ------------------------------------
;; Server definitions and connection settings

(defvar cj/erc-server-alist
  '(("Libera.Chat"
	 :host "irc.libera.chat"
	 :port 6697
	 :tls t
	 :channels ("#erc" "#emacs" "#emacs-social" "#systemcrafters"))

	("IRCnet"
	 :host "open.ircnet.net"
	 :port 6697
	 :tls t
	 :channels ("#english"))

	("Snoonet"
	 :host "irc.snoonet.org"
	 :port 6697
	 :tls t
     :channels ("#talk"))

	("IRCNow"
	 :host "irc.ircnow.org"
	 :port 6697
	 :tls t
	 :channels ("#general" "#lounge")))
  "Alist of IRC servers and their connection details.")

(defun cj/erc-connect-server (server-name)
  "Connect to a server specified by SERVER-NAME from =cj/erc-server-alist=."
  (let* ((server-info (assoc server-name cj/erc-server-alist))
		 (host (plist-get (cdr server-info) :host))
		 (port (plist-get (cdr server-info) :port))
		 (tls (plist-get (cdr server-info) :tls)))
	(if tls
		(erc-tls :server host
				 :port port
				 :nick "craigjennings"
				 :full-name user-whole-name)
	  (erc :server host
		   :port port
		   :nick "craigjennings"
		   :full-name user-whole-name))))

(defun cj/erc-connect-server-with-completion ()
  "Connect to a server using completion for server selection."
  (interactive)
  (let ((server-name (completing-read "Connect to IRC server: "
									  (mapcar #'car cj/erc-server-alist))))
	(cj/erc-connect-server server-name)))

(defun cj/erc-connected-servers ()
  "Return a list of currently connected servers and display them in echo area."
  (interactive)
  (let ((server-buffers '()))
	(dolist (buf (erc-buffer-list))
	  (with-current-buffer buf
		(when (eq (buffer-local-value 'erc-server-process buf) erc-server-process)
		  (unless (member (buffer-name) server-buffers)
			(push (buffer-name) server-buffers)))))

	;; Display the server list when called interactively
	(when (called-interactively-p 'any)
	  (if server-buffers
		  (message "Connected ERC servers: %s"
				   (mapconcat 'identity server-buffers ", "))
		(message "No active ERC server connections")))

	server-buffers))

(defun cj/erc-switch-to-buffer-with-completion ()
  "Switch to an ERC buffer using completion."
  (interactive)
  (let* ((erc-buffers (mapcar 'buffer-name (erc-buffer-list)))
		 (selected (completing-read "Switch to buffer: " erc-buffers)))
	(switch-to-buffer selected)))

(defun cj/erc-server-buffer-active-p ()
  "Return t if the current buffer is an active ERC server buffer."
  (and (derived-mode-p 'erc-mode)
	   (erc-server-process-alive)
	   (erc-server-buffer-p)))

(defun cj/erc-join-channel-with-completion ()
  "Join a channel on the current server.
If not in an active ERC server buffer, reconnect first."
  (interactive)
  (unless (cj/erc-server-buffer-active-p)
	(if (erc-buffer-list)
		;; We have ERC buffers, but current one isn't active
		(let ((server-buffers (cl-remove-if-not
							   (lambda (buf)
								 (with-current-buffer buf
								   (and (erc-server-buffer-p)
										(erc-server-process-alive))))
							   (erc-buffer-list))))
		  (if server-buffers
			  ;; Found active server buffer, switch to it
			  (switch-to-buffer (car server-buffers))
			;; No active server buffer, reconnect
			(message "No active ERC connection. Reconnecting...")
			(call-interactively 'cj/erc-connect-server-with-completion)))
	  ;; No ERC buffers at all, connect to a server
	  (message "No active ERC connection. Connecting to server first...")
	  (call-interactively 'cj/erc-connect-server-with-completion)))

  ;; At this point we should have an active connection
  (if (cj/erc-server-buffer-active-p)
	  (let ((channel (read-string "Join channel: ")))
		(when (string-prefix-p "#" channel)
		  (erc-join-channel channel)))
	(message "Failed to establish an active ERC connection")))


;; Main ERC configuration
(use-package erc
  :defer 1
  :ensure nil ;; built-in
  :commands (erc erc-tls)
  :hook
  (erc-mode . emojify-mode)
  :custom
  (erc-modules
   '(autojoin
	 button
	 completion
	 fill
	 irccontrols
	 list
	 log
	 match
	 move-to-prompt
	 noncommands
	 notifications
	 readonly
	 services
	 stamp
	 track))  ;; Added track module

  (erc-autojoin-channels-alist
   (mapcar (lambda (server)
			 (cons (car server)
				   (plist-get (cdr server) :channels)))
		   cj/erc-server-alist))

  (erc-nick "craigjennings")
  (erc-user-full-name user-whole-name)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-fill-column 120)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)

  :config


  ;; use all text mode abbrevs in ercmode
  (abbrev-table-put erc-mode-abbrev-table :parents (list text-mode-abbrev-table))

  ;; create log directory if it doesn't exist
  (setq erc-log-channels-directory (concat user-emacs-directory "erc/logs/"))
  (if (not (file-exists-p erc-log-channels-directory))
	  (mkdir erc-log-channels-directory t))

  ;; Configure buffer naming to include server name
  (setq erc-rename-buffers t)
  (setq erc-unique-buffers t)

  ;; Custom buffer naming function
  (defun cj/erc-generate-buffer-name (parms)
	"Generate buffer name in the format SERVER-CHANNEL."
	(let ((network (plist-get parms :server))
		  (target (plist-get parms :target)))
	  (if target
		  (concat (or network "") "-" (or target ""))
		(or network ""))))

  (setq erc-generate-buffer-name-function 'cj/erc-generate-buffer-name)

  ;; Configure erc-track (show channel activity in modeline)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
								  "324" "329" "332" "333" "353" "477")
		erc-track-exclude-server-buffer t
		erc-track-visibility 'selected-visible
		erc-track-switch-direction 'importance
		erc-track-showcount t))

;; -------------------------------- ERC Track ---------------------------------
;; Better tracking of activity across channels (already included in modules above)

(use-package erc-track
  :ensure nil  ;; built-in
  :after erc
  :custom
  (erc-track-position-in-mode-line 'before-modes)
  (erc-track-shorten-function 'erc-track-shorten-names)
  (erc-track-shorten-cutoff 8)
  (erc-track-shorten-start 1)
  (erc-track-priority-faces-only 'all)
  (erc-track-faces-priority-list
   '(erc-error-face
	 erc-current-nick-face
	 erc-keyword-face
	 erc-nick-msg-face
	 erc-direct-msg-face
	 erc-notice-face
	 erc-prompt-face)))

;; ------------------------ ERC Desktop Notifications ------------------------
;; Implementation for desktop notifications

(defun cj/erc-notify-on-mention (match-type nick message)
  "Display a notification when MATCH-TYPE is 'current-nick.
NICK is the sender and MESSAGE is the message text."
  (when (and (eq match-type 'current-nick)
			 (not (string= nick (erc-current-nick)))
			 (display-graphic-p))
	(let ((title (format "ERC: %s mentioned you" nick)))
	  ;; Use alert.el if available, otherwise fall back to notifications
	  (if (fboundp 'alert)
		  (alert message :title title :category 'erc)
		(when (fboundp 'notifications-notify)
		  (notifications-notify
		   :title title
		   :body message
		   :app-name "Emacs ERC"
		   :sound-name 'message))))))

(add-hook 'erc-text-matched-hook 'cj/erc-notify-on-mention)

;; ------------------------------ ERC Colorize -------------------------------
;; Better color management with built-in functionality

(defun cj/erc-colorize-setup ()
  "Setup ERC colorization for nicknames."
  (make-local-variable 'erc-nick-color-alist)
  (setq erc-nick-color-alist
		(cl-loop for i from 0 to 15
				 for color in '("blue" "green" "red" "brown" "purple"
								"olive" "dark cyan" "light gray" "dark gray"
								"light blue" "light green" "light red"
								"light brown" "light purple" "yellow" "white")
				 collect (cons i color)))
  (setq erc-nick-color-function 'erc-get-color-for-nick))

(add-hook 'erc-mode-hook 'cj/erc-colorize-setup)

;; -------------------------------- ERC Image ---------------------------------
;; show inlined images (png/jpg/gif/svg) in erc buffers.

(use-package erc-image
  :defer 1
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

;; -------------------------------- ERC Hl Nicks -------------------------------
;; uniquely identify names in ERC

(use-package erc-hl-nicks
  :defer 1
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks)
  (erc-update-modules))

;; ------------------------------ ERC Yank To Gist -----------------------------
;; automatically create a Gist if pasting more than 5 lines
;; this module requires https://github.com/defunkt/gist
;; via ruby: 'gem install gist' via the aur: yay -S gist

(use-package erc-yank
  :defer 1
  :after erc
  :bind
  (:map erc-mode-map
		("C-y" . erc-yank)))

(provide 'erc-config)
;;; erc-config.el ends here
