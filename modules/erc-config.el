;;; erc-config --- Preferences for Emacs Relay Chat (IRC Client) -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Enhanced ERC configuration with multi-server support.
;;
;; Main keybindings:
;; - C-c e C           : Select and connect to a specific server
;; - C-c e c           : Join a channel on current server
;; - C-c e b           : Switch between ERC buffers across all servers
;; - C-c e l           : List connected servers
;; - C-c e q           : Quit current channel
;; - C-c e Q           : Quit ERC server

;;; Code:

;; Load cl-lib at compile time and runtime (lightweight, already loaded in most configs)
(require 'cl-lib)
(eval-when-compile (require 'erc)
                   (require 'user-constants))

;; ------------------------------------ ERC ------------------------------------
;; Server definitions and connection settings

(defvar cj/erc-nick "craigjennings"
  "Default IRC nickname for ERC connections.
Change this value to use a different nickname.")

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

;;;###autoload
(defun cj/erc-connect-server (server-name)
  "Connect to a server specified by SERVER-NAME from `cj/erc-server-alist'."
  (let ((server-info (assoc server-name cj/erc-server-alist)))
	(if (not server-info)
		(error "Server '%s' not found in cj/erc-server-alist" server-name)
	  (let ((host (plist-get (cdr server-info) :host))
			(port (plist-get (cdr server-info) :port))
			(tls (plist-get (cdr server-info) :tls)))
		(if tls
			(erc-tls :server host
					 :port port
					 :nick cj/erc-nick
					 :full-name user-whole-name)
		  (erc :server host
			   :port port
			   :nick cj/erc-nick
			   :full-name user-whole-name))))))


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
				   (mapconcat #'identity server-buffers ", "))
		(message "No active ERC server connections")))

	server-buffers))


(defun cj/erc-switch-to-buffer-with-completion ()
  "Switch to an ERC buffer using completion.
If no ERC buffers exist, prompt to connect to a server.
Buffer names are shown with server context for clarity."
  (interactive)
  (let* ((erc-buffers (erc-buffer-list))
		 (buffer-names (mapcar #'buffer-name erc-buffers)))
	(if buffer-names
		(let ((selected (completing-read "Switch to ERC buffer: " buffer-names nil t)))
		  (switch-to-buffer selected))
	  (message "No ERC buffers found.")
	  (when (y-or-n-p "Connect to an IRC server? ")
		(call-interactively 'cj/erc-connect-server-with-completion)))))


(defun cj/erc-server-buffer-active-p ()
  "Return t if the current buffer is an active ERC server buffer."
  (and (derived-mode-p 'erc-mode)
	   (erc-server-process-alive)
	   (erc-server-buffer-p)))


(defun cj/erc-get-channels-for-current-server ()
  "Get list of channels for the currently connected server."
  (when (and (derived-mode-p 'erc-mode) erc-server-process)
	(let* ((server-host (process-name erc-server-process))
		   (matching-server (cl-find-if
							 (lambda (server)
							   (string-match-p (plist-get (cdr server) :host) server-host))
							 cj/erc-server-alist)))
	  (when matching-server
		(plist-get (cdr matching-server) :channels)))))


(defun cj/erc-join-channel-with-completion ()
  "Join a channel on the current server.
If not in an active ERC server buffer, reconnect first.
Auto-adds # prefix if missing.  Offers completion from configured channels."
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
	  (let* ((channels (cj/erc-get-channels-for-current-server))
			 (channel (if channels
						  (completing-read "Join channel: " channels nil nil "#")
						(read-string "Join channel: " "#"))))
		;; Auto-add # prefix if missing
		(unless (string-prefix-p "#" channel)
		  (setq channel (concat "#" channel)))
		(when (> (length channel) 1)  ; Must have more than just #
		  (erc-join-channel channel)))
	(message "Failed to establish an active ERC connection")))

;; Main ERC configuration
(use-package erc
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

  (erc-nick cj/erc-nick)
  (erc-user-full-name user-whole-name)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-fill-column 120)
  (erc-fill-function #'erc-fill-static)
  (erc-fill-static-center 20)

  :config

  ;; Bind global keymap
  (keymap-global-set "C-c e" cj/erc-command-map)

  ;; use all text mode abbrevs in ercmode
  (abbrev-table-put erc-mode-abbrev-table :parents (list text-mode-abbrev-table))

  ;; create log directory if it doesn't exist
  (setq erc-log-channels-directory (concat user-emacs-directory "erc/logs/"))
  (if (not (file-exists-p erc-log-channels-directory))
	  (mkdir erc-log-channels-directory t))

  ;; Configure buffer naming to include server name
  ;; Note: erc-rename-buffers is obsolete as of Emacs 29.1 (old behavior is now permanent)
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
		erc-track-showcount t)

  ;; Add hooks for notifications and colorization
  (add-hook 'erc-text-matched-hook #'cj/erc-notify-on-mention)
  (add-hook 'erc-mode-hook #'cj/erc-colorize-setup))

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
  "Display a notification when MATCH-TYPE is \\='current-nick.

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
  (setq erc-nick-color-function #'erc-get-color-for-nick))

;; -------------------------------- ERC Image ---------------------------------
;; show inlined images (png/jpg/gif/svg) in erc buffers.

(use-package erc-image
  :if (locate-library "erc-image")
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

;; -------------------------------- ERC Nicks ---------------------------------
;; Nickname highlighting (built-in to Emacs 29+)

(use-package erc-nicks
  :ensure nil  ;; built-in
  :after erc
  :hook (erc-mode . erc-nicks-mode))

;; ------------------------------ ERC Yank To Gist -----------------------------
;; automatically create a Gist if pasting more than 5 lines
;; this module requires https://github.com/defunkt/gist
;; via ruby: 'gem install gist' via the aur: yay -S gist

(use-package erc-yank
  :if (locate-library "erc-yank")
  :after erc
  :bind
  (:map erc-mode-map
        ("C-y" . erc-yank)))



;; Keymap for ERC commands (bound in use-package :config to defer loading)
(defvar cj/erc-command-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C" #'cj/erc-connect-server-with-completion) ;; Connect to server (capital C)
    (keymap-set map "c" #'cj/erc-join-channel-with-completion)   ;; join channel (lowercase c)
    (keymap-set map "b" #'cj/erc-switch-to-buffer-with-completion) ;; switch Buffer
    (keymap-set map "l" #'cj/erc-connected-servers)              ;; print connected servers in echo area
    (keymap-set map "q" #'erc-part-from-channel)                 ;; quit channel
    (keymap-set map "Q" #'erc-quit-server)                       ;; Quit ERC entirely
    map)
  "Keymap for ERC-related commands.")

(provide 'erc-config)
;;; erc-config.el ends here
