;;; erc-config.el --- Preferences for Emacs Relay Chat (IRC Client) -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; IRC should not be a startup load, a command-loaded
;;   deferral candidate for Phase 5.
;; Top-level side effects: defines an ERC keymap, registers it under
;;   cj/custom-keymap, package configuration via use-package.
;; Runtime requires: cl-lib, keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Enhanced ERC configuration with multi-server support.
;;
;; Main keybindings:
;; - C-; E C           : Select and connect to a specific server
;; - C-; E c           : Join a channel on current server
;; - C-; E b           : Switch between ERC buffers across all servers
;; - C-; E l           : List connected servers
;; - C-; E q           : Quit current channel
;; - C-; E Q           : Quit ERC server

;;; Code:

;; Load cl-lib at compile time and runtime (lightweight, already loaded in most configs)
(require 'cl-lib)
(require 'keybindings)  ;; provides cj/custom-keymap
(eval-when-compile (require 'erc))
;; user-constants is required at runtime, not just compile time: `user-whole-name'
;; is read at load time below (erc-user-full-name), so a standalone .elc needs it.
(require 'user-constants)

;; ERC loads lazily (use-package :commands), so these symbols aren't bound at
;; this file's compile time.  Declare them to keep the byte-compiler quiet
;; without forcing an eager require.

;; Functions provided by the erc package.
(declare-function erc-buffer-list "erc")
(declare-function erc-server-process-alive "erc")
(declare-function erc-server-or-unjoined-channel-buffer-p "erc")
(declare-function erc-current-nick "erc")
(declare-function erc-join-channel "erc")
(declare-function erc-part-from-channel "erc")
(declare-function erc-quit-server "erc")

;; Variables read/set in the use-package :config block below.
(defvar erc-log-channels-directory)
(defvar erc-track-exclude-types)
(defvar erc-track-exclude-server-buffer)
(defvar erc-track-visibility)
(defvar erc-track-switch-direction)
(defvar erc-track-showcount)
;; NOTE: erc-unique-buffers and erc-generate-buffer-name-function are not ERC
;; variables in Emacs 30.x (no defcustom/defvar in the package); the setq below
;; only creates inert globals.  Declared here to silence the warning without
;; changing the existing (no-op) behavior -- see the SUSPICIOUS note.
(defvar erc-unique-buffers)
(defvar erc-generate-buffer-name-function)

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
		(when (and (erc-server-or-unjoined-channel-buffer-p) (erc-server-process-alive))
		  (unless (member (buffer-name) server-buffers)
			(push (buffer-name) server-buffers)))))

	;; Display the server list when called interactively
	(when (called-interactively-p 'any)
	  (if server-buffers
		  (message "Connected ERC servers: %s"
				   (mapconcat #'identity server-buffers ", "))
		(message "No active ERC server connections")))

	server-buffers))


(require 'system-lib)

(defun cj/erc-switch-to-buffer-with-completion ()
  "Switch to an ERC buffer using completion.
If no ERC buffers exist, prompt to connect to a server.
Buffer names are shown with server context for clarity."
  (interactive)
  (let* ((erc-buffers (erc-buffer-list))
		 (buffer-names (mapcar #'buffer-name erc-buffers)))
	(if buffer-names
		(let ((selected (completing-read "Switch to ERC buffer: " (cj/completion-table 'buffer buffer-names) nil t)))
		  (switch-to-buffer selected))
	  (message "No ERC buffers found.")
	  (when (y-or-n-p "Connect to an IRC server? ")
		(call-interactively 'cj/erc-connect-server-with-completion)))))


(defun cj/erc-server-buffer-active-p ()
  "Return t if the current buffer is an active ERC server buffer."
  (and (derived-mode-p 'erc-mode)
	   (erc-server-process-alive)
	   (erc-server-or-unjoined-channel-buffer-p)))


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
								   (and (erc-server-or-unjoined-channel-buffer-p)
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

(defun cj/erc-generate-buffer-name (parms)
  "Generate buffer name in the format SERVER-CHANNEL."
  (let ((network (plist-get parms :server))
        (target (plist-get parms :target)))
    (if target
        (concat (or network "") "-" (or target ""))
      (or network ""))))

;; Keymap for ERC commands (must be defined before use-package erc)
(defvar-keymap cj/erc-keymap
  :doc "Keymap for ERC-related commands"
  "C" #'cj/erc-connect-server-with-completion  ;; Connect to server
  "c" #'cj/erc-join-channel-with-completion    ;; join channel
  "b" #'cj/erc-switch-to-buffer-with-completion ;; switch Buffer
  "l" #'cj/erc-connected-servers               ;; list connected servers
  "q" #'erc-part-from-channel                  ;; quit channel
  "Q" #'erc-quit-server)                       ;; Quit ERC entirely

(cj/register-prefix-map "E" cj/erc-keymap)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; E" "ERC chat menu"
    "C-; E C" "connect server"
    "C-; E c" "join channel"
    "C-; E b" "switch buffer"
    "C-; E l" "list servers"
    "C-; E q" "quit channel"
    "C-; E Q" "quit server"))

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
	 image
	 irccontrols
	 list
	 log
	 match
	 move-to-prompt
	 noncommands
	 readonly
	 services
	 stamp
	 track))

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

  ;; use all text mode abbrevs in ercmode
  (abbrev-table-put erc-mode-abbrev-table :parents (list text-mode-abbrev-table))

  ;; create log directory if it doesn't exist
  (setq erc-log-channels-directory (concat user-emacs-directory "erc/logs/"))
  (if (not (file-exists-p erc-log-channels-directory))
	  (mkdir erc-log-channels-directory t))

  ;; Configure buffer naming to include server name
  ;; Note: erc-rename-buffers is obsolete as of Emacs 29.1 (old behavior is now permanent)
  (setq erc-unique-buffers t)

  ;; Custom buffer naming (cj/erc-generate-buffer-name is defined at top level)
  (setq erc-generate-buffer-name-function 'cj/erc-generate-buffer-name)

  ;; Configure erc-track (show channel activity in modeline)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
								  "324" "329" "332" "333" "353" "477")
		erc-track-exclude-server-buffer t
		erc-track-visibility 'selected-visible
		erc-track-switch-direction 'importance
		erc-track-showcount t)

  ;; Add hooks for notifications
  (add-hook 'erc-text-matched-hook #'cj/erc-notify-on-mention))

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

;; -------------------------------- ERC Image ---------------------------------
;; show inlined images (png/jpg/gif/svg) in erc buffers.

(use-package erc-image
  :after erc
  :config
  (setq erc-image-inline-rescale 300))

;; -------------------------------- ERC Nicks ---------------------------------
;; Nickname highlighting (built-in to Emacs 29+)

(use-package erc-nicks
  :ensure nil  ;; built-in
  :after erc
  :hook (erc-mode . erc-nicks-mode))

;; -------------------------------- ERC Yank ----------------------------------
;; The erc-yank package was dropped 2026-06-20: a paste over 5 lines became a
;; PUBLIC gist (it called `gist -P', the clipboard paste flag, with no
;; `--private'), behind only a single y-or-n-p and with no guard if the `gist'
;; binary was absent -- a one-keystroke path to publishing whatever sat on the
;; system clipboard.  No replacement binding is needed: erc-mode-map defines no
;; C-y of its own, so with erc-yank gone C-y falls through to the ordinary
;; global `yank' and a paste stays local.  Gist a large snippet by hand when
;; that's actually wanted.

(provide 'erc-config)
;;; erc-config.el ends here
