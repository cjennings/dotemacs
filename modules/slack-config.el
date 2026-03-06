;;; slack-config.el --- Slack Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Slack client using emacs-slack (https://github.com/emacs-slack/emacs-slack).
;;
;; Authentication:
;;   Credentials are stored in authinfo.gpg (never plaintext).
;;   Add an entry like:
;;     machine deepsatworkspace.slack.com login TOKEN password xoxc-YOUR-TOKEN
;;     machine deepsatworkspace.slack.com login COOKIE password xoxd-YOUR-COOKIE
;;
;;   To get these values, run M-x slack-refresh-token and follow the
;;   instructions to extract them from your browser's developer tools.
;;
;; Keybindings (C-; S prefix):
;;   C-; S s  — connect to Slack
;;   C-; S c  — select unread rooms
;;   C-; S C  — select any channel/room
;;   C-; S d  — direct message a person
;;   C-; S w  — compose message in separate buffer
;;   C-; S r  — reply / show thread
;;   C-; S e  — insert emoji
;;   C-; S !  — add reaction to message
;;   C-; S @  — embed @mention
;;   C-; S #  — embed #channel
;;   C-; S q  — mark channel read and bury buffer
;;   C-; S S  — disconnect
;;
;; Compose buffer:
;;   C-<return> — send message

;;; Code:

(require 'auth-source)

(defvar cj/slack-workspace "deepsatworkspace.slack.com"
  "Slack workspace domain for auth-source lookup.")

(defun cj/slack--get-credential (login-key)
  "Look up LOGIN-KEY credential for the Slack workspace from auth-source."
  (let ((entry (car (auth-source-search :host cj/slack-workspace
                                        :user login-key
                                        :max 1))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun cj/slack-start ()
  "Connect to Slack, registering the team if needed."
  (interactive)
  (require 'slack)
  (let ((token (cj/slack--get-credential "TOKEN"))
        (cookie (cj/slack--get-credential "COOKIE")))
    (unless token
      (user-error "No Slack token found in authinfo for %s (login: TOKEN)" cj/slack-workspace))
    (unless cookie
      (user-error "No Slack cookie found in authinfo for %s (login: COOKIE)" cj/slack-workspace))
    (unless (and (boundp 'slack-teams) slack-teams)
      (slack-register-team
       :name "DeepSat"
       :token token
       :cookie cookie
       :full-and-display-names t
       :default t))
    (slack-start)))

(defun cj/slack-stop ()
  "Disconnect from Slack."
  (interactive)
  (require 'slack)
  (slack-ws-close)
  (message "Slack disconnected."))

(use-package slack
  :defer t
  :commands (slack-start slack-select-rooms slack-select-unread-rooms
             slack-im-select slack-thread-show-or-create
             slack-insert-emoji slack-register-team)
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  :config
  (setq slack-message-custom-notifier #'cj/slack-notify))

;; ----------------------------- Notifications ---------------------------------

(defun cj/slack-notify (message room team)
  "Send desktop notification for DMs and @mentions only.
MESSAGE is the incoming slack message, ROOM is the channel/DM,
TEAM is the slack team object."
  (when (and (not (slack-message-minep message team))
             (or (slack-im-p room)
                 (slack-message-mentioned-p message)))
    (let ((title (format "Slack: %s" (slack-room-display-name room team)))
          (body (slack-message-body message team)))
      (start-process "slack-notify" nil
                     "notify" "info" title body))))

(defun cj/slack-mark-read-and-bury ()
  "Mark the current Slack channel as read and bury the buffer."
  (interactive)
  (when (and (boundp 'slack-current-buffer) slack-current-buffer)
    (let ((ts (slack-buffer-latest-ts slack-current-buffer)))
      (when ts
        (slack-buffer-update-mark-request slack-current-buffer ts))))
  (bury-buffer))

;; ------------------------------ Keybindings ----------------------------------

(defvar cj/slack-keymap (make-sparse-keymap)
  "Keymap for Slack commands under C-; S.")

(global-set-key (kbd "C-; S") cj/slack-keymap)

(define-key cj/slack-keymap (kbd "s") #'cj/slack-start)
(define-key cj/slack-keymap (kbd "c") #'slack-select-unread-rooms)
(define-key cj/slack-keymap (kbd "C") #'slack-select-rooms)
(define-key cj/slack-keymap (kbd "d") #'slack-im-select)
(define-key cj/slack-keymap (kbd "w") #'slack-message-write-another-buffer)
(define-key cj/slack-keymap (kbd "r") #'slack-thread-show-or-create)
(define-key cj/slack-keymap (kbd "e") #'slack-insert-emoji)
(define-key cj/slack-keymap (kbd "!") #'slack-message-add-reaction)
(define-key cj/slack-keymap (kbd "@") #'slack-message-embed-mention)
(define-key cj/slack-keymap (kbd "#") #'slack-message-embed-channel)
(define-key cj/slack-keymap (kbd "q") #'cj/slack-mark-read-and-bury)
(define-key cj/slack-keymap (kbd "S") #'cj/slack-stop)

(which-key-add-keymap-based-replacements cj/slack-keymap
  "" "slack menu"
  "s" "start slack"
  "c" "unread rooms"
  "C" "select channel"
  "d" "direct message"
  "w" "compose message"
  "r" "reply / thread"
  "e" "insert emoji"
  "!" "add reaction"
  "@" "embed @mention"
  "#" "embed #channel"
  "q" "mark read & bury"
  "S" "disconnect")

;; Send from compose buffer with C-<return>
(with-eval-after-load 'slack-message-compose-buffer
  (define-key slack-message-compose-buffer-mode-map (kbd "C-<return>") #'slack-message-send-from-buffer))

(provide 'slack-config)
;;; slack-config.el ends here.
