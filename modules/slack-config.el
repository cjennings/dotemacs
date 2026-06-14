;;; slack-config.el --- Slack Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; optional Slack client, a command-loaded deferral
;;   candidate. Auth and which-key labels should be after-load.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: system-lib, cl-lib.
;; Direct test load: yes.
;;
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
;;   C-; S Q  — close all Slack buffers and windows
;;   C-; S S  — disconnect
;;
;; Compose buffer:
;;   C-<return> — send message

;;; Code:

(require 'system-lib)  ;; provides cj/auth-source-secret-value
(require 'cl-lib)
(require 'keybindings)  ;; provides cj/register-prefix-map

(defvar slack-current-buffer)
(defvar slack-message-compose-buffer-mode-map)
(defvar slack-message-custom-notifier)
(defvar slack-teams)

(declare-function slack-buffer-add-reaction-to-message "slack-buffer")
(declare-function slack-buffer-latest-ts "slack-buffer")
(declare-function slack-buffer-team "slack-buffer")
(declare-function slack-buffer-update-mark-request "slack-buffer")
(declare-function slack-get-ts "slack-util")
(declare-function slack-im-p "slack-im")
(declare-function slack-message-body "slack-message")
(declare-function slack-message-embed-channel "slack-message-buffer")
(declare-function slack-message-embed-mention "slack-message-buffer")
(declare-function slack-message-mentioned-p "slack-message")
(declare-function slack-message-minep "slack-message")
(declare-function slack-message-reaction-input "slack-message-reaction")
(declare-function slack-message-send-from-buffer "slack-message-sender")
(declare-function slack-message-write-another-buffer "slack-message-buffer")
(declare-function slack-reaction-echo-description "slack-buffer")
(declare-function slack-room-display-name "slack-room")
(declare-function slack-ws-close "slack")

(defvar cj/slack-workspace "deepsatworkspace.slack.com"
  "Slack workspace domain for auth-source lookup.")

(defun cj/slack--get-credential (login-key)
  "Look up LOGIN-KEY credential for the Slack workspace from auth-source."
  (cj/auth-source-secret-value cj/slack-workspace login-key))

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

(defun cj/slack--display-buffer (buffer)
  "Display Slack BUFFER in another window, never the selected one.
With a split, reuse one of the other windows rather than taking over the
window point is in or always spawning a fresh split; with a lone window,
split.  Wired as `slack-buffer-function' so opening a room or thread lands
beside the current work instead of clobbering it.  The default
`switch-to-buffer-other-window' picks a least-recently-used window with three
or more panes; this pins the choice to any non-selected window."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-use-some-window
                    display-buffer-pop-up-window)
                   (inhibit-same-window . t))))

(use-package slack
  :defer t
  :commands (slack-start slack-select-rooms slack-select-unread-rooms
             slack-im-select slack-thread-show-or-create
             slack-insert-emoji slack-register-team
             slack-message-write-another-buffer
             slack-message-embed-mention slack-message-embed-channel)
  :custom
  ;; Disabled: emojify-mode in lui buffers causes (wrong-type-argument listp)
  ;; errors on emoji characters during lui-scroll-post-command's recenter call.
  ;; Native emoji rendering via Noto Color Emoji fontset works fine without it.
  ;; Re-enable if emojify/circe fix the interaction. (2026-03-16)
  (slack-buffer-emojify nil)
  (slack-prefer-current-team t)
  ;; Open rooms/threads in another window, never the selected one (see
  ;; cj/slack--display-buffer) so Slack lands beside the current work in a split.
  (slack-buffer-function #'cj/slack--display-buffer)
  :config
  (setq slack-message-custom-notifier #'cj/slack-notify))

;; ------------------------------ Reactions ------------------------------------

(defvar cj/slack-common-reactions
  '(("thumbs up" . "+1")
    ("thumbsup" . "thumbsup")
    ("thumbs down" . "-1")
    ("pray" . "pray")
    ("raised hands" . "raised_hands")
    ("eyes" . "eyes")
    ("white check mark" . "white_check_mark")
    ("heavy check mark" . "heavy_check_mark")
    ("plus one" . "+1")
    ("heart" . "heart")
    ("heart eyes" . "heart_eyes")
    ("joy" . "joy")
    ("laughing" . "laughing")
    ("smile" . "smile")
    ("thinking face" . "thinking_face")
    ("rocket" . "rocket")
    ("fire" . "fire")
    ("party" . "tada")
    ("clap" . "clap")
    ("ok hand" . "ok_hand"))
  "Curated common Slack reaction labels mapped to Slack emoji names.")

(defun cj/slack--safe-reaction-echo-description (orig-fun &rest args)
  "Call ORIG-FUN safely from `post-command-hook'.
If emacs-slack sees a malformed reaction text property, remove the local hook
so the Slack buffer stays usable."
  (condition-case err
      (apply orig-fun args)
    (error
     (remove-hook 'post-command-hook #'slack-reaction-echo-description t)
     (message "Slack reaction hover disabled in this buffer: %s"
              (error-message-string err)))))

(defun cj/slack--reaction-candidates ()
  "Return display candidates for `cj/slack-common-reactions'."
  (append
   (mapcar (lambda (entry)
             (let ((label (car entry))
                   (name (cdr entry)))
               (cons (format "%-18s :%s:" label name) name)))
           cj/slack-common-reactions)
   '(("Other..." . :other))))

(defun cj/slack-select-reaction (team)
  "Select a Slack reaction for TEAM, preferring a short common list."
  (let* ((candidates (cj/slack--reaction-candidates))
         (choice (completing-read "Reaction: " candidates nil t))
         (reaction (cdr (assoc choice candidates))))
    (if (eq reaction :other)
        (slack-message-reaction-input team)
      reaction)))

(defun cj/slack-message-add-reaction ()
  "Add a reaction to the current Slack message using a curated shortlist.
Errors if called outside a Slack message buffer."
  (interactive)
  (let ((buf (or slack-current-buffer
                 (user-error "Not in a Slack buffer"))))
    (when-let* ((team (slack-buffer-team buf))
                (reaction (cj/slack-select-reaction team)))
      (slack-buffer-add-reaction-to-message buf reaction (slack-get-ts)))))

(with-eval-after-load 'slack-buffer
  (advice-add 'slack-reaction-echo-description
              :around #'cj/slack--safe-reaction-echo-description))

;; ----------------------------- Notifications ---------------------------------

(defun cj/slack-notify (message room team)
  "Send desktop notification for DMs and @mentions only.
MESSAGE is the incoming slack message, ROOM is the channel/DM,
TEAM is the slack team object.
Errors are logged to *Messages* since the websocket library silently
swallows exceptions via `websocket-try-callback'."
  (condition-case err
      (when (and (not (slack-message-minep message team))
                 (or (slack-im-p room)
                     (slack-message-mentioned-p message team)))
        (let ((title (format "Slack: %s" (slack-room-display-name room team)))
              (body (or (slack-message-body message team) "")))
          (start-process "slack-notify" nil
                         "notify" "info" title body)))
    (error (message "cj/slack-notify error: %S" err))))

(defun cj/slack-test-notify ()
  "Send a test desktop notification to verify the notify pipeline works."
  (interactive)
  (condition-case err
      (start-process "slack-notify-test" nil
                     "notify" "info" "Slack: Test" "Notification pipeline works")
    (error (message "cj/slack-test-notify error: %S" err))))

(defun cj/slack-mark-read-and-bury ()
  "Mark the current Slack channel as read and bury the buffer."
  (interactive)
  (when (and (boundp 'slack-current-buffer) slack-current-buffer)
    (let ((ts (slack-buffer-latest-ts slack-current-buffer)))
      (when ts
        (slack-buffer-update-mark-request slack-current-buffer ts))))
  (bury-buffer))

(defun cj/slack-close-all-buffers ()
  "Kill all Slack buffers and delete their windows."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (and (buffer-local-boundp 'slack-current-buffer buf)
                 (buffer-local-value 'slack-current-buffer buf))
        (let ((win (get-buffer-window buf t)))
          (when (and win (not (window-dedicated-p win)))
            (delete-window win)))
        (kill-buffer buf)
        (cl-incf count)))
    (message "Closed %d Slack buffer%s" count (if (= count 1) "" "s"))))

;; ------------------------------ Keybindings ----------------------------------

(defvar cj/slack-keymap (make-sparse-keymap)
  "Keymap for Slack commands under C-; S.")

(cj/register-prefix-map "S" cj/slack-keymap "slack")

(define-key cj/slack-keymap (kbd "s") #'cj/slack-start)
(define-key cj/slack-keymap (kbd "c") #'slack-select-unread-rooms)
(define-key cj/slack-keymap (kbd "C") #'slack-select-rooms)
(define-key cj/slack-keymap (kbd "d") #'slack-im-select)
(define-key cj/slack-keymap (kbd "w") #'slack-message-write-another-buffer)
(define-key cj/slack-keymap (kbd "r") #'slack-thread-show-or-create)
(define-key cj/slack-keymap (kbd "e") #'slack-insert-emoji)
(define-key cj/slack-keymap (kbd "!") #'cj/slack-message-add-reaction)
(define-key cj/slack-keymap (kbd "@") #'slack-message-embed-mention)
(define-key cj/slack-keymap (kbd "#") #'slack-message-embed-channel)
(define-key cj/slack-keymap (kbd "q") #'cj/slack-mark-read-and-bury)
(define-key cj/slack-keymap (kbd "Q") #'cj/slack-close-all-buffers)
(define-key cj/slack-keymap (kbd "S") #'cj/slack-stop)

;; Register which-key labels lazily so this module's require doesn't
;; depend on which-key being loaded.  Other config modules use the same
;; pattern.
(with-eval-after-load 'which-key
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
    "Q" "close all slack"
    "S" "disconnect"))

;; Send from compose buffer with C-<return>
(with-eval-after-load 'slack-message-compose-buffer
  (define-key slack-message-compose-buffer-mode-map (kbd "C-<return>") #'slack-message-send-from-buffer))

(provide 'slack-config)
;;; slack-config.el ends here.
