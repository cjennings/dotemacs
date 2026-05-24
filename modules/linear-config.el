;;; linear-config.el --- Linear.app integration -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Wires the local linear-emacs checkout (~/code/linear-emacs) into the config,
;; pointed at DeepSat's Linear workspace.
;;
;; Authentication:
;;   The Linear personal API key is read from authinfo.gpg, never plaintext.
;;   Add an entry like:
;;     machine api.linear.app login apikey password lin_api_YOURKEYHERE
;;   Generate the key in Linear: Settings -> Security & access -> Personal API
;;   keys.  The key is loaded lazily on first use, so there is no GPG prompt at
;;   startup.
;;
;; The default team is DeepSat's Software Engineering team (the SE-* issues), so
;; new issues land there unless another team is chosen.  The synced issues file
;; lives at data/linear.org inside emacs home, next to the calendar-sync output.
;;
;; Keybindings (C-; L prefix):
;;   Lists/views:  l list   p by project   f filtered   q saved query   v view
;;   Refresh:      g current view   r current issue
;;   Issue:        o open in browser   n new   D delete at point
;;   Edit (C-; L e):  a assignee   s state   p priority   b labels   c comment
;;   Sync:         s enable org sync   S disable   u push issue   U push title
;;   View:         V open current view in Linear
;;   Maintenance:  t test connection   ? check setup   k clear cache   d debug

;;; Code:

(require 'system-lib)  ;; provides cj/auth-source-secret-value

;; Owned by linear-emacs, which loads lazily via :load-path below.
(defvar linear-emacs-api-key)
(defvar linear-emacs-default-team-id)
(defvar linear-emacs-org-file-path)
(declare-function linear-emacs--graphql-request-async "linear-emacs")

(defconst cj/linear-team-id "9fca2cf6-390c-4102-a9ff-f94a4ed823c5"
  "Linear team id for DeepSat's Software Engineering team (the SE-* issues).")

(defun cj/linear--ensure-api-key ()
  "Load the Linear API key from authinfo.gpg into `linear-emacs-api-key' if unset.
Looks up host \"api.linear.app\".  This is a no-op once the key is set, so the
GPG prompt fires at most once per session and only when Linear is actually used."
  (unless linear-emacs-api-key
    (setq linear-emacs-api-key (cj/auth-source-secret-value "api.linear.app"))))

(defun cj/linear--ensure-key-before (&rest _)
  "Advice: load the Linear API key before a GraphQL request runs.
Named (not a lambda) so the advice is idempotent across reloads and removable."
  (cj/linear--ensure-api-key))

(defun cj/linear--install-key-advice ()
  "Install the lazy API-key loader on every entry point that needs the key.
The GraphQL request funnels all real operations.  `linear-emacs-check-setup'
reads `linear-emacs-api-key' directly without making a request, so it needs the
loader too — otherwise it reports \"not set\" on a fresh session before the key
has ever been fetched."
  (advice-add 'linear-emacs--graphql-request-async :before
              #'cj/linear--ensure-key-before)
  (advice-add 'linear-emacs-check-setup :before
              #'cj/linear--ensure-key-before))

(use-package linear-emacs
  :ensure nil                       ;; local checkout, not from an archive
  :load-path "~/code/linear-emacs"
  :defer t
  :commands (linear-emacs-list-issues
             linear-emacs-list-issues-by-project
             linear-emacs-list-issues-filtered
             linear-emacs-run-saved-query
             linear-emacs-run-view
             linear-emacs-refresh-current-view
             linear-emacs-refresh-current-issue
             linear-emacs-open-current-issue
             linear-emacs-open-current-view-in-linear
             linear-emacs-new-issue
             linear-emacs-delete-current-issue
             linear-emacs-add-comment
             linear-emacs-set-assignee
             linear-emacs-set-state
             linear-emacs-set-priority
             linear-emacs-set-labels
             linear-emacs-sync-current-issue
             linear-emacs-sync-current-issue-title
             linear-emacs-enable-org-sync
             linear-emacs-disable-org-sync
             linear-emacs-clear-cache
             linear-emacs-toggle-debug
             linear-emacs-load-api-key-from-env
             linear-emacs-test-connection
             linear-emacs-check-setup)
  :config
  (setq linear-emacs-default-team-id cj/linear-team-id)
  ;; Keep the synced org file inside emacs home, next to the calendar-sync
  ;; output (gcal.org / pcal.org / dcal.org).  Without this it falls back to
  ;; `org-directory'/gtd/linear.org and silently creates a stray ~/org tree.
  (setq linear-emacs-org-file-path
        (expand-file-name "data/linear.org" user-emacs-directory))
  ;; Load the key lazily before any operation that reads it — both the GraphQL
  ;; request and the check-setup diagnostic.  Retries if the key was added to
  ;; authinfo after a first (failed) attempt this session.
  (cj/linear--install-key-advice))

;; ------------------------------ Keybindings ----------------------------------

(defvar cj/linear-edit-keymap (make-sparse-keymap)
  "Keymap for editing the Linear issue at point, under C-; L e.")

(defvar cj/linear-keymap (make-sparse-keymap)
  "Keymap for Linear commands under C-; L.")

(global-set-key (kbd "C-; L") cj/linear-keymap)

;; Lists and views.
(define-key cj/linear-keymap (kbd "l") #'linear-emacs-list-issues)
(define-key cj/linear-keymap (kbd "p") #'linear-emacs-list-issues-by-project)
(define-key cj/linear-keymap (kbd "f") #'linear-emacs-list-issues-filtered)
(define-key cj/linear-keymap (kbd "q") #'linear-emacs-run-saved-query)
(define-key cj/linear-keymap (kbd "v") #'linear-emacs-run-view)
;; Refresh.
(define-key cj/linear-keymap (kbd "g") #'linear-emacs-refresh-current-view)
(define-key cj/linear-keymap (kbd "r") #'linear-emacs-refresh-current-issue)
;; Issue actions.
(define-key cj/linear-keymap (kbd "o") #'linear-emacs-open-current-issue)
(define-key cj/linear-keymap (kbd "V") #'linear-emacs-open-current-view-in-linear)
(define-key cj/linear-keymap (kbd "n") #'linear-emacs-new-issue)
(define-key cj/linear-keymap (kbd "D") #'linear-emacs-delete-current-issue)
;; Sync.
(define-key cj/linear-keymap (kbd "s") #'linear-emacs-enable-org-sync)
(define-key cj/linear-keymap (kbd "S") #'linear-emacs-disable-org-sync)
(define-key cj/linear-keymap (kbd "u") #'linear-emacs-sync-current-issue)
(define-key cj/linear-keymap (kbd "U") #'linear-emacs-sync-current-issue-title)
;; Maintenance.
(define-key cj/linear-keymap (kbd "t") #'linear-emacs-test-connection)
(define-key cj/linear-keymap (kbd "?") #'linear-emacs-check-setup)
(define-key cj/linear-keymap (kbd "k") #'linear-emacs-clear-cache)
(define-key cj/linear-keymap (kbd "d") #'linear-emacs-toggle-debug)
;; Edit-issue sub-prefix.
(define-key cj/linear-keymap (kbd "e") cj/linear-edit-keymap)
(define-key cj/linear-edit-keymap (kbd "a") #'linear-emacs-set-assignee)
(define-key cj/linear-edit-keymap (kbd "s") #'linear-emacs-set-state)
(define-key cj/linear-edit-keymap (kbd "p") #'linear-emacs-set-priority)
(define-key cj/linear-edit-keymap (kbd "b") #'linear-emacs-set-labels)
(define-key cj/linear-edit-keymap (kbd "c") #'linear-emacs-add-comment)

;; Register which-key labels lazily so this module's require doesn't depend on
;; which-key being loaded.  Same pattern as the other client modules.
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements cj/linear-keymap
    "" "linear menu"
    "l" "list issues"
    "p" "issues by project"
    "f" "filtered issues"
    "q" "saved query"
    "v" "run view"
    "g" "refresh view"
    "r" "refresh issue"
    "o" "open issue in browser"
    "V" "open view in linear"
    "n" "new issue"
    "D" "delete issue"
    "s" "enable org sync"
    "S" "disable org sync"
    "u" "push issue"
    "U" "push issue title"
    "t" "test connection"
    "?" "check setup"
    "k" "clear cache"
    "d" "toggle debug"
    "e" "edit issue")
  (which-key-add-keymap-based-replacements cj/linear-edit-keymap
    "a" "set assignee"
    "s" "set state"
    "p" "set priority"
    "b" "set labels"
    "c" "add comment"))

(provide 'linear-config)
;;; linear-config.el ends here
