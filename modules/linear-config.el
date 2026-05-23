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
;; new issues land there unless another team is chosen.
;;
;; Keybindings (C-; L prefix):
;;   C-; L l  — list issues
;;   C-; L p  — list issues by project
;;   C-; L n  — new issue
;;   C-; L s  — enable org sync
;;   C-; L S  — disable org sync
;;   C-; L t  — test connection
;;   C-; L ?  — check setup

;;; Code:

(require 'system-lib)  ;; provides cj/auth-source-secret-value

;; Owned by linear-emacs, which loads lazily via :load-path below.
(defvar linear-emacs-api-key)
(defvar linear-emacs-default-team-id)
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

(use-package linear-emacs
  :ensure nil                       ;; local checkout, not from an archive
  :load-path "~/code/linear-emacs"
  :defer t
  :commands (linear-emacs-list-issues
             linear-emacs-list-issues-by-project
             linear-emacs-new-issue
             linear-emacs-enable-org-sync
             linear-emacs-disable-org-sync
             linear-emacs-test-connection
             linear-emacs-check-setup)
  :config
  (setq linear-emacs-default-team-id cj/linear-team-id)
  ;; Load the key before any GraphQL request — lazy, and it retries if the key
  ;; was added to authinfo after a first (failed) attempt this session.
  (advice-add 'linear-emacs--graphql-request-async :before
              #'cj/linear--ensure-key-before))

;; ------------------------------ Keybindings ----------------------------------

(defvar cj/linear-keymap (make-sparse-keymap)
  "Keymap for Linear commands under C-; L.")

(global-set-key (kbd "C-; L") cj/linear-keymap)

(define-key cj/linear-keymap (kbd "l") #'linear-emacs-list-issues)
(define-key cj/linear-keymap (kbd "p") #'linear-emacs-list-issues-by-project)
(define-key cj/linear-keymap (kbd "n") #'linear-emacs-new-issue)
(define-key cj/linear-keymap (kbd "s") #'linear-emacs-enable-org-sync)
(define-key cj/linear-keymap (kbd "S") #'linear-emacs-disable-org-sync)
(define-key cj/linear-keymap (kbd "t") #'linear-emacs-test-connection)
(define-key cj/linear-keymap (kbd "?") #'linear-emacs-check-setup)

;; Register which-key labels lazily so this module's require doesn't depend on
;; which-key being loaded.  Same pattern as the other client modules.
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements cj/linear-keymap
    "" "linear menu"
    "l" "list issues"
    "p" "issues by project"
    "n" "new issue"
    "s" "enable org sync"
    "S" "disable org sync"
    "t" "test connection"
    "?" "check setup"))

(provide 'linear-config)
;;; linear-config.el ends here