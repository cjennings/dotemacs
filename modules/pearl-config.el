;;; pearl-config.el --- Linear.app integration via pearl -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: deferred (command-loaded).
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none.
;; Direct test load: no.
;;
;; Near-vanilla pearl setup (local checkout instead of a package archive), in
;; multi-account mode: two Linear workspaces, deepsat (work) and craigjennings
;; (personal), named by Linear's own urlKey.  Each account renders to its own
;; Org file, deepsat.pearl.org / craigjennings.pearl.org, so they never collide.
;; `M-x pearl-switch-account' swaps the active one; the mode line shows it.
;;
;; pearl owns its own keymap.  `pearl-mode' turns on automatically in any buffer
;; pearl renders (it carries a `#+LINEAR-SOURCE' header) and binds the whole
;; command surface under `pearl-keymap-prefix' (default "C-; L").  This config
;; also binds that same `pearl-prefix-map' globally under C-; L (`:bind-keymap'),
;; so the full command surface is reachable from any buffer; the first press
;; autoloads pearl.  `M-x pearl-menu' / `M-x pearl-list-issues' still work too.
;;
;; Authentication: each account reads its key from authinfo.gpg by a distinct
;; login under the api.linear.app host:
;;   machine api.linear.app login apikey         password lin_api_<deepsat key>
;;   machine api.linear.app login pearl-personal password lin_api_<personal key>
;; Generate keys in Linear: Settings -> Security & access -> Personal API keys.

;;; Code:

(use-package pearl
  :ensure nil                       ;; local checkout, not from an archive
  :load-path "~/code/pearl"
  :commands (pearl-menu pearl-list-issues pearl-create-issue
             pearl-run-linear-view pearl-switch-account)
  ;; Bind pearl's command map globally under C-; L, so the full surface is
  ;; reachable from any buffer (not only inside a pearl-rendered one).  The
  ;; first press autoloads pearl; it's the same `pearl-prefix-map' that
  ;; `pearl-mode' binds in-buffer, so behavior is identical everywhere.
  :bind-keymap ("C-; L" . pearl-prefix-map)
  :custom
  ;; Shorten the assignee @-tag to the first name only (e.g. @first instead of
  ;; @first_last), trading disambiguation for a tighter tag line.
  (pearl-assignee-tag-short t)
  ;; Two workspaces, keyed by Linear's urlKey.  Each resolves its API key from
  ;; authinfo.gpg by its own login (see Commentary), renders to its own Org
  ;; file, and carries a default team so create / by-project skip the prompt.
  (pearl-accounts
   '(("deepsat"
      :api-key-source (:auth-source :host "api.linear.app" :user "apikey")
      :org-file "~/org/gtd/deepsat.pearl.org"
      :default-team-id "9fca2cf6-390c-4102-a9ff-f94a4ed823c5")        ;; DeepSat SE
     ("craigjennings"
      :api-key-source (:auth-source :host "api.linear.app" :user "pearl-personal")
      :org-file "~/org/gtd/craigjennings.pearl.org"
      :default-team-id "ee285e6c-fcc9-4dd6-9292-c47f2df75b82")))      ;; Pearl
  ;; Which workspace pearl opens into.  Work is primary; switch per-session at
  ;; runtime with `M-x pearl-switch-account' (e.g. to dogfood the personal
  ;; "craigjennings" workspace).
  (pearl-default-account "deepsat"))

(provide 'pearl-config)
;;; pearl-config.el ends here
