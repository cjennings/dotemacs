;;; linear-config.el --- Linear.app integration -*- lexical-binding: t; -*-
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
;; Near-vanilla pearl setup: close to what pearl's README documents for a
;; first-time install (local checkout instead of a package archive), with two
;; deliberate tweaks layered on after dogfooding the out-of-box experience — a
;; global C-; L prefix (see below) and the shorter assignee @-tag.
;;
;; pearl owns its own keymap.  `pearl-mode' turns on automatically in any buffer
;; pearl renders (it carries a `#+LINEAR-SOURCE' header) and binds the whole
;; command surface under `pearl-keymap-prefix' (default "C-; L").  This config
;; also binds that same `pearl-prefix-map' globally under C-; L (`:bind-keymap'),
;; so the full command surface is reachable from any buffer; the first press
;; autoloads pearl.  `M-x pearl-menu' / `M-x pearl-list-issues' still work too.
;;
;; Authentication: the Linear personal API key is read from authinfo.gpg.  Add:
;;   machine api.linear.app login apikey password lin_api_YOURKEYHERE
;; Generate it in Linear: Settings -> Security & access -> Personal API keys.

;;; Code:

(use-package pearl
  :ensure nil                       ;; local checkout, not from an archive
  :load-path "~/code/pearl"
  :commands (pearl-menu pearl-list-issues pearl-create-issue pearl-run-linear-view)
  ;; Bind pearl's command map globally under C-; L, so the full surface is
  ;; reachable from any buffer (not only inside a pearl-rendered one).  The
  ;; first press autoloads pearl; it's the same `pearl-prefix-map' that
  ;; `pearl-mode' binds in-buffer, so behavior is identical everywhere.
  :bind-keymap ("C-; L" . pearl-prefix-map)
  :custom
  (pearl-org-file-path (expand-file-name "gtd/linear.org" org-directory))
  ;; Shorten the assignee @-tag to the first name only (e.g. @first instead of
  ;; @first_last), trading disambiguation for a tighter tag line.
  (pearl-assignee-tag-short t)
  ;; Optional defaults — uncomment and fill in to skip the prompts.  Set them
  ;; HERE, at init level, not via M-x pearl-set-default-view /
  ;; pearl-set-default-team: those persist through `customize-save-variable',
  ;; and this config redirects `custom-file' to a throwaway temp file
  ;; (system-defaults.el), so a setter's value is discarded on the next
  ;; restart.  These :custom lines re-apply on every startup instead.
  ;; (pearl-default-view "My active work")  ;; the local view `C-; L l' opens
  ;; (pearl-default-team-id "9fca2cf6-390c-4102-a9ff-f94a4ed823c5")  ;; DeepSat SE; skips the team prompt on create / by-project
  :config
  (setq pearl-api-key
        (auth-source-pick-first-password :host "api.linear.app")))

(provide 'linear-config)
;;; linear-config.el ends here
