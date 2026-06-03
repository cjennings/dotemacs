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
;; Vanilla pearl setup, deliberately kept to exactly what pearl's README
;; documents for a first-time install — no custom keymap, no team default, no
;; lazy-key advice — so it can be dogfooded as the real out-of-box experience.
;; The only deviation from the README is loading from the local checkout
;; (~/code/pearl) instead of a package archive.
;;
;; pearl owns its own keymap.  `pearl-mode' turns on automatically in any buffer
;; pearl renders (it carries a `#+LINEAR-SOURCE' header) and binds the whole
;; command surface under `pearl-keymap-prefix' (default "C-; L").  This config
;; binds no global key, so from a non-Linear buffer reach pearl with `M-x'
;; (e.g. `M-x pearl-list-issues' or `M-x pearl-menu'); inside a Linear buffer
;; everything is live under C-; L.
;;
;; Authentication: the Linear personal API key is read from authinfo.gpg.  Add:
;;   machine api.linear.app login apikey password lin_api_YOURKEYHERE
;; Generate it in Linear: Settings -> Security & access -> Personal API keys.

;;; Code:

(use-package pearl
  :ensure nil                       ;; local checkout, not from an archive
  :load-path "~/code/pearl"
  :commands (pearl-menu pearl-list-issues pearl-create-issue pearl-run-linear-view)
  :custom
  (pearl-org-file-path (expand-file-name "gtd/linear.org" org-directory))
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
