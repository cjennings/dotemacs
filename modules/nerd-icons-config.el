;;; nerd-icons-config.el --- Nerd-icons setup and integrations -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P.
;; Load shape: eager.
;; Eager reason: provides the nerd-icons fonts that dashboard, modeline, and
;;   completion render in the first frame.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none.
;; Direct test load: yes.
;;
;; Single home for nerd-icons:
;;   - the package itself
;;   - completion integration (`nerd-icons-completion')
;;   - ibuffer integration (`nerd-icons-ibuffer')
;;   - dir-icon color advice (so directory glyphs carry a color face like
;;     file glyphs do, instead of falling through to the buffer default
;;     face)
;;
;; Icon colors are theme-driven: nerd-icons' 34 `nerd-icons-*' color faces are
;; owned by the theme (themeable in theme-studio), not overwritten at load time.
;;
;; Per-feature USE of nerd-icons stays in the feature module that consumes
;; it: `dashboard-icon-type', `dirvish-attributes', and the keyboard-compat
;; terminal-frame icon-blanking advice are not centralized here.

;;; Code:

;; ------------------------------- Helpers -------------------------------------

(defun cj/--nerd-icons-color-dir (icon)
  "Layer `nerd-icons-yellow' onto ICON's face stack and return ICON.
ICON is the propertized string returned by `nerd-icons-icon-for-dir'.
Without this, directory icons render in the buffer default face — so a
buffer-local face-remap of `default' catches them unintentionally.

Idempotent: nerd-icons memoizes its return strings, and this advice modifies
in place, so a naive `add-face-text-property' would stack the face symbol on
every call. The `memq' check skips when the face is already present."
  (when (and (stringp icon) (> (length icon) 0))
    (let ((faces (ensure-list (get-text-property 0 'face icon))))
      (unless (memq 'nerd-icons-yellow faces)
        (add-face-text-property 0 (length icon) 'nerd-icons-yellow nil icon))))
  icon)

;; ------------------------------- Packages ------------------------------------

;; `:demand t' is required: `dashboard-config.el' calls
;; `nerd-icons-faicon' / `nerd-icons-mdicon' / `nerd-icons-devicon' at
;; load time to build `dashboard-navigator-buttons', so nerd-icons must
;; be loaded eagerly before dashboard-config requires.  The earlier
;; deferral attempt (commit d618bb46) broke startup with a
;; `void-function nerd-icons-faicon' error.
(use-package nerd-icons
  :demand t
  :config
  (advice-add 'nerd-icons-icon-for-dir :filter-return #'cj/--nerd-icons-color-dir))

;; Safety net: if this module is re-evaluated in a running Emacs where
;; nerd-icons is already loaded, `:config' above won't fire again --
;; ensure the dir advice still applies.
(with-eval-after-load 'nerd-icons
  (unless (advice-member-p #'cj/--nerd-icons-color-dir 'nerd-icons-icon-for-dir)
    (advice-add 'nerd-icons-icon-for-dir
                :filter-return #'cj/--nerd-icons-color-dir)))

(use-package nerd-icons-completion
  :demand t
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-icon t)
  (nerd-icons-ibuffer-color-icon t)
  (nerd-icons-ibuffer-human-readable-size t))

(provide 'nerd-icons-config)
;;; nerd-icons-config.el ends here
