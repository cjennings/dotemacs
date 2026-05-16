;;; nerd-icons-config.el --- Nerd-icons setup, integrations, and tinting -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Single home for nerd-icons:
;;   - the package itself
;;   - completion integration (`nerd-icons-completion')
;;   - ibuffer integration (`nerd-icons-ibuffer')
;;   - bulk color tinting of every `nerd-icons-*' color face
;;   - dir-icon color advice (so directory glyphs carry a color face like
;;     file glyphs do, instead of falling through to the buffer default
;;     face)
;;
;; Per-feature USE of nerd-icons stays in the feature module that consumes
;; it: `dashboard-icon-type', `dirvish-attributes', and the keyboard-compat
;; terminal-frame icon-blanking advice are not centralized here.

;;; Code:

;; ----------------------------- Customization ---------------------------------

(defcustom cj/nerd-icons-tint-color "darkgoldenrod"
  "Single foreground color applied to every `nerd-icons-*' color face.
Set via Customize or by `setq' before this module loads, then call
`cj/nerd-icons-apply-tint' to re-apply on demand."
  :type 'string
  :group 'cj)

(defconst cj/--nerd-icons-color-faces
  '(nerd-icons-red       nerd-icons-lred       nerd-icons-dred       nerd-icons-red-alt
    nerd-icons-green     nerd-icons-lgreen     nerd-icons-dgreen
    nerd-icons-yellow    nerd-icons-lyellow    nerd-icons-dyellow
    nerd-icons-orange    nerd-icons-lorange    nerd-icons-dorange
    nerd-icons-blue      nerd-icons-blue-alt   nerd-icons-lblue      nerd-icons-dblue
    nerd-icons-cyan      nerd-icons-cyan-alt   nerd-icons-lcyan      nerd-icons-dcyan
    nerd-icons-purple    nerd-icons-purple-alt nerd-icons-lpurple    nerd-icons-dpurple
    nerd-icons-pink      nerd-icons-lpink      nerd-icons-dpink
    nerd-icons-maroon    nerd-icons-lmaroon    nerd-icons-dmaroon
    nerd-icons-silver    nerd-icons-lsilver    nerd-icons-dsilver)
  "Every color face nerd-icons attaches to glyphs via `:inherit'.")

;; ------------------------------- Helpers -------------------------------------

(defun cj/nerd-icons-apply-tint (&optional color)
  "Set every face in `cj/--nerd-icons-color-faces' to foreground COLOR.
COLOR defaults to `cj/nerd-icons-tint-color'. Faces that are not yet
defined (nerd-icons not loaded) are silently skipped."
  (interactive)
  (let ((c (or color cj/nerd-icons-tint-color)))
    (dolist (f cj/--nerd-icons-color-faces)
      (when (facep f)
        (set-face-foreground f c)))))

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

(use-package nerd-icons
  :defer t
  :config
  (advice-add 'nerd-icons-icon-for-dir :filter-return #'cj/--nerd-icons-color-dir)
  (cj/nerd-icons-apply-tint))

;; If nerd-icons is already loaded (e.g. when this module is re-evaluated
;; after a session in which a feature module already required it), the
;; `:config' block above won't fire again -- fall through to install the
;; advice and tint immediately.
(with-eval-after-load 'nerd-icons
  (unless (advice-member-p #'cj/--nerd-icons-color-dir 'nerd-icons-icon-for-dir)
    (advice-add 'nerd-icons-icon-for-dir
                :filter-return #'cj/--nerd-icons-color-dir))
  (cj/nerd-icons-apply-tint))

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
