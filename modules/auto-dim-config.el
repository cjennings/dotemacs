;;; auto-dim-config.el --- Dim non-selected windows -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/O.
;; Load shape: eager.
;; Eager reason: global UI minor mode; the dimming should be visible in the
;;   first frame.
;; Top-level side effects: enables auto-dim-other-buffers-mode and edits
;;   auto-dim-other-buffers-affected-faces.
;; Runtime requires: none (loads the auto-dim-other-buffers fork via use-package).
;; Direct test load: conditional (needs the ~/code fork on the load-path).
;;
;; Dims windows that do not have focus so the selected window stands out,
;; using a local fork of auto-dim-other-buffers (the fork adds a focus-change
;; debounce).  The dimmed faces (auto-dim-other-buffers and
;; auto-dim-other-buffers-hide) live in the active theme
;; (themes/dupre-faces.el) so they track theme switches.

;;; Code:

(use-package auto-dim-other-buffers
  :load-path "~/code/auto-dim-other-buffers.el"
  :ensure nil
  ;; :vc (:url "git@cjennings.net:auto-dim-other-buffers.git" :rev :newest)
  :custom
  ;; Dim only non-selected windows within Emacs, not the whole frame when
  ;; Emacs loses focus -- on Hyprland focus moves to other apps constantly,
  ;; and the ai-vterm agents live in their own windows.
  (auto-dim-other-buffers-dim-on-focus-out nil)
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer t)
  :config
  ;; Remap these faces to auto-dim-other-buffers (pure-black background +
  ;; faded gray foreground, defined in the theme) in non-selected windows.
  ;; The font-lock faces are included so code text fades to "disabled"
  ;; rather than staying lit -- remapping default alone would leave
  ;; syntax-highlighted text at full colour.  Fringe is left out because
  ;; dimming it forces a full-frame refresh that flickers on this non-pgtk
  ;; build; org-hide uses the -hide face so hidden text stays hidden.
  (setq auto-dim-other-buffers-affected-faces
		'((default                          . (auto-dim-other-buffers      . nil))
		  (org-block                        . (auto-dim-other-buffers      . nil))
		  (org-hide                         . (auto-dim-other-buffers-hide . nil))
		  (font-lock-keyword-face           . (auto-dim-other-buffers      . nil))
		  (font-lock-string-face            . (auto-dim-other-buffers      . nil))
		  (font-lock-comment-face           . (auto-dim-other-buffers      . nil))
		  (font-lock-comment-delimiter-face . (auto-dim-other-buffers      . nil))
		  (font-lock-doc-face               . (auto-dim-other-buffers      . nil))
		  (font-lock-function-name-face     . (auto-dim-other-buffers      . nil))
		  (font-lock-variable-name-face     . (auto-dim-other-buffers      . nil))
		  (font-lock-type-face              . (auto-dim-other-buffers      . nil))
		  (font-lock-constant-face          . (auto-dim-other-buffers      . nil))
		  (font-lock-builtin-face           . (auto-dim-other-buffers      . nil))
		  (font-lock-preprocessor-face      . (auto-dim-other-buffers      . nil))
		  (font-lock-warning-face           . (auto-dim-other-buffers      . nil))
		  ;; Org TODO-keyword + priority faces dim to their own -dim variant
		  ;; (a darker shade of the same colour) rather than the flat gray, so
		  ;; a dimmed window's keywords stay recognizable.  Faces are defined
		  ;; in themes/dupre-faces.el and wired in modules/org-config.el.
		  (dupre-org-todo                   . (dupre-org-todo-dim          . nil))
		  (dupre-org-project                . (dupre-org-project-dim       . nil))
		  (dupre-org-doing                  . (dupre-org-doing-dim         . nil))
		  (dupre-org-waiting                . (dupre-org-waiting-dim       . nil))
		  (dupre-org-verify                 . (dupre-org-verify-dim        . nil))
		  (dupre-org-stalled                . (dupre-org-stalled-dim       . nil))
		  (dupre-org-failed                 . (dupre-org-failed-dim        . nil))
		  (dupre-org-done                   . (dupre-org-done-dim          . nil))
		  (dupre-org-priority-a             . (dupre-org-priority-a-dim    . nil))
		  (dupre-org-priority-b             . (dupre-org-priority-b-dim    . nil))
		  (dupre-org-priority-c             . (dupre-org-priority-c-dim    . nil))
		  (dupre-org-priority-d             . (dupre-org-priority-d-dim    . nil))))
  (auto-dim-other-buffers-mode 1))

(provide 'auto-dim-config)
;;; auto-dim-config.el ends here
