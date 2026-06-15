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
;;
;; Terminal buffers (ghostel) do not participate in window dimming: ghostel
;; bakes its color palette into the native module per-terminal, not per-window,
;; so there is no per-window color hook to dim through (the vterm engine had
;; one via `vterm--get-color', which this module used to advise).  See the
;; terminal-migration follow-up task in todo.org for revisiting this.

;;; Code:

(declare-function auto-dim-other-buffers-mode "auto-dim-other-buffers")

(defun cj/auto-dim--never-dim-dashboard-p (buffer)
  "Return non-nil when BUFFER is the dashboard, so it stays lit.
The dashboard banner is a transparent PNG.  On this non-pgtk build Emacs
composites image alpha against one background color at render time and
caches the flat pixmap; it can't re-blend when dimming remaps the
background to the near-black `auto-dim-other-buffers' face, so the
transparent edges show a baked-in rectangle in a dimmed dashboard.  Live
alpha would need a pgtk build, ruled out by its fractional-scaling input
lag.  Exempting just this one short-lived buffer keeps the fix local --
every other approach changes dimming for all buffers.  The cost is no
focus cue on a split-displayed dashboard, accepted as a fair trade."
  (equal (buffer-name buffer) "*dashboard*"))

(use-package auto-dim-other-buffers
  :load-path "~/code/auto-dim-other-buffers.el"
  :ensure nil
  ;; :vc (:url "git@cjennings.net:auto-dim-other-buffers.git" :rev :newest)
  :custom
  ;; Dim only non-selected windows within Emacs, not the whole frame when
  ;; Emacs loses focus -- on Hyprland focus moves to other apps constantly,
  ;; and the ai-term agents live in their own windows.
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
		  ;; and wired in modules/org-faces-config.el.
		  (org-faces-todo                   . (org-faces-todo-dim          . nil))
		  (org-faces-project                . (org-faces-project-dim       . nil))
		  (org-faces-doing                  . (org-faces-doing-dim         . nil))
		  (org-faces-waiting                . (org-faces-waiting-dim       . nil))
		  (org-faces-verify                 . (org-faces-verify-dim        . nil))
		  (org-faces-stalled                . (org-faces-stalled-dim       . nil))
		  (org-faces-delegated              . (org-faces-delegated-dim     . nil))
		  (org-faces-failed                 . (org-faces-failed-dim        . nil))
		  (org-faces-done                   . (org-faces-done-dim          . nil))
		  (org-faces-cancelled              . (org-faces-cancelled-dim     . nil))
		  (org-faces-priority-a             . (org-faces-priority-a-dim    . nil))
		  (org-faces-priority-b             . (org-faces-priority-b-dim    . nil))
		  (org-faces-priority-c             . (org-faces-priority-c-dim    . nil))
		  (org-faces-priority-d             . (org-faces-priority-d-dim    . nil))))
  (add-hook 'auto-dim-other-buffers-never-dim-buffer-functions
            #'cj/auto-dim--never-dim-dashboard-p)
  (auto-dim-other-buffers-mode 1))

(provide 'auto-dim-config)
;;; auto-dim-config.el ends here
