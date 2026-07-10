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
;; auto-dim-other-buffers-hide) live in the active theme (the generated
;; theme-studio theme) so they track theme switches.
;;
;; EAT terminals render in real Emacs faces and use the `default' face for the
;; terminal background, so -- unlike the old ghostel/vterm engines, which baked
;; color per-terminal with no per-window hook -- they follow the per-window
;; dimmed background like any other buffer.
;;
;; One caveat, so nobody chases it through this alist: ANSI-coloured spans in a
;; terminal keep their colour when dimmed.  EAT attaches those as anonymous face
;; plists carrying a literal foreground, e.g. (:foreground "#AFD7FF" :inherit
;; (eat-term-font-0)), and `face-remap-add-relative' only reaches named faces.
;; There is no face name to add below.  Reaching them would need an overlay
;; (whose face outranks a text property), not a remap.  Background and uncoloured
;; text still dim, which is close enough; this is deliberate, not an oversight.

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
		  ;; The built-in link faces, distinct from org-link below.  They fontify
		  ;; links in help, info, and customize buffers.  Both carry :underline t,
		  ;; which survives the relative remap, so a dimmed link still reads as one.
		  (link                             . (auto-dim-other-buffers      . nil))
		  (link-visited                     . (auto-dim-other-buffers      . nil))
		  ;; Org structure faces flat-dim like font-lock rather than getting
		  ;; -dim variants: the active theme gives org-level-1..8 one shared
		  ;; foreground and no height or weight, so there is no level-by-colour
		  ;; signal to preserve.  The remap is relative, so org-link keeps its
		  ;; underline and the heading stars / org-indent keep conveying depth.
		  ;; That premise is theme-dependent -- a theme that colours heading
		  ;; levels distinctly would make the flat dim discard real signal, and
		  ;; those levels would then want -dim variants like the keywords below.
		  (org-level-1                      . (auto-dim-other-buffers      . nil))
		  (org-level-2                      . (auto-dim-other-buffers      . nil))
		  (org-level-3                      . (auto-dim-other-buffers      . nil))
		  (org-level-4                      . (auto-dim-other-buffers      . nil))
		  (org-level-5                      . (auto-dim-other-buffers      . nil))
		  (org-level-6                      . (auto-dim-other-buffers      . nil))
		  (org-level-7                      . (auto-dim-other-buffers      . nil))
		  (org-level-8                      . (auto-dim-other-buffers      . nil))
		  (org-link                         . (auto-dim-other-buffers      . nil))
		  (org-tag                          . (auto-dim-other-buffers      . nil))
		  ;; org-todo and org-priority are deliberately absent: they are keyword
		  ;; class, like the org-faces-* set below, and flat-dimming them would
		  ;; erase the status colour those -dim variants exist to preserve.
		  ;; Document header: #+TITLE:, #+AUTHOR:, #+ARCHIVE: and their values.
		  (org-document-title               . (auto-dim-other-buffers      . nil))
		  (org-document-info                . (auto-dim-other-buffers      . nil))
		  (org-document-info-keyword        . (auto-dim-other-buffers      . nil))
		  (org-meta-line                    . (auto-dim-other-buffers      . nil))
		  ;; Inline markup and source blocks.
		  (org-code                         . (auto-dim-other-buffers      . nil))
		  (org-verbatim                     . (auto-dim-other-buffers      . nil))
		  (org-block-begin-line             . (auto-dim-other-buffers      . nil))
		  (org-block-end-line               . (auto-dim-other-buffers      . nil))
		  ;; Drawers, properties, and planning lines.
		  (org-drawer                       . (auto-dim-other-buffers      . nil))
		  (org-special-keyword              . (auto-dim-other-buffers      . nil))
		  (org-property-value               . (auto-dim-other-buffers      . nil))
		  (org-date                         . (auto-dim-other-buffers      . nil))
		  ;; Tables and the fold indicator.
		  (org-table                        . (auto-dim-other-buffers      . nil))
		  (org-table-row                    . (auto-dim-other-buffers      . nil))
		  (org-ellipsis                     . (auto-dim-other-buffers      . nil))
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
