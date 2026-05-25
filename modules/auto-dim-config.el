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

(require 'cl-lib)
(require 'color)

(declare-function auto-dim-other-buffers-mode "auto-dim-other-buffers")
(declare-function adob--update "auto-dim-other-buffers")
(declare-function vterm--get-color "vterm")
(declare-function vterm--invalidate "vterm")
(declare-function vterm--set-size "vterm")
(declare-function vterm--get-margin-width "vterm")
(defvar vterm-min-window-width)
(defvar vterm--term)

(defvar cj/auto-dim--last-selected-window nil
  "Most recent selected window seen by `cj/auto-dim--refresh-vterm-on-command'.")

(defvar cj/auto-dim--vterm-refresh-timer nil
  "Timer used to defer vterm redraws until after auto-dim updates.")

(defcustom cj/auto-dim-vterm-foreground-blend 0.45
  "Blend amount for dimmed vterm foreground colors.

0 keeps the original vterm color; 1 uses the
`auto-dim-other-buffers' foreground color."
  :type 'number
  :group 'auto-dim-other-buffers)

(defcustom cj/auto-dim-vterm-background-blend 0.7
  "Blend amount for dimmed vterm background colors.

0 keeps the original vterm color; 1 uses the
`auto-dim-other-buffers' background color."
  :type 'number
  :group 'auto-dim-other-buffers)

(defun cj/auto-dim--vterm-buffer-dimmed-p ()
  "Return non-nil when the current vterm buffer should render dimmed.

Vterm resolves terminal colors to concrete color strings while redrawing the
buffer, so this integration is buffer-level.  If the same vterm buffer is shown
in multiple windows and any one of those windows is selected/undimmed, keep the
buffer bright."
  (and (eq major-mode 'vterm-mode)
       (let ((windows (get-buffer-window-list (current-buffer) nil 'visible)))
         (and windows
              (not (catch 'undimmed
                     (dolist (window windows)
                       (unless (window-parameter window 'adob--dim)
                         (throw 'undimmed t)))))))))

(defun cj/auto-dim--face-color (face attribute fallback-face)
  "Return FACE ATTRIBUTE, falling back to FALLBACK-FACE."
  (let ((color (face-attribute face attribute nil 'default)))
    (if (or (null color) (eq color 'unspecified))
        (face-attribute fallback-face attribute nil 'default)
      color)))

(defun cj/auto-dim--color-rgb (color)
  "Return COLOR as a list of RGB floats, or nil if COLOR is unknown."
  (cond
   ((and (stringp color)
         (string-match
          "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'"
          color))
    (mapcar (lambda (index)
              (/ (string-to-number (match-string index color) 16) 255.0))
            '(1 2 3)))
   ((and (stringp color)
         (string-match
          "\\`#\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\'"
          color))
    (mapcar (lambda (index)
              (/ (* 17 (string-to-number (match-string index color) 16)) 255.0))
            '(1 2 3)))
   (t
    (ignore-errors
      (mapcar (lambda (component) (/ component 65535.0))
              (color-values color))))))

(defun cj/auto-dim--blend-color (color target amount)
  "Blend COLOR toward TARGET by AMOUNT and return a hex color string."
  (if-let* ((rgb (cj/auto-dim--color-rgb color))
            (target-rgb (cj/auto-dim--color-rgb target)))
      (apply #'color-rgb-to-hex
             (append
              (cl-mapcar
               (lambda (source dest)
                 (+ (* source (- 1 amount)) (* dest amount)))
               rgb target-rgb)
              '(2)))
    color))

(defun cj/auto-dim--vterm-dim-color (color foreground-p)
  "Return dimmed vterm COLOR.

When FOREGROUND-P is non-nil, blend toward the dimmed foreground face; otherwise
blend toward the dimmed background face."
  (let* ((attribute (if foreground-p :foreground :background))
         (target (cj/auto-dim--face-color 'auto-dim-other-buffers attribute 'default))
         (amount (if foreground-p
                     cj/auto-dim-vterm-foreground-blend
                   cj/auto-dim-vterm-background-blend)))
    (cj/auto-dim--blend-color color target amount)))

(defun cj/auto-dim--vterm-get-color (orig-fun index &rest args)
  "Advise vterm color lookup ORIG-FUN for dimmed windows.

INDEX and ARGS are passed through to `vterm--get-color'."
  (let ((color (apply orig-fun index args)))
    (if (and color (cj/auto-dim--vterm-buffer-dimmed-p))
        (cj/auto-dim--vterm-dim-color color (memq :foreground args))
      color)))

(defun cj/auto-dim--refresh-vterm-windows (&optional frame)
  "Refresh visible vterm buffers in FRAME after dim state changes."
  (when (or (fboundp 'vterm--set-size) (fboundp 'vterm--invalidate))
    (dolist (window (window-list frame 'no-minibuf))
      (with-current-buffer (window-buffer window)
        (when (eq major-mode 'vterm-mode)
          (let ((inhibit-read-only t))
            (if (and (bound-and-true-p vterm--term)
                     (window-live-p window)
                     (fboundp 'vterm--get-margin-width))
                (let* ((height (max 2 (window-body-height window)))
                       (min-width (if (boundp 'vterm-min-window-width)
                                      vterm-min-window-width
                                    80))
                       (width (max min-width
                                   (- (window-body-width window)
                                      (vterm--get-margin-width)))))
                  ;; `vterm--redraw' only repaints rows libvterm marked dirty.
                  ;; A resize marks the whole terminal grid dirty, so briefly
                  ;; nudge height and restore it to force a full repaint after
                  ;; dim-state changes.
                  (vterm--set-size vterm--term (1+ height) width)
                  (vterm--set-size vterm--term height width))
              (when (fboundp 'vterm--invalidate)
                (vterm--invalidate)))))))))

(defun cj/auto-dim--refresh-vterm-after-auto-dim (&optional frame)
  "Update auto-dim state, then refresh visible vterm buffers in FRAME."
  (setq cj/auto-dim--vterm-refresh-timer nil)
  (when (fboundp 'adob--update)
    (adob--update))
  (cj/auto-dim--refresh-vterm-windows frame))

(defun cj/auto-dim--schedule-vterm-refresh (&optional frame)
  "Schedule a deferred vterm refresh for FRAME.

The delay lets selection-changing commands finish before we recompute
auto-dim state and invalidate vterm."
  (when cj/auto-dim--vterm-refresh-timer
    (cancel-timer cj/auto-dim--vterm-refresh-timer))
  (setq cj/auto-dim--vterm-refresh-timer
        (run-with-timer 0 nil #'cj/auto-dim--refresh-vterm-after-auto-dim frame)))

(defun cj/auto-dim--refresh-vterm-on-command ()
  "Refresh visible vterm buffers when selected window changes.

`window-selection-change-functions' does not catch every selection path used by
windmove/Shift-arrow focus changes in this config, so this post-command hook is
the fallback that makes vterm repaint after auto-dim changes window state."
  (let ((window (selected-window)))
    (unless (eq window cj/auto-dim--last-selected-window)
      (setq cj/auto-dim--last-selected-window window)
      (cj/auto-dim--schedule-vterm-refresh))))

(defun cj/auto-dim--after-select-window (&rest _)
  "Schedule vterm refresh after `select-window'."
  (setq cj/auto-dim--last-selected-window (selected-window))
  (cj/auto-dim--schedule-vterm-refresh))

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

(with-eval-after-load 'vterm
  (unless (advice-member-p #'cj/auto-dim--vterm-get-color #'vterm--get-color)
    (advice-add #'vterm--get-color :around #'cj/auto-dim--vterm-get-color))
  (unless (advice-member-p #'cj/auto-dim--after-select-window #'select-window)
    (advice-add #'select-window :after #'cj/auto-dim--after-select-window))
  (add-hook 'window-selection-change-functions
            #'cj/auto-dim--schedule-vterm-refresh)
  (add-hook 'post-command-hook #'cj/auto-dim--refresh-vterm-on-command))

(provide 'auto-dim-config)
;;; auto-dim-config.el ends here
