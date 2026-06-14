;;; ui-config --- User Interface Preferences -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/S.
;; Load shape: eager.
;; Eager reason: UI preferences that should be visible in the first frame.
;; Top-level side effects: UI defaults, a post-command hook, and
;;   display-buffer-alist entries.
;; Runtime requires: user-constants.
;; Direct test load: yes.
;;
;; This file centralizes user interface preferences, including:

;; • Frame and window behavior
;;   – Start all frames maximized
;;   – Disable file‐ and dialog‐boxes
;;   – Pixel scroll precision
;;   – Show column numbers in the mode-line

;; • Transparency controls
;;   – Customizable variables 'cj/enable-transparency' and 'cj/transparency-level'
;;   – Interactive 'cj/toggle-transparency' command

;; • Cursor appearance
;;   – Cursor color changes on the buffer's write and insertion state
;;     (i.e., read-only, overwrite, normal)
;;   – Option to customize cursor shape with 'cj/set-cursor-type'

;; • Icons
;;   – Load and enable 'nerd-icons' for UI glyphs

;; Customize the transparency and cursor color options at the top of this file.

;;; Code:

;; -------------------------------- UI Constants -------------------------------

(defvar cj/enable-transparency nil
  "Non-nil means use `cj/transparency-level' for frame transparency.")

(defvar cj/transparency-level 84
  "Opacity level for Emacs frames when `cj/enable-transparency' is non-nil.
100 = fully opaque, 0 = fully transparent.")

;; Use buffer status colors from user-constants
(require 'user-constants)

;; ----------------------------- System UI Settings ----------------------------

(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; start the initial frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start every frame maximized
(setq pixel-scroll-precision-mode nil)						 ;; disabled for performance

(setq-default frame-inhibit-implied-resize t)				 ;; don't resize frames when setting ui-elements
(setq frame-title-format '("Emacs " emacs-version " : %b"))	 ;; the title is emacs with version and buffer name

(setq use-file-dialog nil)									 ;; no file dialog
(setq use-dialog-box nil)									 ;; no dialog boxes either
(line-number-mode 1)                                         ;; show line number in the modeline (cached)
(column-number-mode 1)										 ;; show column number in the modeline (cached)
(setq switch-to-buffer-obey-display-actions t)               ;; manual buffer switching obeys display action rules

;; -------------------------------- Transparency -------------------------------

(defun cj/apply-transparency ()
  "Apply `cj/transparency-level' to the selected frame and future frames.

When `cj/enable-transparency' is nil, reset alpha to fully opaque."
  (let ((alpha (if cj/enable-transparency
				   (cons cj/transparency-level cj/transparency-level)
				 '(100 . 100))))
	;; apply to current frame (skip if terminal frame)
	(when (display-graphic-p)
	  (condition-case err
		  (set-frame-parameter nil 'alpha alpha)
		(error (message "Failed to set transparency: %s" (error-message-string err)))))
	;; update default for new frames
	(setq default-frame-alist
		  (assq-delete-all 'alpha default-frame-alist))
	(add-to-list 'default-frame-alist `(alpha . ,alpha))))

;; apply once at startup
(cj/apply-transparency)

(defun cj/toggle-transparency ()
  "Toggle `cj/enable-transparency' and re-apply."
  (interactive)
  (setq cj/enable-transparency (not cj/enable-transparency))
  (cj/apply-transparency)
  (message "Transparency %s"
		   (if cj/enable-transparency "enabled" "disabled")))

;; ----------------------------------- Cursor ----------------------------------
;; Set the cursor color from the active theme's faces according to buffer state.
;; The state classifier and the state->face map live in user-constants.el
;; (cj/buffer-status-state / cj/buffer-status-faces, colored via the theme's
;; error / warning / success faces) and are shared with the modeline buffer-name
;; indicator, so the cursor and the modeline stay in sync.

(defvar cj/-cursor-last-color nil
  "Last color applied by `cj/set-cursor-color-according-to-mode'.")
(defvar cj/-cursor-last-buffer nil
  "Last buffer name where cursor color was applied.")

(defun cj/set-cursor-color-according-to-mode ()
  "Set the cursor color from the active theme according to buffer state.
The state and its theme face come from `cj/buffer-status-state' and
`cj/buffer-status-color' (shared with the modeline), so the color follows the
loaded theme.  Only updates real user buffers, not internal/temporary ones; a
no-op on non-graphical frames -- TTY/batch sessions have no cursor color to set."
  (when (display-graphic-p)
    ;; Only update cursor for real buffers (not internal ones like *temp*, *Echo Area*).
    (unless (string-prefix-p " " (buffer-name))  ; internal buffers start with a space
      (let ((color (cj/buffer-status-color (cj/buffer-status-state))))
        ;; Skip only when BOTH color and buffer are unchanged (so the color still
        ;; updates when the buffer state changes).
        (when (and color
                   (not (and (equal color cj/-cursor-last-color)
                             (equal (buffer-name) cj/-cursor-last-buffer))))
          (set-cursor-color color)
          (setq cj/-cursor-last-color color
                cj/-cursor-last-buffer (buffer-name)))))))

;; Use post-command-hook to update cursor color after every command
;; This ensures cursor color always matches the current buffer's state.
;; The hook only registers under a graphical session so batch / TTY runs
;; don't pay per-command overhead for a no-op.
(when (display-graphic-p)
  (add-hook 'post-command-hook #'cj/set-cursor-color-according-to-mode))
;; Daemon mode: the first frame may be created after this module loads.
;; Re-attempt the hook install once a GUI frame appears.
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (and (display-graphic-p)
                       (not (memq #'cj/set-cursor-color-according-to-mode
                                  post-command-hook)))
              (add-hook 'post-command-hook
                        #'cj/set-cursor-color-according-to-mode))))

;; Don’t show a cursor in non-selected windows:
(setq cursor-in-non-selected-windows nil)

;; Initialize to box cursor (or any type you prefer)
(defun cj/set-cursor-type (new-cursor-type)
  "Set the cursor type of the selected frame to NEW-CURSOR-TYPE."
  (interactive
   (list (intern (completing-read
				  "Cursor type: "
				  (mapcar #'list '("box" "hollow" "bar" "hbar" nil))))))
  (modify-frame-parameters nil `((cursor-type . ,new-cursor-type))))

(cj/set-cursor-type 'box)

;; Keep the cursor a solid block -- no blinking (including the initial blink
;; burst when entering read-only buffers like EPUBs or vterm).
(blink-cursor-mode -1)

;; --------------------------------- Nerd Icons --------------------------------
;; use icons from nerd fonts in the Emacs UI

(use-package nerd-icons
  :defer t)

(provide 'ui-config)
;;; ui-config.el ends here
