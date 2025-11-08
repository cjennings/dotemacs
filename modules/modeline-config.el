;;; modeline-config --- Modeline Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Simple, minimal modeline using only built-in Emacs functionality.
;; No external packages = no buffer issues, no native-comp errors.

;; Features:
;; - Buffer status (modified, read-only)
;; - Buffer name
;; - Major mode
;; - Version control status
;; - Line and column position
;; - Buffer percentage

;;; Code:

;; Use buffer status colors from user-constants
(require 'user-constants)

;; -------------------------- Modeline Configuration --------------------------

;; Use Emacs 30's built-in right-alignment
(setq mode-line-right-align-edge 'right-margin)

;; String truncation length for narrow windows
(defcustom cj/modeline-string-truncate-length 12
  "String length after which truncation happens in narrow windows."
  :type 'natnum
  :group 'modeline)

;; -------------------------- Helper Functions ---------------------------------

(defun cj/modeline-window-narrow-p ()
  "Return non-nil if window is narrow (less than 100 chars wide)."
  (< (window-total-width) 100))

(defun cj/modeline-string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (stringp str)
       (not (string-empty-p str))
       (cj/modeline-window-narrow-p)
       (> (length str) cj/modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun cj/modeline-string-cut-middle (str)
  "Truncate STR in the middle if appropriate, else return STR.
Example: `my-very-long-name.el' → `my-ver...me.el'"
  (if (cj/modeline-string-truncate-p str)
      (let ((half (floor cj/modeline-string-truncate-length 2)))
        (concat (substring str 0 half) "..." (substring str (- half))))
    str))

;; -------------------------- Modeline Segments --------------------------------

(defvar-local cj/modeline-buffer-name
  '(:eval (let* ((state (cond
                         (buffer-read-only 'read-only)
                         (overwrite-mode   'overwrite)
                         (t                'normal)))
                 (color (alist-get state cj/buffer-status-colors))
                 (name (buffer-name))
                 (truncated-name (cj/modeline-string-cut-middle name)))
            (propertize truncated-name
                        'face `(:foreground ,color)
                        'mouse-face 'mode-line-highlight
                        'help-echo (concat
                                    name "\n"
                                    (or (buffer-file-name)
                                        (format "No file. Directory: %s" default-directory)))
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line mouse-1] 'previous-buffer)
                                     (define-key map [mode-line mouse-3] 'next-buffer)
                                     map))))
  "Buffer name colored by read-only/read-write status.
Green = writeable, Red = read-only, Gold = overwrite.
Truncates in narrow windows.  Click to switch buffers.")

(defvar-local cj/modeline-position
  '("L:" (:eval (format-mode-line "%l")) " C:" (:eval (format-mode-line "%c")))
  "Line and column position as L:line C:col.
Uses built-in cached values for performance.")

(defvar cj/modeline-vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state to face mapping.")

(defvar-local cj/modeline-vc-branch
  '(:eval (when (mode-line-window-selected-p)  ; Only show in active window
            (when-let* ((file (or buffer-file-name default-directory))
                        (backend (vc-backend file)))
              (when-let* ((branch (vc-working-revision file backend)))
                ;; For Git, try to get symbolic branch name
                (when (eq backend 'Git)
                  (require 'vc-git)
                  (when-let* ((symbolic (vc-git--symbolic-ref file)))
                    (setq branch symbolic)))
                ;; Get VC state for face
                (let* ((state (vc-state file backend))
                       (face (alist-get state cj/modeline-vc-faces 'vc-up-to-date-state))
                       (truncated-branch (cj/modeline-string-cut-middle branch)))
                  (concat
                   (propertize (char-to-string #xE0A0) 'face 'shadow) ; Git branch symbol
                   " "
                   (propertize truncated-branch
                               'face face
                               'mouse-face 'mode-line-highlight
                               'help-echo (format "Branch: %s\nState: %s\nmouse-1: vc-diff\nmouse-3: vc-root-diff" branch state)
                               'local-map (let ((map (make-sparse-keymap)))
                                            (define-key map [mode-line mouse-1] 'vc-diff)
                                            (define-key map [mode-line mouse-3] 'vc-root-diff)
                                            map))))))))
  "Git branch with symbol and colored by VC state.
Shows only in active window.  Truncates in narrow windows.
Click to show diffs with `vc-diff' or `vc-root-diff'.")

(defvar-local cj/modeline-major-mode
  '(:eval (let ((mode-str (format-mode-line mode-name))  ; Convert to string
                (mode-sym major-mode))
            (propertize mode-str
                        'mouse-face 'mode-line-highlight
                        'help-echo (if-let* ((parent (get mode-sym 'derived-mode-parent)))
                                       (format "Major mode: %s\nDerived from: %s\nmouse-1: describe-mode" mode-sym parent)
                                     (format "Major mode: %s\nmouse-1: describe-mode" mode-sym))
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line mouse-1] 'describe-mode)
                                     map))))
  "Major mode name only (no minor modes).
Click to show help with `describe-mode'.")

(defvar-local cj/modeline-misc-info
  '(:eval (when (mode-line-window-selected-p)
            mode-line-misc-info))
  "Misc info (chime notifications, etc).
Shows only in active window.")

;; -------------------------- Modeline Assembly --------------------------------

(setq-default mode-line-format
  '("%e"  ; Error message if out of memory
    ;; LEFT SIDE
    " "
    cj/modeline-major-mode
    "  "
    cj/modeline-buffer-name
    "  "
    cj/modeline-position
    ;; RIGHT SIDE (using Emacs 30 built-in right-align)
    ;; Order: leftmost to rightmost as they appear in the list
    mode-line-format-right-align
    cj/modeline-vc-branch
    "  "
    cj/modeline-misc-info
    "  "))

;; Mark all segments as risky-local-variable (required for :eval forms)
(dolist (construct '(cj/modeline-buffer-name
                     cj/modeline-position
                     cj/modeline-vc-branch
                     cj/modeline-vc-faces
                     cj/modeline-major-mode
                     cj/modeline-misc-info))
  (put construct 'risky-local-variable t))

(provide 'modeline-config)
;;; modeline-config.el ends here
