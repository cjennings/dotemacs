;;; modeline-config --- Modeline Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/S.
;; Load shape: eager.
;; Eager reason: the modeline is visible in the first frame.
;; Top-level side effects: two add-hook (VC cache lifecycle).
;; Runtime requires: user-constants.
;; Direct test load: yes.
;;
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
;; Use 'window instead of 'right-margin so centered text modes (nov-mode, etc.)
;; don't push modeline elements inward
(setq mode-line-right-align-edge 'window)

;; String truncation length for narrow windows
(defcustom cj/modeline-string-truncate-length 12
  "String length after which truncation happens in narrow windows."
  :type 'natnum
  :group 'modeline)

(defcustom cj/modeline-vc-cache-ttl 5
  "Seconds to reuse cached VC branch and state in the modeline."
  :type 'number
  :group 'modeline)

(defcustom cj/modeline-vc-show-remote nil
  "When non-nil, show VC branch and state for remote files."
  :type 'boolean
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
  '(:eval (let* ((color (cj/buffer-status-color (cj/buffer-status-state)))
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
  "Buffer name colored by modification and read-only status.
White = unmodified, Green = modified, Red = read-only, Gold = overwrite.
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

(defvar-local cj/modeline-vc-cache-key nil
  "Cache key for the current buffer's modeline VC data.")

(defvar-local cj/modeline-vc-cache-time nil
  "Timestamp for the current buffer's modeline VC cache.")

(defvar-local cj/modeline-vc-cache-value nil
  "Cached modeline VC plist for the current buffer.")

(defvar-local cj/modeline-vc-cache-set-p nil
  "Non-nil when the current buffer's modeline VC cache has a value.")

(defun cj/modeline-vc-file ()
  "Return the file or directory to inspect for VC modeline data."
  (or buffer-file-name default-directory))

(defun cj/modeline-vc-cache-clear ()
  "Clear cached VC modeline data for the current buffer."
  (setq cj/modeline-vc-cache-key nil
        cj/modeline-vc-cache-time nil
        cj/modeline-vc-cache-value nil
        cj/modeline-vc-cache-set-p nil))

(defun cj/modeline-vc-cache-key (file)
  "Return the cache key for FILE: the file path and `cj/modeline-vc-show-remote'.
`file-truename' is deliberately omitted -- the mode-line rebuilds this key on
every render to check cache validity, so a stat here would run per redisplay.
A symlink whose target moves to a different VC tree is picked up at the next
TTL refresh, when `vc-backend' resolves the link fresh."
  (list file cj/modeline-vc-show-remote))

(defun cj/modeline-vc-cache-valid-p (key now)
  "Return non-nil when cached VC data is valid for KEY at NOW."
  (and cj/modeline-vc-cache-set-p
       cj/modeline-vc-cache-time
       (equal key cj/modeline-vc-cache-key)
       (<= (- now cj/modeline-vc-cache-time) cj/modeline-vc-cache-ttl)))

(defun cj/modeline-vc-fetch (file)
  "Fetch modeline VC data for FILE.
Return a plist with `:branch' and `:state', or nil when FILE has no VC data.
Uses `vc-git--symbolic-ref' for branch names when available (it returns the
symbolic ref like \"main\" instead of a SHA when HEAD is on a branch), but
falls back to `vc-working-revision' if the internal accessor is missing --
the symbol is internal and can be renamed or removed between Emacs versions.

The whole VC probe is wrapped in `condition-case' returning nil.  These are
synchronous git calls that, on TTL expiry, run while the mode-line is built;
on a slow or unmounted filesystem a signal here would land in redisplay and
break it.  Caching nil degrades to \"no VC info\" instead."
  (condition-case nil
      (unless (and (file-remote-p file) (not cj/modeline-vc-show-remote))
        (when-let* ((backend (vc-backend file))
                    (branch (vc-working-revision file backend)))
          (when (eq backend 'Git)
            (unless (fboundp 'vc-git--symbolic-ref)
              (require 'vc-git nil 'noerror))
            (when (fboundp 'vc-git--symbolic-ref)
              (when-let* ((symbolic (ignore-errors (vc-git--symbolic-ref file))))
                (setq branch symbolic))))
          (list :branch branch
                :state (vc-state file backend))))
    (error nil)))

(defun cj/modeline-vc-info ()
  "Return cached modeline VC data for the current buffer."
  (when-let* ((file (cj/modeline-vc-file)))
    (unless (and (file-remote-p file) (not cj/modeline-vc-show-remote))
      (let* ((now (float-time))
             (key (cj/modeline-vc-cache-key file)))
        (if (cj/modeline-vc-cache-valid-p key now)
            cj/modeline-vc-cache-value
          (setq cj/modeline-vc-cache-key key
                cj/modeline-vc-cache-time now
                cj/modeline-vc-cache-value (cj/modeline-vc-fetch file)
                cj/modeline-vc-cache-set-p t)
          cj/modeline-vc-cache-value)))))

(defun cj/modeline-vc-render (info)
  "Render modeline VC INFO plist."
  (when-let* ((branch (plist-get info :branch)))
    (let* ((state (plist-get info :state))
           (face (alist-get state cj/modeline-vc-faces 'vc-up-to-date-state))
           (truncated-branch (cj/modeline-string-cut-middle branch)))
      (concat
       (propertize (char-to-string #xE0A0) 'face 'shadow)
       " "
       (propertize truncated-branch
                   'face face
                   'mouse-face 'mode-line-highlight
                   'help-echo (format "Branch: %s\nState: %s\nmouse-1: vc-diff\nmouse-3: vc-root-diff" branch state)
                   'local-map (let ((map (make-sparse-keymap)))
                                (define-key map [mode-line mouse-1] 'vc-diff)
                                (define-key map [mode-line mouse-3] 'vc-root-diff)
                                map))))))

(defvar-local cj/modeline-vc-branch
  '(:eval (when (mode-line-window-selected-p)  ; Only show in active window
            (cj/modeline-vc-render (cj/modeline-vc-info))))
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

(add-hook 'after-save-hook #'cj/modeline-vc-cache-clear)
(add-hook 'after-revert-hook #'cj/modeline-vc-cache-clear)

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
    (:eval (when (fboundp 'cj/recording-modeline-indicator)
             (cj/recording-modeline-indicator)))
    ;; Flycheck status: prefix + counts (or success indicator).  Gated
    ;; to the active window, and to buffers where flycheck has loaded
    ;; and turned on, so the call is safe even before flycheck loads.
    (:eval (when (and (mode-line-window-selected-p)
                      (bound-and-true-p flycheck-mode))
             (flycheck-mode-line-status-text)))
    "  "
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
