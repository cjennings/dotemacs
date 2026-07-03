;;; modeline-config.el --- Modeline Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/S.
;; Load shape: eager.
;; Eager reason: the modeline is visible in the first frame.
;; Top-level side effects: two add-hook (VC cache lifecycle).
;; Runtime requires: none (nerd-icons and flycheck are used opportunistically
;;   behind fboundp guards, so the modeline renders with plain-text fallbacks
;;   when either is absent).
;; Direct test load: yes.
;;
;; Simple, minimal modeline built on Emacs 30's own right-alignment.
;; Segments are pure helpers wired in with thin :eval forms.
;;
;; Layout principle: the LEFT side is Emacs information -- everything
;; about this buffer and this Emacs (identity, state, position, VC,
;; process).  The RIGHT side is a systray for package indicators
;; (recording, flycheck counts, the misc-info icons).
;;
;; Left side, in order:
;; - Padding space (optional taller bar via `cj/modeline-height-factor')
;; - Major-mode icon (nerd-icons; falls back to the mode name in
;;   terminal frames or when nerd-icons is absent)
;; - eat terminal state: input-mode + process icons with hover text
;;   (replaces eat's own [semi-char]:run mode-line-process)
;; - Modified dot / read-only lock
;; - Buffer name (click to cycle buffers)
;; - Remote @host tag for TRAMP buffers
;; - Narrow tag when the buffer is narrowed (click to widen)
;; - VC branch colored by state (click for diffs)
;; - Line/column and percentage; selection info while the region is active
;; - MACRO tag while a keyboard macro is recording
;; - Process state for non-eat buffers (comint/compilation via
;;   `mode-line-process'; eat buffers get theirs cleared in eat-config)
;;
;; Right side (the systray):
;; - Recording indicator (video-audio-recording capture)
;; - Flycheck error/warning counts (click to list errors)
;; - Misc info (chime notifications, weather, etc.)
;;
;; Glyphs are nerd-icons private-use codepoints or plain unicode shapes
;; (U+25CF dot), never emoji codepoints — emojify rewrites those.
;;
;; `cj/modeline-reset' repairs a buffer whose mode-line-format was
;; hijacked buffer-locally (two-column mode, ediff, calc).

;;; Code:

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

(defcustom cj/modeline-height-factor 1.15
  "Height multiplier for the modeline's padding space.
Values above 1.0 make the modeline slightly taller than the text it
holds.  1.0 (or nil) renders a plain space with no height change."
  :type '(choice (const :tag "No extra height" nil) number)
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

(defun cj/--modeline-click-map (mouse-1 &optional mouse-3)
  "Return a mode-line `local-map' binding mouse clicks to commands.
\[mode-line mouse-1] runs MOUSE-1; when MOUSE-3 is non-nil, [mode-line mouse-3]
runs it too.  Shared builder for the clickable modeline segments."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] mouse-1)
    (when mouse-3
      (define-key map [mode-line mouse-3] mouse-3))
    map))

(defun cj/modeline-reset ()
  "Restore the default modeline in the current buffer.
Some packages (two-column mode, ediff, calc) replace `mode-line-format'
buffer-locally with their own layout and can leave it behind.  This
kills the buffer-local value so the default format returns."
  (interactive)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update)
  (message "Modeline restored to default in %s" (buffer-name)))

;; -------------------------- Modeline Segments --------------------------------

(defun cj/--modeline-padding ()
  "Return the leading modeline space, taller per `cj/modeline-height-factor'.
An absolute :height face on a single space pads the whole modeline
vertically.  The height is anchored to the frame's default face rather
than the current buffer's, so a buffer that remaps `default' larger —
nov-mode's reading view, `text-scale-mode' — no longer inflates the bar.
A `display (height FACTOR)' property would scale with the buffer's default
face and blow the modeline up in those buffers."
  (let ((base (face-attribute 'default :height nil t)))
    (if (and cj/modeline-height-factor
             (/= cj/modeline-height-factor 1.0)
             (integerp base))
        (propertize " " 'face
                    (list :height (round (* cj/modeline-height-factor base))))
      " ")))

(defvar-local cj/--modeline-mode-icon-cache nil
  "Cons of (MAJOR-MODE . GRAPHIC-P) paired with the rendered mode segment.
Avoids a nerd-icons lookup on every redisplay.")

(defun cj/--modeline-mode-icon-compute ()
  "Build the major-mode segment: a nerd-icons glyph, or the mode name.
Graphical frames with nerd-icons available get the mode's colored icon;
terminal frames and icon-less setups get the plain mode name.  Either
way the segment carries the full mode name in its help-echo and clicks
through to `describe-mode'."
  (let* ((name (format-mode-line mode-name))
         (icon (and (display-graphic-p)
                    (fboundp 'nerd-icons-icon-for-mode)
                    (let ((i (ignore-errors (nerd-icons-icon-for-mode major-mode))))
                      (and (stringp i) i))))
         (help (if-let* ((parent (get major-mode 'derived-mode-parent)))
                   (format "Major mode: %s (%s)\nDerived from: %s\nmouse-1: describe-mode"
                           name major-mode parent)
                 (format "Major mode: %s (%s)\nmouse-1: describe-mode"
                         name major-mode))))
    (propertize (or icon name)
                'mouse-face 'mode-line-highlight
                'help-echo help
                'local-map (cj/--modeline-click-map 'describe-mode))))

(defun cj/--modeline-mode-icon ()
  "Return the cached major-mode segment for the current buffer."
  (let ((key (cons major-mode (display-graphic-p))))
    (unless (equal (car-safe cj/--modeline-mode-icon-cache) key)
      (setq cj/--modeline-mode-icon-cache
            (cons key (cj/--modeline-mode-icon-compute))))
    (cdr cj/--modeline-mode-icon-cache)))

(defun cj/--modeline-buffer-status ()
  "Return the modified/read-only indicator, or nil when neither applies.
Read-only shows a lock and wins over modified.  The modified dot shows
only for file-visiting buffers -- special buffers are perpetually
modified and would be noise.  Clean file buffers show nothing."
  (cond
   (buffer-read-only
    (concat (or (and (fboundp 'nerd-icons-faicon)
                     (ignore-errors (nerd-icons-faicon "nf-fa-lock" :face 'shadow)))
                (propertize "RO" 'face 'shadow))
            " "))
   ((and (buffer-modified-p) buffer-file-name)
    (concat (propertize "●" 'face 'warning
                        'help-echo "Buffer has unsaved changes")
            " "))))

(defvar-local cj/modeline-buffer-name
  '(:eval (let* ((name (buffer-name))
                 (truncated-name (cj/modeline-string-cut-middle name)))
            (propertize truncated-name
                        'mouse-face 'mode-line-highlight
                        'help-echo (concat
                                    name "\n"
                                    (or (buffer-file-name)
                                        (format "No file. Directory: %s" default-directory)))
                        'local-map (cj/--modeline-click-map 'previous-buffer 'next-buffer))))
  "Buffer name in the mode line.
Truncates in narrow windows.  Click to switch buffers.")

(defun cj/--modeline-remote-host ()
  "Return an @host tag when the buffer's directory is remote, else nil."
  (when-let* ((host (file-remote-p default-directory 'host)))
    (concat " "
            (propertize (concat "@" host)
                        'face 'warning
                        'help-echo (format "Remote: %s" default-directory)))))

(defun cj/--modeline-narrow-indicator ()
  "Return the Narrow tag when the buffer is narrowed, else nil.
Click to widen."
  (when (buffer-narrowed-p)
    (concat " "
            (propertize "Narrow"
                        'face 'warning
                        'mouse-face 'mode-line-highlight
                        'help-echo "Buffer is narrowed\nmouse-1: widen"
                        'local-map (cj/--modeline-click-map 'widen)))))

(defun cj/--modeline-position-info ()
  "Return position info: L:line C:col and percentage through the buffer.
While the region is active, return selection info instead (lines and
characters selected)."
  (if (use-region-p)
      (let* ((lines (count-lines (region-beginning) (region-end)))
             (chars (- (region-end) (region-beginning))))
        (format "%d line%s, %d char%s"
                lines (if (= lines 1) "" "s")
                chars (if (= chars 1) "" "s")))
    ;; Percent is computed from point rather than %p: it answers "how far
    ;; is point" instead of "where is the window", and it stays correct in
    ;; batch/undisplayed buffers.  %% survives the mode-line's %-construct
    ;; pass over :eval results as a literal percent sign.
    (format "L:%s C:%s %d%%%%"
            (format-mode-line "%l")
            (format-mode-line "%c")
            (floor (* 100.0 (- (point) (point-min)))
                   (max 1 (- (point-max) (point-min)))))))

(defun cj/--modeline-macro-indicator ()
  "Return the MACRO tag while a keyboard macro is recording, else nil."
  (when defining-kbd-macro
    (concat "  "
            (propertize "MACRO"
                        'face 'error
                        'help-echo "Recording keyboard macro\nF4 or C-x ) to stop"))))

;; ----------------------------- Eat State Segment ------------------------------

(defconst cj/--modeline-eat-mode-help
  '((semi-char . "most keys go to the terminal; Emacs keeps C-x, M-x, and the F-keys")
    (char . "raw: nearly every key goes to the terminal")
    (line . "line editing in Emacs; RET sends the line")
    (emacs . "all keys are Emacs; terminal output is read-only"))
  "Hover-text description per eat input mode.")

(defun cj/--modeline-eat-input-mode ()
  "Return the current eat input mode as a symbol."
  (cond ((bound-and-true-p eat--semi-char-mode) 'semi-char)
        ((bound-and-true-p eat--char-mode) 'char)
        ((bound-and-true-p eat--line-mode) 'line)
        (t 'emacs)))

(defun cj/--modeline-eat-switch-map (mode)
  "Return a mode-line keymap switching away from eat input MODE.
Mirrors eat's own mouse-1/2/3 assignments for each mode."
  (let ((map (make-sparse-keymap))
        (targets (pcase mode
                   ('semi-char '(eat-char-mode eat-line-mode eat-emacs-mode))
                   ('char '(eat-semi-char-mode eat-line-mode eat-emacs-mode))
                   ('line '(eat-semi-char-mode eat-emacs-mode eat-char-mode))
                   (_ '(eat-semi-char-mode eat-line-mode eat-char-mode)))))
    (define-key map [mode-line down-mouse-1] (nth 0 targets))
    (define-key map [mode-line down-mouse-2] (nth 1 targets))
    (define-key map [mode-line down-mouse-3] (nth 2 targets))
    map))

(defun cj/--modeline-eat-state ()
  "Input-mode + process icons for eat terminal buffers, or nil elsewhere.
Replaces eat's own [semi-char]:run `mode-line-process' text (cleared in
eat-config): a keyboard glyph colored quiet for semi-char and warning
for the other modes, then a running/exited indicator.  Hover text
explains each; the keyboard glyph clicks through eat's mode switches."
  (when (derived-mode-p 'eat-mode)
    (let* ((mode (cj/--modeline-eat-input-mode))
           (mode-face (if (eq mode 'semi-char) 'shadow 'warning))
           (icons-p (and (display-graphic-p) (fboundp 'nerd-icons-faicon)))
           (mode-glyph (or (and icons-p
                                (ignore-errors
                                  (nerd-icons-faicon "nf-fa-keyboard_o" :face mode-face)))
                           (propertize (upcase (substring (symbol-name mode) 0 1))
                                       'face mode-face)))
           (proc (get-buffer-process (current-buffer)))
           (live (and proc (process-live-p proc)))
           (proc-glyph (if live
                           (or (and icons-p
                                    (ignore-errors
                                      (nerd-icons-faicon "nf-fa-play" :face 'success)))
                               (propertize "run" 'face 'success))
                         (or (and icons-p
                                  (ignore-errors
                                    (nerd-icons-faicon "nf-fa-power_off" :face 'error)))
                             (propertize "exit" 'face 'error)))))
      (concat "  "
              (propertize mode-glyph
                          'mouse-face 'mode-line-highlight
                          'help-echo (format "Input mode: %s -- %s\nmouse-1/2/3: switch input modes"
                                             mode
                                             (alist-get mode cj/--modeline-eat-mode-help))
                          'local-map (cj/--modeline-eat-switch-map mode))
              " "
              (propertize proc-glyph
                          'help-echo (if live
                                         "Terminal process: running"
                                       "Terminal process exited"))))))

;; ------------------------------ Flycheck Segment ------------------------------

(defvar cj/--modeline-flycheck-glyphs nil
  "Cached (ERROR-GLYPH . WARNING-GLYPH) for the flycheck segment.
nerd-icons private-use glyphs (emojify never rewrites those).  Only a
successful lookup is cached, so a load before nerd-icons doesn't poison
the cache with nils.")

(defun cj/--modeline-flycheck-glyphs ()
  "Return the flycheck glyph pair, or nil when icons aren't usable.
Text fallbacks apply on non-graphic frames (PUA glyphs don't render in
a terminal) and whenever nerd-icons is absent."
  (when (display-graphic-p)
    (or cj/--modeline-flycheck-glyphs
        (when (fboundp 'nerd-icons-faicon)
          (let ((pair (cons (ignore-errors (nerd-icons-faicon "nf-fa-times_circle" :face 'error))
                            (ignore-errors (nerd-icons-faicon "nf-fa-warning" :face 'warning)))))
            (when (and (car pair) (cdr pair))
              (setq cj/--modeline-flycheck-glyphs pair)))))))

(defun cj/--modeline-flycheck-render (counts)
  "Render flycheck COUNTS alist ((error . N) (warning . M) ...) or nil.
Errors carry the error face, warnings the warning face; zero-count
severities are omitted; all-clean renders nothing.  The segment clicks
through to `flycheck-list-errors'."
  (let* ((errors (or (alist-get 'error counts) 0))
         (warnings (or (alist-get 'warning counts) 0))
         (glyphs (cj/--modeline-flycheck-glyphs))
         (parts nil))
    (when (> warnings 0)
      (push (concat (or (cdr glyphs) (propertize "W" 'face 'warning)) " "
                    (propertize (number-to-string warnings) 'face 'warning))
            parts))
    (when (> errors 0)
      (push (concat (or (car glyphs) (propertize "E" 'face 'error)) " "
                    (propertize (number-to-string errors) 'face 'error))
            parts))
    (when parts
      (propertize (mapconcat #'identity parts "  ")
                  'mouse-face 'mode-line-highlight
                  'help-echo "Flycheck\nmouse-1: list errors"
                  'local-map (cj/--modeline-click-map 'flycheck-list-errors)))))

(defun cj/--modeline-flycheck-status ()
  "Return the rendered flycheck counts for the current buffer, or nil."
  (when (and (fboundp 'flycheck-count-errors)
             (boundp 'flycheck-current-errors))
    (cj/--modeline-flycheck-render
     (flycheck-count-errors flycheck-current-errors))))

;; -------------------------------- VC Segment ----------------------------------

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

(defun cj/--modeline-vc-cache-key (file)
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
             (key (cj/--modeline-vc-cache-key file)))
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
                   'local-map (cj/--modeline-click-map 'vc-diff 'vc-root-diff))))))

(defvar-local cj/modeline-vc-branch
  '(:eval (when (mode-line-window-selected-p)  ; Only show in active window
            (cj/modeline-vc-render (cj/modeline-vc-info))))
  "Git branch with symbol and colored by VC state.
Shows only in active window.  Truncates in narrow windows.
Click to show diffs with `vc-diff' or `vc-root-diff'.")

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
    ;; LEFT SIDE -- Emacs information: identity, state, position, VC, process
    (:eval (cj/--modeline-padding))
    (:eval (cj/--modeline-mode-icon))
    (:eval (cj/--modeline-eat-state))
    "  "
    (:eval (cj/--modeline-buffer-status))
    cj/modeline-buffer-name
    (:eval (cj/--modeline-remote-host))
    (:eval (cj/--modeline-narrow-indicator))
    "  "
    cj/modeline-vc-branch
    "  "
    (:eval (cj/--modeline-position-info))
    (:eval (cj/--modeline-macro-indicator))
    " "
    mode-line-process
    ;; RIGHT SIDE -- the package systray (using Emacs 30 built-in right-align)
    ;; Order: leftmost to rightmost as they appear in the list
    mode-line-format-right-align
    (:eval (when (fboundp 'cj/recording-modeline-indicator)
             (cj/recording-modeline-indicator)))
    ;; Flycheck status: error/warning counts.  Gated to the active
    ;; window, and to buffers where flycheck has loaded and turned on,
    ;; so the call is safe even before flycheck loads.
    (:eval (when (and (mode-line-window-selected-p)
                      (bound-and-true-p flycheck-mode))
             (cj/--modeline-flycheck-status)))
    "  "
    cj/modeline-misc-info
    "  "))

;; Mark all segments as risky-local-variable (required for :eval forms)
(dolist (construct '(cj/modeline-buffer-name
                     cj/modeline-vc-branch
                     cj/modeline-misc-info))
  (put construct 'risky-local-variable t))

(provide 'modeline-config)
;;; modeline-config.el ends here
