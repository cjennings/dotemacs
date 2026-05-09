;;; eshell-vterm-config --- Settings for the Emacs Shell -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; ESHELL
;; - Eshell is useful as a REPL
;; - Redirect to the kill ring : ls > /dev/kill
;; - Redirect to the clioboard : ls > /dev/clip
;; - Redirect to a buffer      : ls > #<ls-output>
;; - Use elisp functions       : write your own "detox" command in elisp
;;                             : then use it in eshell
;; - cd to remote directories  : cd /sshx:c@cjennings.net:/home/cjennings
;;                             : and take all the elisp functionality remotely
;;                             : including Dired or Magit on a remote server

;; VTERM
;; At the moment, vterm behaves like a real terminal. For most keys, vterm will
;; just send them to the process that is currently running. So, C-a may be
;; beginning-of-the-line in a shell, or the prefix key in a screen session.

;; If you enter vterm-copy-mode C-c C-t or <pause>, the buffer will become a normal
;; Emacs buffer. You can then use your navigation keys, select rectangles, etc.
;; When you press RET, the region will be copied and you'll be back in a working
;; terminal session.

;; ANSI-TERM & TERM
;; I haven't yet found a need for term or ansi-term in my workflows, so I leave
;; them with their default configurations.

;;; Code:

(require 'system-utils)

;; ------------------------------ Eshell -----------------------------
;; the Emacs shell.

(use-package eshell
  :ensure nil ;; built-in
  :commands (eshell)
  :config
  (setq eshell-banner-message "")
  (setq eshell-scroll-to-bottom-on-input 'all)
  (setq eshell-error-if-no-glob t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)
  (setq eshell-prefer-lisp-functions nil)
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; no pagers required
  (setenv "PAGER" "cat")

  (setq eshell-prompt-function
		(lambda ()
		  (concat
		   (propertize (format-time-string "[%d-%m-%y %T]") 'face '(:foreground "gray"))
		   " "
		   (propertize (user-login-name) 'face '(:foreground "gray"))
		   " "
           (propertize (system-name) 'face '(:foreground "gray"))
		   ":"
		   (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "gray"))
		   "\n"
		   (propertize "%"  'face '(:foreground "white"))
		   " ")))

  (add-hook
   'eshell-mode-hook
   (lambda ()
     (setq pcomplete-cycle-completions nil)))
  (setq eshell-cmpl-cycle-completions nil)

  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (add-hook 'eshell-hist-mode-hook
            (lambda ()
              (keymap-set eshell-hist-mode-map "<up>" #'previous-line)
              (keymap-set eshell-hist-mode-map "<down>" #'next-line)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands '("lf" "ranger" "tail" "htop" "gotop" "mc" "ncdu" "top"))
              (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
              (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))

              ;; aliases
              (eshell/alias "e"      "find-file $1")
			  (eshell/alias "em"     "find-file $1")
			  (eshell/alias "emacs"  "find-file $1")
			  (eshell/alias "open"   "cj/xdg-open $1")
			  (eshell/alias "gocj"   "cd /sshx:cjennings@cjennings.net:/var/cjennings/")
			  (eshell/alias "gosb"   "cd /sshx:cjennings@wolf.usbx.me:/home/cjennings/")
			  (eshell/alias "gowolf" "cd /sshx:cjennings@wolf.usbx.me:/home/cjennings/")
			  (eshell/alias "v"      "eshell-exec-visual $*")
			  (eshell/alias "ff"     "find-file-other-window $1")
			  (eshell/alias "f"      "find-using-dired $1")
			  (eshell/alias "r"      "ranger")
			  (eshell/alias "ll"     "ls -laF"))))

(defun eshell/find-file-other-window (&rest files)
  "Open FILE(s) in other window from eshell."
  (if (= 1 (length files))
	  ;; Single file - just use it directly
	  (find-file-other-window (car files))
	;; Multiple files - open each in other window
	(dolist (file files)
	  (find-file-other-window file))))

(defun eshell/find-file (&rest files)
  "Open FILE(s) from eshell."
  (if (= 1 (length files))
	  ;; Single file
	  (find-file (car files))
	;; Multiple files
	(dolist (file files)
      (find-file file))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/find-using-dired (file-pattern)
  "Find a file matching FILE-PATTERN using `find-name-dired'."
  (let ((escaped-pattern (regexp-quote file-pattern)))
    (find-name-dired default-directory escaped-pattern)))

(defun cj/eshell-delete-window-on-exit ()
  "Close the eshell window when exiting."
  (when (not (one-window-p))
    (delete-window)))
(advice-add 'eshell-life-is-too-much :after 'cj/eshell-delete-window-on-exit)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 2)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("C-<f12>" . eshell-toggle))

(use-package xterm-color
  :after eshell
  :hook
  (eshell-before-prompt-hook . (lambda ()
                                 (setq xterm-color-preserve-properties t)))
  :config
  (setenv "TERM" "xterm-256color"))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-up
  :after eshell
  :config
  (defalias 'eshell/up 'eshell-up)
  (defalias 'eshell/up-peek 'eshell-up-peek))

;; Enhance history searching
(defun cj/eshell-history-search ()
  "Search eshell history with completion."
  (interactive)
  (insert
   (completing-read "Eshell history: "
					(delete-dups
					 (ring-elements eshell-history-ring)))))

(add-hook 'eshell-mode-hook
		  (lambda ()
			(keymap-set eshell-mode-map "C-r" #'cj/eshell-history-search)))

;; Better completion for eshell
(use-package pcmpl-args
  :after eshell)

;; Company mode integration for eshell
(use-package company-shell
  :after (eshell company)
  :config
  (add-to-list 'company-backends 'company-shell)
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (setq-local company-minimum-prefix-length 2)
			  (setq-local company-idle-delay 2)
			  (company-mode 1))))


;; ------------------------------ Vterm ------------------------------
;; faster and highly dependable, but not extensible

(use-package vterm
  :defer .5
  :commands (vterm vterm-other-window)
  :init
  (setq vterm-always-compile-module t)

  (defun cj/turn-off-chrome-for-vterm ()
	(hl-line-mode -1)
	(display-line-numbers-mode -1))

  (defun cj/vterm-launch-tmux ()
	"Automatically launch tmux in vterm if not already in a tmux session.

Skipped when `cj/--ai-vterm-suppress-tmux' is non-nil so the AI-vterm
flow can run its own project-named tmux session instead of a bare,
auto-named one.  `bound-and-true-p' keeps this safe whether or not
ai-vterm.el is loaded."
	(let ((proc (get-buffer-process (current-buffer))))
	  (when (and proc
				 (not (getenv "TMUX")) ; Check if not already in tmux
				 (not (bound-and-true-p cj/--ai-vterm-suppress-tmux)))
		(vterm-send-string "tmux\n"))))
  :hook
  ((vterm-mode . cj/turn-off-chrome-for-vterm)
   (vterm-mode . cj/vterm-launch-tmux))
  :bind
  (:map vterm-mode-map
		("<f8>"    . nil)
		("<f9>"    . nil)
		("<f10>"   . nil)
		("<f12>"   . nil)
		("C-y"     . vterm-yank)
		("C-p"     . vtermf-copy-mode)
		("<pause>" . vterm-copy-mode))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 100000)
  :config
  (setq vterm-timer-delay nil))

;; vterm-toggle is kept installed so `M-x vterm-toggle' still works,
;; but F12 below is bound to a custom toggle (`cj/vterm-toggle') that
;; excludes claude-prefixed buffers from its candidate set.
(use-package vterm-toggle
  :defer .5
  :config
  (setq vterm-toggle-fullscreen-p nil))

;; ----------------------- F12 toggle (custom) -----------------------
;;
;; Replacement for `vterm-toggle' on F12.  Two reasons to roll our own:
;;
;; 1. claude exclusion.  vterm-toggle picks the most-recently-selected
;;    vterm buffer as the toggle target.  When the user just used F9
;;    on a claude vterm, the most-recent vterm IS claude, so F12 ends
;;    up toggling claude -- which has its own F9 / C-F9 / M-F9 surface
;;    in `ai-vterm.el' and shouldn't be affected by F12.  The claude
;;    exclusion lives in the candidate filter (`cj/--vterm-toggle-buffer-p').
;;
;; 2. user-modified geometry.  vterm-toggle's display rule had a
;;    hard-coded `(window-height . 0.7)' that overrode any mouse-resize
;;    or M-S-t orientation flip on the next toggle.  This module mirrors
;;    the geometry-preservation pattern shipped in ai-vterm.el: capture
;;    direction + body size at toggle-off, replay them via a custom
;;    display action (`cj/--vterm-toggle-display-saved') that uses
;;    frame-edge directions and `(body-columns . N)' / `(body-lines . N)'
;;    so the result is divider-independent and layout-stable.

(require 'cl-lib)
(require 'seq)

(defcustom cj/vterm-toggle-window-height 0.7
  "Default fraction of frame height for the F12 vterm window.
Used as the size fallback when `cj/--vterm-toggle-last-size' is nil
(i.e. the user hasn't toggled off a vterm yet this session)."
  :type 'number
  :group 'vterm)

(defvar cj/--vterm-toggle-last-direction nil
  "Last user-chosen direction for the F12 vterm display.
Symbol: right, left, below, above.  nil means use the default
'below for F12's traditional bottom split.")

(defvar cj/--vterm-toggle-last-size nil
  "Last user-chosen body size for the F12 vterm display.
Positive integer: body-cols (right/left) or body-lines (below/above).
nil means fall back to `cj/vterm-toggle-window-height' as a fraction.")

(defun cj/--vterm-toggle-buffer-p (buffer)
  "Return non-nil when BUFFER is a vterm buffer F12 should manage.

Qualifies when BUFFER is alive, has `vterm-mode' (or its name starts
with the vterm-toggle prefix), AND its name does NOT start with the
claude prefix used by ai-vterm.el.  The claude exclusion keeps F12
from grabbing buffers that ai-vterm.el's F9 dispatch owns."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (or (eq major-mode 'vterm-mode)
                  (string-prefix-p (or (bound-and-true-p vterm-buffer-name)
                                       "*vterm*")
                                   (buffer-name buffer)))
              (not (string-prefix-p "claude [" (buffer-name buffer)))))))

(defun cj/--vterm-toggle-buffers ()
  "Return live F12-managed vterm buffers in `buffer-list' (MRU) order."
  (seq-filter #'cj/--vterm-toggle-buffer-p (buffer-list)))

(defun cj/--vterm-toggle-displayed-window (&optional frame)
  "Return a window in FRAME currently displaying an F12 vterm buffer, or nil.
FRAME defaults to the selected frame.  Minibuffer is excluded."
  (seq-find (lambda (w)
              (cj/--vterm-toggle-buffer-p (window-buffer w)))
            (window-list (or frame (selected-frame)) 'never)))

(defun cj/--vterm-toggle-window-direction (window)
  "Return the side WINDOW occupies in its frame.

Returns one of right, below, left, above.  Falls back to 'below
(F12's traditional bottom split) when WINDOW fills its frame's
root area.  Comparison uses `frame-root-window' edges so the
minibuffer doesn't make every full-area window look like it
fails to span the full height."
  (let* ((root (frame-root-window (window-frame window)))
         (edges (window-edges window))
         (root-edges (window-edges root))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bottom (nth 3 edges))
         (root-left (nth 0 root-edges))
         (root-top (nth 1 root-edges))
         (root-right (nth 2 root-edges))
         (root-bottom (nth 3 root-edges))
         (spans-full-width (and (= left root-left) (= right root-right)))
         (spans-full-height (and (= top root-top) (= bottom root-bottom))))
    (cond
     ((not spans-full-width) (if (= left root-left) 'left 'right))
     ((not spans-full-height) (if (= top root-top) 'above 'below))
     (t 'below))))

(defun cj/--vterm-toggle-window-size (window direction)
  "Return WINDOW's body size in cols (right/left) or lines (below/above)."
  (if (memq direction '(right left))
      (window-body-width window)
    (window-body-height window)))

(defun cj/--vterm-toggle-capture-state (window)
  "Capture WINDOW's direction + body size into module-level state."
  (when (window-live-p window)
    (let* ((dir (cj/--vterm-toggle-window-direction window))
           (size (cj/--vterm-toggle-window-size window dir)))
      (setq cj/--vterm-toggle-last-direction dir
            cj/--vterm-toggle-last-size size))))

(defun cj/--vterm-toggle-display-saved (buffer alist)
  "Display-buffer action: split per saved direction and body size.

Reads `cj/--vterm-toggle-last-direction' and
`cj/--vterm-toggle-last-size', falling back to 'below and
`cj/vterm-toggle-window-height' when nil.  The cardinal direction
is mapped to its frame-edge variant (`right' -> `rightmost', etc.)
so the new vterm always lands at the same frame edge it came from
regardless of which window is selected.  An integer size is wrapped
in a `(body-columns . N)' / `(body-lines . N)' cons so the body
width or height is set explicitly, divider-independent.  A float
size passes through as a fraction of the new window's parent."
  (let* ((direction (or cj/--vterm-toggle-last-direction 'below))
         (edge-direction (pcase direction
                           ('right 'rightmost)
                           ('left 'leftmost)
                           ('below 'bottom)
                           ('above 'top)
                           (_ 'bottom)))
         (size (or cj/--vterm-toggle-last-size cj/vterm-toggle-window-height))
         (size-key (if (memq direction '(right left))
                       'window-width
                     'window-height))
         (body-tag (if (memq direction '(right left))
                       'body-columns
                     'body-lines))
         (size-value (if (integerp size)
                         (cons body-tag size)
                       size))
         (filtered (cl-remove-if
                    (lambda (cell)
                      (memq (car-safe cell)
                            '(direction window-width window-height)))
                    alist))
         (effective (append
                     (list (cons 'direction edge-direction)
                           (cons size-key size-value))
                     filtered)))
    (display-buffer-in-direction buffer effective)))

(defun cj/--vterm-toggle-display-rule-list ()
  "Return the `display-buffer-alist' entry list installed by F12.

Routes any vterm buffer that satisfies `cj/--vterm-toggle-buffer-p'
through two actions: reuse-window (for visible vterm windows) then
the saved-geometry display action.  Excludes claude buffers via the
predicate -- those are handled by ai-vterm.el's display rule."
  '(((lambda (buffer-or-name _)
       (cj/--vterm-toggle-buffer-p (get-buffer buffer-or-name)))
     (display-buffer-reuse-window
      cj/--vterm-toggle-display-saved)
     (inhibit-same-window . t))))

(dolist (entry (cj/--vterm-toggle-display-rule-list))
  (add-to-list 'display-buffer-alist entry))

(defun cj/--vterm-toggle-dispatch ()
  "Compute the F12 (`cj/vterm-toggle') action without performing it.

Returns one of:
- (toggle-off . WINDOW)        -- vterm displayed in WINDOW; hide it.
- (show-recent . BUFFER)       -- vterm alive but not shown; redisplay.
- (create-new)                 -- no vterm buffer alive; create one."
  (let ((win (cj/--vterm-toggle-displayed-window)))
    (cond
     (win (cons 'toggle-off win))
     (t
      (let ((buffers (cj/--vterm-toggle-buffers)))
        (cond
         (buffers (cons 'show-recent (car buffers)))
         (t '(create-new))))))))

(declare-function vterm "vterm" (&optional buffer-name))

(defun cj/vterm-toggle ()
  "Toggle a normal (non-claude) vterm buffer.

- If an F12-managed vterm is currently displayed in this frame,
  capture its geometry and delete its window (toggle off).  Falls
  back to burying the buffer when the vterm is the only window in
  the frame.
- Otherwise, if any F12-managed vterm buffer is alive, display the
  most-recent one via the saved-geometry action.
- Otherwise, create a new vterm via `(vterm)' which routes through
  the same display action.

Excludes claude-prefixed vterm buffers; those have their own F9 /
C-F9 / M-F9 dispatch via `cj/ai-vterm'."
  (interactive)
  (pcase (cj/--vterm-toggle-dispatch)
    (`(toggle-off . ,win)
     (cj/--vterm-toggle-capture-state win)
     (if (one-window-p)
         (bury-buffer (window-buffer win))
       (delete-window win))
     nil)
    (`(show-recent . ,buf)
     (display-buffer buf)
     (let ((w (get-buffer-window buf)))
       (when w (select-window w)))
     buf)
    (`(create-new)
     (vterm))))

(keymap-global-set "<f12>" #'cj/vterm-toggle)

(provide 'eshell-vterm-config)
;;; eshell-vterm-config.el ends here.
