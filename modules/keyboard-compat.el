;;; keyboard-compat.el --- Keyboard compatibility for terminal and GUI -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 1 (Foundation).
;; Category: F/S.
;; Load shape: eager.
;; Eager reason: normalizes terminal/GUI key input before custom bindings matter.
;; Top-level side effects: adds cj/keyboard-compat-terminal-setup to startup.
;; Runtime requires: host-environment.
;; Direct test load: yes.
;;
;; Normalizes Meta+Shift bindings across GUI and terminal frames. GUI frames
;; translate M-uppercase events to explicit M-S-lowercase keys; terminal frames
;; decode arrow escape sequences before key lookup so ESC O prefixes do not trip
;; M-S bindings.
;;
;; Also provides terminal-specific display fallbacks, such as hiding icon glyphs
;; that render poorly outside GUI frames.

;;; Code:

(require 'host-environment)

;; =============================================================================
;; Terminal-specific fixes
;; =============================================================================

(defun cj/keyboard-compat-terminal-setup ()
  "Set up keyboard compatibility for terminal/console mode.
This runs after init to override any package settings."
  (when (env-terminal-p)
    ;; Fix arrow key escape sequences for various terminal types
    ;; These must be decoded BEFORE keybinding lookup to prevent
    ;; M-O prefix from intercepting arrow keys
    (define-key input-decode-map "\e[A" [up])
    (define-key input-decode-map "\e[B" [down])
    (define-key input-decode-map "\e[C" [right])
    (define-key input-decode-map "\e[D" [left])

    ;; Application mode arrows (sent by some terminals like xterm)
    (define-key input-decode-map "\eOA" [up])
    (define-key input-decode-map "\eOB" [down])
    (define-key input-decode-map "\eOC" [right])
    (define-key input-decode-map "\eOD" [left])))

;; Run after init completes to override any package settings
(add-hook 'emacs-startup-hook #'cj/keyboard-compat-terminal-setup)

;; Icon-rendering functions return blank on terminal frames so unicode
;; artifacts don't show up. The check runs per call against the selected
;; frame, so the same daemon serves real icons to GUI clients and blanks to
;; terminal clients. Earlier this lived in a top-level (when (env-terminal-p)
;; ...) block that redefined the icon functions at module-load time, which
;; broke under daemon startup: no frame exists yet, display-graphic-p returns
;; nil, env-terminal-p returns t, and the stubs install permanently. GUI
;; clients connecting later saw empty icons everywhere.

(defun cj/--icon-blank-in-terminal (orig &rest args)
  "Return empty string on a terminal frame, otherwise call ORIG with ARGS."
  (if (display-graphic-p) (apply orig args) ""))

(with-eval-after-load 'nerd-icons
  (dolist (fn '(nerd-icons-icon-for-file
                nerd-icons-icon-for-dir
                nerd-icons-icon-for-mode
                nerd-icons-icon-for-buffer))
    (advice-add fn :around #'cj/--icon-blank-in-terminal)))

(with-eval-after-load 'all-the-icons
  (dolist (fn '(all-the-icons-icon-for-file
                all-the-icons-icon-for-dir
                all-the-icons-icon-for-mode))
    (advice-add fn :around #'cj/--icon-blank-in-terminal)))

;; =============================================================================
;; GUI-specific fixes
;; =============================================================================

(defun cj/keyboard-compat-gui-setup ()
  "Set up keyboard compatibility for GUI mode.
Translates M-uppercase keys to M-S-lowercase so that pressing
Meta+Shift+letter triggers M-S-letter keybindings."
  (when (env-gui-p)
    ;; Translate M-O (what keyboard sends) to M-S-o (what keybindings use)
    ;; key-translation-map runs before keybinding lookup
    (define-key key-translation-map (kbd "M-O") (kbd "M-S-o"))
    (define-key key-translation-map (kbd "M-M") (kbd "M-S-m"))
    (define-key key-translation-map (kbd "M-Y") (kbd "M-S-y"))
    (define-key key-translation-map (kbd "M-F") (kbd "M-S-f"))
    (define-key key-translation-map (kbd "M-W") (kbd "M-S-w"))
    (define-key key-translation-map (kbd "M-E") (kbd "M-S-e"))
    (define-key key-translation-map (kbd "M-L") (kbd "M-S-l"))
    (define-key key-translation-map (kbd "M-R") (kbd "M-S-r"))
    (define-key key-translation-map (kbd "M-V") (kbd "M-S-v"))
    (define-key key-translation-map (kbd "M-H") (kbd "M-S-h"))
    (define-key key-translation-map (kbd "M-T") (kbd "M-S-t"))
    (define-key key-translation-map (kbd "M-Z") (kbd "M-S-z"))
    (define-key key-translation-map (kbd "M-U") (kbd "M-S-u"))
    (define-key key-translation-map (kbd "M-D") (kbd "M-S-d"))
    (define-key key-translation-map (kbd "M-I") (kbd "M-S-i"))
    (define-key key-translation-map (kbd "M-C") (kbd "M-S-c"))
    (define-key key-translation-map (kbd "M-B") (kbd "M-S-b"))
    (define-key key-translation-map (kbd "M-K") (kbd "M-S-k"))))

;; In daemon mode, no frame exists at startup so env-gui-p returns nil.
;; Use server-after-make-frame-hook to set up translations when the first
;; GUI client connects. In non-daemon mode, run at startup as before.
;;
;; `add-hook' is idempotent for named functions (re-adding the same
;; symbol is a no-op), and `cj/keyboard-compat-gui-setup' itself only
;; calls `define-key' -- each key has one binding regardless of how many
;; times the function fires -- so this block is safe under repeated
;; module loads.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'cj/keyboard-compat-gui-setup)
  (add-hook 'emacs-startup-hook #'cj/keyboard-compat-gui-setup))

(provide 'keyboard-compat)
;;; keyboard-compat.el ends here
