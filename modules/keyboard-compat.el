;;; keyboard-compat.el --- Keyboard compatibility for terminal and GUI -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; This module fixes keyboard input differences between terminal and GUI Emacs.
;;
;; THE PROBLEM: Meta+Shift keybindings behave differently in terminal vs GUI
;; =========================================================================
;;
;; In Emacs, there are two ways to express "Meta + Shift + o":
;;
;;   1. M-O   (Meta + uppercase O) - key code 134217807
;;   2. M-S-o (Meta + explicit Shift modifier + lowercase o) - key code 167772271
;;
;; These are NOT the same key in Emacs!
;;
;; GUI Emacs behavior:
;;   When you press Meta+Shift+o on your keyboard, GUI Emacs receives M-O
;;   (uppercase O). It does NOT receive M-S-o. This is because the keyboard
;;   sends Shift+o as uppercase 'O', not as a Shift modifier plus lowercase 'o'.
;;
;; Terminal Emacs behavior:
;;   Terminals send escape sequences for special keys. Arrow keys send:
;;     - Up:    ESC O A
;;     - Down:  ESC O B
;;     - Right: ESC O C
;;     - Left:  ESC O D
;;
;;   The problem: ESC O is interpreted as M-O by Emacs! So if you bind M-O
;;   to a function, pressing the up arrow sends "ESC O A", Emacs sees "M-O"
;;   and triggers your function instead of moving up. Arrow keys break.
;;
;; THE SOLUTION: Different handling for each display type
;; ======================================================
;;
;; For terminal mode (handled by cj/keyboard-compat-terminal-setup):
;;   - Use input-decode-map to translate arrow escape sequences BEFORE
;;     any keybinding lookup. ESC O A becomes [up], not M-O followed by A.
;;   - Keybindings use M-S-o syntax (some terminals support explicit Shift)
;;   - Disable graphical icons that show as unicode artifacts
;;
;; For GUI mode (handled by cj/keyboard-compat-gui-setup):
;;   - Use key-translation-map to translate M-O to M-S-o BEFORE lookup
;;   - This way, pressing Meta+Shift+o (which sends M-O) gets translated
;;     to M-S-o, matching the keybinding definitions
;;   - All 18 Meta+Shift keybindings work correctly
;;
;; WHY NOT JUST USE M-O FOR KEYBINDINGS?
;; =====================================
;;
;; We could bind to M-O directly, but:
;;   1. Terminal arrow keys would break (ESC O prefix conflict)
;;   2. We'd need to maintain two sets of bindings (M-O for GUI, something
;;      else for terminal)
;;
;; By using M-S-o syntax everywhere and translating M-O -> M-S-o in GUI mode,
;; we have one consistent set of keybindings that work everywhere.
;;
;; KEYBINDINGS AFFECTED:
;; ====================
;;
;; The following M-S- keybindings are translated from M-uppercase in GUI:
;;
;;   M-O -> M-S-o  cj/kill-other-window (undead-buffers.el)
;;   M-M -> M-S-m  cj/kill-all-other-buffers-and-windows (undead-buffers.el)
;;   M-Y -> M-S-y  yank-media (keybindings.el)
;;   M-F -> M-S-f  fontaine-set-preset (font-config.el)
;;   M-W -> M-S-w  wttrin (weather-config.el)
;;   M-E -> M-S-e  eww (eww-config.el)
;;   M-L -> M-S-l  cj/switch-themes (ui-theme.el)
;;   M-R -> M-S-r  cj/elfeed-open (elfeed-config.el)
;;   M-V -> M-S-v  cj/split-and-follow-right (ui-navigation.el)
;;   M-H -> M-S-h  cj/split-and-follow-below (ui-navigation.el)
;;   M-T -> M-S-t  toggle-window-split (ui-navigation.el)
;;   M-S -> M-S-s  window-swap-states (ui-navigation.el)
;;   M-Z -> M-S-z  cj/undo-kill-buffer (ui-navigation.el)
;;   M-U -> M-S-u  winner-undo (ui-navigation.el)
;;   M-D -> M-S-d  dwim-shell-commands-menu (dwim-shell-config.el)
;;   M-I -> M-S-i  edit-indirect-region (text-config.el)
;;   M-C -> M-S-c  time-zones (chrono-tools.el)
;;   M-B -> M-S-b  calibredb (calibredb-epub-config.el)
;;   M-K -> M-S-k  show-kill-ring (show-kill-ring.el)

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

;; Icon disabling only in terminal mode (prevents unicode artifacts)
(when (env-terminal-p)
  ;; Disable nerd-icons display (shows as \uXXXX artifacts)
  (with-eval-after-load 'nerd-icons
    (defun nerd-icons-icon-for-file (&rest _) "")
    (defun nerd-icons-icon-for-dir (&rest _) "")
    (defun nerd-icons-icon-for-mode (&rest _) "")
    (defun nerd-icons-icon-for-buffer (&rest _) ""))

  ;; Disable dashboard icons
  (with-eval-after-load 'dashboard
    (setq dashboard-display-icons-p nil)
    (setq dashboard-set-file-icons nil)
    (setq dashboard-set-heading-icons nil))

  ;; Disable all-the-icons
  (with-eval-after-load 'all-the-icons
    (defun all-the-icons-icon-for-file (&rest _) "")
    (defun all-the-icons-icon-for-dir (&rest _) "")
    (defun all-the-icons-icon-for-mode (&rest _) "")))

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
    (define-key key-translation-map (kbd "M-S") (kbd "M-S-s"))
    (define-key key-translation-map (kbd "M-Z") (kbd "M-S-z"))
    (define-key key-translation-map (kbd "M-U") (kbd "M-S-u"))
    (define-key key-translation-map (kbd "M-D") (kbd "M-S-d"))
    (define-key key-translation-map (kbd "M-I") (kbd "M-S-i"))
    (define-key key-translation-map (kbd "M-C") (kbd "M-S-c"))
    (define-key key-translation-map (kbd "M-B") (kbd "M-S-b"))
    (define-key key-translation-map (kbd "M-K") (kbd "M-S-k"))))

;; Run early - key-translation-map should be set up before keybindings
(add-hook 'emacs-startup-hook #'cj/keyboard-compat-gui-setup)

(provide 'keyboard-compat)
;;; keyboard-compat.el ends here
