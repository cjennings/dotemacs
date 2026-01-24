;;; terminal-compat.el --- Terminal compatibility fixes -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Fixes for running Emacs in terminal/console mode, especially over mosh.
;; - Arrow key escape sequence handling
;; - Disable graphical icons that show as unicode artifacts

;;; Code:

(require 'host-environment)

(defun cj/terminal-compat-setup ()
  "Set up terminal compatibility after init completes."
  (when (env-terminal-p)
    ;; Fix arrow key escape sequences for various terminal types
    (define-key input-decode-map "\e[A" [up])
    (define-key input-decode-map "\e[B" [down])
    (define-key input-decode-map "\e[C" [right])
    (define-key input-decode-map "\e[D" [left])

    ;; Application mode arrows (sent by some terminals)
    (define-key input-decode-map "\eOA" [up])
    (define-key input-decode-map "\eOB" [down])
    (define-key input-decode-map "\eOC" [right])
    (define-key input-decode-map "\eOD" [left])))

;; Run after init completes to override any package settings
(add-hook 'emacs-startup-hook #'cj/terminal-compat-setup)

;; Icon disabling only in terminal mode
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

(provide 'terminal-compat)
;;; terminal-compat.el ends here
