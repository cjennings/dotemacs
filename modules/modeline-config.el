;;; modeline-config --- Modeline Settings -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:


;;; Code:

;; ------------------------------- Doom Modeline -------------------------------

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Performance optimizations
  (doom-modeline-buffer-file-name-style 'relative-from-project) ;; Faster than 'file-name
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback nil)
  (doom-modeline-minor-modes nil)           ;; Hide minor modes as requested
  (doom-modeline-enable-word-count nil)     ;; Faster without word count
  (doom-modeline-continuous-word-count-modes nil)
  (doom-modeline-buffer-encoding nil)       ;; Hide encoding for speed
  (doom-modeline-indent-info nil)           ;; Hide indent info for speed
  (doom-modeline-checker-simple-format t)   ;; Simpler checker format for speed
  (doom-modeline-number-limit 99)           ;; Lower number limit for better performance
  (doom-modeline-vcs-max-length 12)         ;; Limit VCS info length for speed
  (doom-modeline-persp-name nil)            ;; Disable perspective name for speed
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon nil)
  (doom-modeline-lsp nil)                   ;; Disable LSP info for speed

  ;; UI Preferences
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-window-width-limit 0.25)
  (doom-modeline-project-detection 'projectile) ;; Use projectile if available, nil is faster

  ;; Use nerd-icons instead of all-the-icons
  (doom-modeline-icon-preference 'nerd-icons)

  ;; Enable elements you specifically requested
  (doom-modeline-column-number t)           ;; Show column number
  (doom-modeline-percent-position t)        ;; Show percentage position
  (doom-modeline-buffer-name t)             ;; Show buffer name
  (doom-modeline-buffer-file-name t)        ;; Show file name
  :config
  (setq doom-modeline-refresh-rate 0.75))      ;; Update rate in seconds

;; (setq read-process-output-max (* 1024 1024)) ;; 1MB process read size for better performance

(provide 'modeline-config)
;;; modeline-config.el ends here
