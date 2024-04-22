;;; eshell-vterm-config --- Settings for the Emacs Shell -*- lexical-binding: t; -*-
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

;; ------------------------------ Eshell -----------------------------
;; the Emacs shell.

(use-package eshell
  :ensure nil ;; built-in
  :defer .5
  :config
  (setq eshell-banner-message "")
  (setq eshell-scroll-to-bottom-on-input 'all)
  (setq eshell-error-if-no-glob t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)
  (setq eshell-prefer-lisp-functions nil)
  (setq eshell-destroy-buffer-when-process-dies t)

  (add-hook
   'eshell-mode-hook
   (lambda ()
	 (setq pcomplete-cycle-completions nil)))
  (setq eshell-cmpl-cycle-completions nil)

  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (add-hook 'eshell-hist-mode-hook
            (lambda ()
              (define-key eshell-hist-mode-map (kbd "<up>") 'previous-line)
              (define-key eshell-hist-mode-map (kbd "<down>") 'next-line)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands '("lf" "ranger" "tail" "htop" "gotop" "mc" "ncdu" "top"))
              (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
              (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))

              ;; aliases
              (eshell/alias "clear"  "clear 1") ;; leaves prompt at the top of the window
              (eshell/alias "e"      "find-file $1")
              (eshell/alias "gocj"   "cd /sshx:cjennings@cjennings.net:/var/cjennings/")
              (eshell/alias "gosb"   "cd /sshx:cjennings@wolf.usbx.me:/home/cjennings/")
              (eshell/alias "gowolf" "cd /sshx:cjennings@wolf.usbx.me:/home/cjennings/")
              (eshell/alias "v"      "eshell-exec-visual $*")
              (eshell/alias "ff"     "find-file-other-window $1")
              (eshell/alias "f"      "find-using-dired $1")
              (eshell/alias "r"      "ranger")
              (eshell/alias "em"     "find-file $1")
              (eshell/alias "emacs"  "find-file $1")
              (eshell/alias "ll"     "ls -l"))))

(defun eshell/find-file-other-window (file)
  (find-file-other-window (mapconcat 'identity file " ")))

(defun eshell/find-file (file)
  (find-file-other-window (mapconcat 'identity file " ")))

(defun eshell/find-using-dired (file-pattern)
  "Find a file FILE-PATTERN' using 'find-name-dired'."
  (let ((escaped-pattern (regexp-quote file-pattern)))
    (find-name-dired . escaped-pattern)))

(defun cj/eshell-delete-window-on-exit ()
  "Close the eshell window when exiting."
  (when (not (one-window-p))
    (delete-window)))
(advice-add 'eshell-life-is-too-much :after 'cj/eshell-delete-window-on-exit)

(use-package eshell-toggle
  :after eshell
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("<f12>" . eshell-toggle))

(use-package xterm-color
  :defer .5
  :after eshell
  :hook
  (eshell-before-prompt-hook . (lambda ()
							   (setq xterm-color-preserve-properties t)))
  :config
  (setenv "TERM" "xterm-256color"))

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

  :hook (vterm-mode . cj/turn-off-chrome-for-vterm)
  :bind
  (:map vterm-mode-map
        ("C-y"     . vterm-yank)
        ("C-p"     . vterm-copy-mode)
        ("<pause>" . vterm-copy-mode))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 100000))

(use-package vterm-toggle
  :defer .5
  :bind
  ("C-<f12>" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (dedicated . t) ;dedicated is supported in Emacs 27+
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(provide 'eshell-vterm-config)
;;; eshell-vterm-config.el ends here.
