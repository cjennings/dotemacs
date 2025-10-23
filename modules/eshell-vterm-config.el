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
              (eshell/alias "clear"  "clear 1") ;; leaves prompt at the top of the window
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
	"Automatically launch tmux in vterm if not already in a tmux session."
	(let ((proc (get-buffer-process (current-buffer))))
	  (when (and proc
				 (not (getenv "TMUX"))) ; Check if not already in tmux
		(vterm-send-string "tmux\n"))))
  :hook
  ((vterm-mode . cj/turn-off-chrome-for-vterm)
   (vterm-mode . cj/vterm-launch-tmux))
  :bind
  (:map vterm-mode-map
		("<f12>"   . nil)
		("C-y"     . vterm-yank)
		("C-p"     . vtermf-copy-mode)
		("<pause>" . vterm-copy-mode))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 100000)
  :config
  (setq vterm-timer-delay nil))

(use-package vterm-toggle
  :defer .5
  :bind
  ("<f12>" . vterm-toggle)
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
				 (window-height . 0.7))))

(provide 'eshell-vterm-config)
;;; eshell-vterm-config.el ends here.
