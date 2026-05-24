;;; eshell-config.el --- Settings for the Emacs Shell -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none; shell/REPL, a command/hook-loaded deferral candidate.
;; Top-level side effects: one add-hook, one advice-add, package config.
;; Runtime requires: system-utils.
;; Direct test load: yes.
;;
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

;;; Code:

(require 'system-utils)

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
  ;; Scope `TERM=xterm-256color' to eshell-spawned processes only by
  ;; binding the env var on the eshell mode hook.  The previous global
  ;; `setenv' at config-time changed `process-environment' for the
  ;; whole Emacs process, so every subsequent `start-process' inherited
  ;; `xterm-256color' regardless of whether the receiver was a terminal
  ;; that could actually interpret the escapes.
  :hook
  (eshell-mode . (lambda ()
                   (setq-local process-environment
                               (cons "TERM=xterm-256color"
                                     process-environment)))))

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

(provide 'eshell-config)
;;; eshell-config.el ends here.
