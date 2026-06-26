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

;; Eshell is loaded lazily (:commands eshell), so its vars and functions are
;; not defined when this file is byte-compiled standalone. Declare them to
;; silence compile-time free-variable / undefined-function warnings.
(defvar eshell-banner-message)
(defvar eshell-scroll-to-bottom-on-input)
(defvar eshell-error-if-no-glob)
(defvar eshell-hist-ignoredups)
(defvar eshell-save-history-on-exit)
(defvar eshell-prefer-lisp-functions)
(defvar eshell-destroy-buffer-when-process-dies)
(defvar eshell-prompt-function)
(defvar eshell-cmpl-cycle-completions)
(defvar eshell-modules-list)
(defvar eshell-hist-mode-map)
(defvar eshell-visual-commands)
(defvar eshell-visual-subcommands)
(defvar eshell-visual-options)
(defvar eshell-history-ring)
(defvar eshell-preoutput-filter-functions)
(defvar eshell-output-filter-functions)

(declare-function ring-elements "ring")
(declare-function eshell-send-input "esh-mode")
(declare-function eshell/pwd "em-dirs")
(declare-function eshell/alias "em-alias")
(declare-function eshell/cd "em-dirs")
(declare-function eshell-stringify "esh-util")
(declare-function eat-eshell-mode "eat")

(defgroup cj/eshell nil
  "Personal Eshell configuration."
  :group 'eshell)

(defcustom cj/eshell-ssh-hosts
  '(("gocj"   . "/sshx:cjennings@cjennings.net:/var/cjennings/")
    ("gosb"   . "/sshx:cjennings@wolf.usbx.me:/home/cjennings/")
    ("gowolf" . "/sshx:cjennings@wolf.usbx.me:/home/cjennings/"))
  "Alist of Eshell SSH-jump aliases.
Each entry is a cons cell (ALIAS-NAME . REMOTE-PATH).  At Eshell
startup an alias named ALIAS-NAME is defined that runs `cd' to the
given TRAMP REMOTE-PATH.  Override on a different machine to point at
your own hosts, or set to nil to define no SSH aliases."
  :type '(alist :key-type (string :tag "Alias")
                :value-type (string :tag "Remote path"))
  :group 'cj/eshell)

(defun cj/--eshell-ssh-alias-commands (hosts)
  "Return the SSH alias definitions for HOSTS.
HOSTS is an alist of (ALIAS-NAME . REMOTE-PATH) as in
`cj/eshell-ssh-hosts'.  The result is a list of (ALIAS-NAME . COMMAND)
pairs where COMMAND is the `cd' string `eshell/alias' should run."
  (mapcar (lambda (entry)
            (cons (car entry) (concat "cd " (cdr entry))))
          hosts))

(defun cj/--eshell-define-ssh-aliases (hosts)
  "Define the Eshell SSH-jump aliases for HOSTS via `eshell/alias'."
  (dolist (pair (cj/--eshell-ssh-alias-commands hosts))
    (eshell/alias (car pair) (cdr pair))))

;; ---------------------------- prompt segments --------------------------------

(defun cj/--eshell-git-branch ()
  "Return the current git branch for `default-directory', or nil.
Reads .git/HEAD directly so it adds no subprocess per prompt, and skips remote
directories so a TRAMP prompt stays fast."
  (unless (file-remote-p default-directory)
    (when-let* ((root (locate-dominating-file default-directory ".git"))
                (head (expand-file-name ".git/HEAD" root)))
      (when (file-readable-p head)
        (with-temp-buffer
          (insert-file-contents head)
          (when (looking-at "ref: refs/heads/\\(.*\\)")
            (string-trim (match-string 1))))))))

(defun cj/--eshell-prompt-status-segment ()
  "Return the eshell prompt's exit-status segment, or an empty string.
Shows the last command's exit code in brackets when it was non-zero, mirroring
the zsh prompt's failure indicator."
  (let ((status (bound-and-true-p eshell-last-command-status)))
    (if (or (null status) (zerop status))
        ""
      (format " [%d]" status))))

;; ------------------------------- zoxide --------------------------------------
;; Share the same frecency database as the zsh shell by calling the zoxide
;; binary: `z' jumps to a remembered directory, and every eshell directory
;; change feeds `zoxide add' so eshell visits accrue in the same database.

(defun eshell/z (&rest args)
  "Jump to a directory via zoxide, sharing the zsh zoxide database.
With no ARGS, cd home.  Otherwise query zoxide for the best match and cd there."
  (if (null args)
      (eshell/cd)
    (let ((dir (string-trim
                (shell-command-to-string
                 (concat "zoxide query -- "
                         (mapconcat #'shell-quote-argument
                                    (mapcar #'eshell-stringify args) " "))))))
      (if (and (not (string-empty-p dir)) (file-directory-p dir))
          (eshell/cd dir)
        (error "zoxide: no match for %s"
               (string-join (mapcar #'eshell-stringify args) " "))))))

(defun cj/--eshell-zoxide-add ()
  "Record `default-directory' in the zoxide database (skips remote dirs)."
  (when (and (not (file-remote-p default-directory))
             (executable-find "zoxide"))
    (call-process "zoxide" nil 0 nil "add" "--"
                  (expand-file-name default-directory))))

(add-hook 'eshell-directory-change-hook #'cj/--eshell-zoxide-add)

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
		   (propertize (format-time-string "[%d-%m-%y %T]") 'face 'default)
		   " "
		   (propertize (user-login-name) 'face 'default)
		   " "
           (propertize (system-name) 'face 'default)
		   ":"
		   (propertize (abbreviate-file-name (eshell/pwd)) 'face 'default)
		   (let ((branch (cj/--eshell-git-branch)))
		     (if branch (propertize (concat " (" branch ")") 'face 'default) ""))
		   (propertize (cj/--eshell-prompt-status-segment) 'face 'default)
		   "\n"
		   (propertize "%"  'face 'default)
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
              (dolist (cmd '("lf" "ranger" "tail" "htop" "gotop" "mc" "ncdu" "top"))
                (add-to-list 'eshell-visual-commands cmd))
              (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
              (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))

              ;; aliases
              (eshell/alias "e"      "find-file $1")
			  (eshell/alias "em"     "find-file $1")
			  (eshell/alias "emacs"  "find-file $1")
			  (eshell/alias "open"   "cj/xdg-open $1")
			  (cj/--eshell-define-ssh-aliases cj/eshell-ssh-hosts)
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

;; Run eshell's external commands through EAT (a real terminal): visual commands
;; (vim, htop, less) render properly and ANSI output is faithful, while eshell
;; stays the shell -- elisp functions as commands + TRAMP transparency.  EAT
;; handles color itself, so it supersedes xterm-color for eshell; the
;; xterm-color block below stays for now and steps aside if colors double up.
(with-eval-after-load 'esh-mode
  (require 'eat)
  (eat-eshell-mode 1))

;; eshell-toggle and xterm-color are retired.  F12 opens eshell now (the
;; dock-and-remember toggle in eat-config.el), and eat-eshell-mode renders
;; eshell's output through EAT, which handles ANSI color natively -- so
;; xterm-color's filter and its TERM=xterm-256color override are redundant and
;; would fight EAT's own TERM=eat-truecolor.

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
