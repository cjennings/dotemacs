;;; tramp-config.el --- Tramp Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; TRAMP (Transparent Remote Access, Multiple Protocol)
;;
;; To handle fancy prompts on remote servers, add this to your shell configuration:
;;
;; [[ $TERM == "dumb" ]] && PS1='$ ' && return
;; [[ $TERM == "tramp" ]] && PS1='$ ' && return
;;
;; For zsh users:
;; [[ $TERM == "dumb" || $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return

;;; Code:

(use-package tramp
  :defer .5
  :ensure nil ;; built-in
  :config
  ;; Debugging (uncomment when needed)
  ;; (setq tramp-debug-buffer t)
  ;; (setq tramp-verbose 10)

  ;; Basic Settings
  ;; Terminal type reported by tramp to host
  (setq tramp-terminal-type "dumb")

  ;; Use the path assigned to the remote user by the remote host
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Also check for a ~/.ssh/config host entry
  (tramp-set-completion-function "ssh"
								 '((tramp-parse-sconfig "/etc/ssh/ssh_config")
								   (tramp-parse-sconfig "~/.ssh/config")))

  ;; File Handling
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory
		(expand-file-name "tramp-auto-save" user-emacs-directory))

  ;; Create directory if it doesn't exist
  (unless (file-exists-p tramp-auto-save-directory)
	(make-directory tramp-auto-save-directory t))

  ;; Turn off the backup "$filename~" feature for remote files
  (setq remote-file-name-inhibit-auto-save-visited t)
  (add-to-list 'backup-directory-alist
			   (cons tramp-file-name-regexp nil))

  ;; Performance Settings
  ;; Set a more representative name for the persistency file
  (setq tramp-persistency-file-name
		(expand-file-name "tramp-connection-history" user-emacs-directory))

  ;; Always use external program to copy (more efficient)
  (setq tramp-copy-size-limit nil)

  ;; Cache remote file attributes for better performance
  (setq remote-file-name-inhibit-cache nil)

  ;; Don't check for modified buffers before revert
  ;; to avoid unnecessary remote operations
  (setq revert-without-query '(".*"))

  ;; Refresh buffers when needed rather than automatically
  (setq auto-revert-remote-files nil)

  ;; Security & Authentication
  ;; Cache and don't expire passwords
  (setq password-cache t)
  (setq password-cache-expiry nil)

  ;; Use SSH control connections for better performance
  (setq tramp-use-ssh-controlmaster-options t)

  ;; Connection Settings
  ;; Set tramp-direct-async-process locally in all ssh connections
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "sshx")
   'remote-direct-async-process)

  ;; Set sane defaults for frequently used methods
  (connection-local-set-profile-variables
   'remote-bash
   '((shell-file-name . "/bin/bash")
	 (shell-command-switch . "-c")))
  (connection-local-set-profiles
   '(:application tramp)
   'remote-bash)

  ;; Don't determine remote files VC status (for a performance gain)
  (setq vc-ignore-dir-regexp (concat vc-ignore-dir-regexp "\\|" tramp-file-name-regexp))

  ;; Method-specific settings
  ;; Default transfer method (use scp for most efficient transfer)
  (setq tramp-default-method "scp")

  ;; Use different methods based on host/domain patterns
  (add-to-list 'tramp-methods
			   '("sshfast"
				 (tramp-login-program "ssh")
				 (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c")
									("-e" "none") ("-t" "-t") ("%h")))
				 (tramp-async-args (("-q")))
				 (tramp-remote-shell "/bin/sh")
				 (tramp-remote-shell-login ("-l"))
				 (tramp-remote-shell-args ("-c"))
				 (tramp-connection-timeout 10)))

  ;; Remote shell and project settings
  ;; Support for Docker containers
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/usr/local/bin")
  (add-to-list 'tramp-remote-path "/usr/local/sbin")

  ;; Enable directory tracking (useful for eshell)
  (setq dirtrack-list '("^.*?\\([a-zA-Z]:.*\\|/.*\\)" 1))

  ;; Avoid problems with Git on TRAMP
  (setq magit-git-executable "/usr/bin/git")

  ;; Cleanup settings
  ;; Cleanup TRAMP buffers when idle (every 15 min)
  (setq tramp-cleanup-idle-time 900))

(provide 'tramp-config)
;;; tramp-config.el ends here
