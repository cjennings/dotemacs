;;; system-utils --- System-Wide Utilities -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'host-environment)
(require 'user-constants)

;; ---------------------------- Edit A File With Sudo ----------------------------

(use-package sudo-edit
  :defer 1
  :bind ("C-x M-f" . sudo-edit))

;; ------------------------------- Open File With ------------------------------

(defun cj/open-file-with-command (command)
  "Open the current file with COMMAND.
Works in both Dired buffers and regular file buffers. The command runs
fully detached from Emacs."
  (interactive "MOpen with command: ")
  (let* ((file (cond
				;; In dired/dirvish mode, get file at point
				((derived-mode-p 'dired-mode)
				 (require 'dired)
				 (dired-get-file-for-visit))
				;; In a regular file buffer
				(buffer-file-name
				 buffer-file-name)
				;; Fallback - prompt for file
				(t
				 (read-file-name "File to open: "))))
		 ;; For xdg-open and similar launchers, we need special handling
		 (is-launcher (member command '("xdg-open" "open" "start"))))
	;; Validate file exists
	(unless (and file (file-exists-p file))
	  (error "No valid file found or selected"))
	;; Use different approaches for launchers vs regular commands
	(if is-launcher
		;; For launchers, use call-process with 0 to fully detach
		(progn
		  (call-process command nil 0 nil file)
		  (message "Opening %s with %s..." (file-name-nondirectory file) command))
	  ;; For other commands, use start-process-shell-command for potential output
	  (let* ((output-buffer-name (format "*Open with %s: %s*"
										 command
										 (file-name-nondirectory file)))
			 (output-buffer (generate-new-buffer output-buffer-name)))
		(start-process-shell-command
		 command
		 output-buffer
		 (format "%s %s" command (shell-quote-argument file)))
		(message "Running %s on %s..." command (file-name-nondirectory file))))))

;; ------------------------------ Server Shutdown ------------------------------

(defun server-shutdown ()
  "Save buffers, kill Emacs and shutdown the server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))
(global-set-key (kbd "C-<f10>") #'server-shutdown)
(global-set-key (kbd "<f10>")     #'save-buffers-kill-terminal)


;; ---------------------------- History Persistence ----------------------------
;; Persist history over Emacs restarts

(use-package savehist
  :ensure nil  ; built-in
  :init
  (savehist-mode)
  :config
  (setq savehist-file  "~/.emacs.d/.emacs-history"))

;; ------------------------ List Buffers With Nerd Icons -----------------------

(global-set-key [remap list-buffers] #'ibuffer)
(use-package nerd-icons-ibuffer
  :defer 0.5
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon            t
		nerd-icons-ibuffer-color-icon      t
		nerd-icons-ibuffer-human-readable-size t))

;; -------------------------- Scratch Buffer Happiness -------------------------

(defvar scratch-emacs-version-and-system
  (concat ";; Emacs " emacs-version
		  " on " system-configuration ".\n"))
(defvar scratch-greet
  (concat ";; Emacs ♥ you, " user-login-name ". Happy Hacking!\n\n"))
(setq initial-scratch-message
	  (concat scratch-emacs-version-and-system scratch-greet))
(setq initial-major-mode 'org-mode)

;; --------------------------------- Dictionary --------------------------------

(use-package quick-sdcv
  :defer 1
  :bind
  ("C-h d" . quick-sdcv-search-input)
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼"))

;; -------------------------------- Log Silently -------------------------------

(defun cj/log-silently (text)
  "Append TEXT to *Messages* buffer without echoing in the minibuffer."
  (let ((inhibit-read-only t))
	(with-current-buffer (get-buffer-create "*Messages*")
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (insert text)
      (unless (bolp) (insert "\n")))))

;; ------------------------------ Process Monitor ------------------------------

(use-package proced
  :ensure nil ;; built-in
  :defer 0.5
  :commands proced
  :bind ("C-M-p" . proced)
  :custom
  (proced-auto-update-flag t)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list 'proced-format-alist
			   '(custom user pid ppid sess tree pcpu pmem rss start time
						state (args comm))))

(provide 'system-utils)
;;; system-utils.el ends here
