;;; system-utils --- System-Wide Utilities -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; A "system-util" is an enhancement to common a Emacs command, or the extension
;; of an existing command. Perhaps this group can be better named.
;;
;; Eval-Buffer     : This is a command I use frequently that's not mapped and
;;                   doesn't give confirmation. Fixed it.
;; Sudo-Edit       : If you're using Emacs to edit system files, you'll want
;;                   this so you don't have to run Emacs as root.
;; Open-File-With  : This set of methods will open a given file using the default application
;;                   for the OS and detached from Emacs. Leveraged in Dired/Dirvish.
;; Server Shutdown : Closes the running Emacs Server. Will close all attached Emacs clients.
;; savehist        : Persistence for recently opened files, and minibuffer history
;; ibuffer         : Better replacement for list-buffer with icons for easier recognition
;; scratch-buffer  : A little joy in the scratch buffer can go a long way.
;; dictionary      : look up words via sdcv
;; log-silently    : for debugging messages where you don't want to see activity in the echo area.
;; proced          : I was astonished to discover a process monitor application built into Emacs.
;;                   Then again, given all that's here, how surprising is it really?
;;
;;; Code:

(require 'cl-lib)
(require 'host-environment)
(require 'user-constants)

;;; -------------------------------- Eval Buffer --------------------------------

(defun cj/eval-buffer-with-confirmation-or-error-message ()
  "Evaluate the buffer and display a message."
  (interactive)
  (let ((result (eval-buffer)))
    (if (not (eq result 'error))
        (message "Buffer evaluated.")
      (message "error occurred during evaluation: %s" result))))
(global-set-key (kbd "C-c b")   'cj/eval-buffer-with-confirmation-or-error-message)

;;; ---------------------------- Edit A File With Sudo ----------------------------

(use-package sudo-edit
  :defer 1
  :bind ("C-x M-f" . sudo-edit))

;;; ------------------------------- Open File With ------------------------------
;; TASK: Favor this method over cj/open-this-file-with and add to custom buffer funcs

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


(defun cj/identify-external-open-command ()
  "Return the OS-default \"open\" command for this host.
Signals an error if the host is unsupported."
  (cond
   ((env-linux-p)   "xdg-open")
   ((env-macos-p)   "open")
   ((env-windows-p) "start")
   (t (error "external-open: unsupported host environment"))))

(defun cj/xdg-open (&optional filename)
  "Open FILENAME (or the file at point) with the OS default handler.
Logs output and exit code to buffer *external-open.log*."
  (interactive)
  (let* ((file  (expand-file-name (or filename (dired-file-name-at-point))))
         (cmd   (cj/identify-external-open-command))
         (logbuf (get-buffer-create "*external-open.log*")))
    (with-current-buffer logbuf
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (format "Opening: %s\n" file)))
    (cond
     ;; Windows: let the shell handle association; fully detached.
     ((env-windows-p)
      (w32-shell-execute "open" file))
     ;; macOS/Linux: run the opener synchronously; it returns immediately.
     (t
      (call-process cmd nil 0 nil file)
      (with-current-buffer logbuf
        (insert "  → Launched asynchronously\n"))))
    nil))

;;; ------------------------------ Server Shutdown ------------------------------

(defun server-shutdown ()
  "Save buffers, kill Emacs and shutdown the server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))
(global-set-key (kbd "C-<f10>") #'server-shutdown)

;;; ---------------------------- History Persistence ----------------------------
;; Persist history over Emacs restarts

(use-package savehist
  :ensure nil  ; built-in
  :init
  (savehist-mode)
  :config
  (setq savehist-file  "~/.emacs.d/.emacs-history"))

;;; ------------------------ List Buffers With Nerd Icons -----------------------

(global-set-key [remap list-buffers] #'ibuffer)
(use-package nerd-icons-ibuffer
  :defer 0.5
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon            t
        nerd-icons-ibuffer-color-icon      t
        nerd-icons-ibuffer-human-readable-size t))

;;; -------------------------- Scratch Buffer Happiness -------------------------

(defvar scratch-emacs-version-and-system
  (concat ";; Emacs " emacs-version
          " on " system-configuration ".\n"))
(defvar scratch-greet
  (concat ";; Emacs ♥ you, " user-login-name ". Happy Hacking!\n\n"))
(setq initial-scratch-message
      (concat scratch-emacs-version-and-system scratch-greet))
(setq initial-major-mode 'org-mode)

;;; --------------------------------- Dictionary --------------------------------

(use-package quick-sdcv
  :defer 1
  :bind
  ("C-h d" . quick-sdcv-search-input)
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼"))

;;; -------------------------------- Log Silently -------------------------------

(defun cj/log-silently (format-string &rest args)
  "Append formatted message (FORMAT-STRING with ARGS) to *Messages* buffer.
This does so without echoing in the minibuffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (apply #'format format-string args))
      (unless (bolp) (insert "\n")))))

;;; ------------------------------ Process Monitor ------------------------------

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
