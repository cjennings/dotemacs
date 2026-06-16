;;; system-utils --- System-Wide Utilities -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/C/S.
;; Load shape: eager.
;; Eager reason: registers global keys (C-c b, C-<f10>, list-buffers remap) and
;;   a startup hook for its enhanced commands.
;; Top-level side effects: three keymap-global-set, one emacs-startup-hook, plus
;;   use-package configuration.
;; Runtime requires: system-lib, external-open-lib.
;; Direct test load: yes.
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

(require 'system-lib)
(require 'external-open-lib)

(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-file-name-at-point "dired" ())
(declare-function env-linux-p "host-environment" ())
(declare-function env-macos-p "host-environment" ())
(declare-function env-windows-p "host-environment" ())
(declare-function w32-shell-execute "w32fns" (operation document &optional parameters show-flag))

;;; -------------------------------- Eval Buffer --------------------------------

(defun cj/eval-buffer-with-confirmation-or-error-message ()
  "Evaluate the buffer and display a message."
  (interactive)
  (condition-case err
      (progn
        (eval-buffer)
        (message "Buffer evaluated."))
    (error
     (message "Error occurred during evaluation: %s" (error-message-string err)))))
(keymap-global-set "C-c b" #'cj/eval-buffer-with-confirmation-or-error-message)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c b" "eval buffer"))

;;; ---------------------------- Edit A File With Sudo ----------------------------

(use-package sudo-edit
  :commands sudo-edit
  :bind ("C-x M-f" . sudo-edit))

;;; ------------------------------- Open File With ------------------------------
;; TASK: Favor this method over cj/open-this-file-with and add to custom buffer funcs

(defun cj/open-file-with-command (command)
  "Open the current file with COMMAND.
Works in both Dired buffers and regular file buffers.  Prompts for a
file only when neither context yields one.  The command runs fully
detached from Emacs."
  (interactive "MOpen with command: ")
  (let* ((file (or (cj/file-from-context)
				   (read-file-name "File to open: "))))
	(unless (and file (file-exists-p file))
	  (error "No valid file found or selected"))
	(if (cj/external-open-launcher-p command)
		(progn
		  (call-process command nil 0 nil file)
		  (message "Opening %s with %s..."
				   (file-name-nondirectory file) command))
	  (let* ((output-buffer-name (format "*Open with %s: %s*"
										 command
										 (file-name-nondirectory file)))
			 (output-buffer (generate-new-buffer output-buffer-name)))
		(start-process-shell-command
		 command
		 output-buffer
		 (format "%s %s" command (shell-quote-argument file)))
		(message "Running %s on %s..."
				 (file-name-nondirectory file) command)))))


;;; ------------------------------ Server Shutdown ------------------------------

(defun cj/server-shutdown ()
  "Save buffers, kill Emacs and shutdown the server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))
(keymap-global-set "C-<f10>" #'cj/server-shutdown)

;;; ---------------------------- History Persistence ----------------------------

(use-package savehist
  :ensure nil  ; built-in
  :config
  (setq kill-ring-max 50
        history-length 50)

  (setq savehist-additional-variables
        '(kill-ring
          command-history
          set-variable-value-history
          custom-variable-history
          query-replace-history
          read-expression-history
          minibuffer-history
          read-char-history
          face-name-history
          bookmark-history
          file-name-history))

  (put 'minibuffer-history         'history-length 50)
  (put 'file-name-history          'history-length 50)
  (put 'set-variable-value-history 'history-length 25)
  (put 'custom-variable-history    'history-length 25)
  (put 'query-replace-history      'history-length 25)
  (put 'read-expression-history    'history-length 25)
  (put 'read-char-history          'history-length 25)
  (put 'face-name-history          'history-length 25)
  (put 'bookmark-history           'history-length 25)

  (setq history-delete-duplicates t)
  (let (message-log-max)
    (savehist-mode))
  )

;;; ----------------------------- List Buffers Remap ----------------------------

;; Remap list-buffers to ibuffer (built-in). Icons come from nerd-icons-ibuffer
;; in `nerd-icons-config'.
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)

;;; -------------------------- Scratch Buffer Happiness -------------------------

(defvar scratch-emacs-version-and-system
  (concat "# Emacs " emacs-version " ♥ you, " user-login-name ". Happy Hacking!\n"))
(defvar scratch-greet "\n")
(setopt initial-scratch-message
        (concat scratch-emacs-version-and-system scratch-greet))

;; Set scratch buffer to org-mode
(setopt initial-major-mode 'org-mode)

;; Tint the *scratch* background a shade lighter than the default so it reads
;; as the scratch buffer at a glance.  Buffer-local face remap, recomputed from
;; whatever theme is loaded.
(require 'color)

(defcustom cj/scratch-background-lighten 5
  "Percent to lighten the *scratch* background above the default background.
Aesthetic; tune to taste."
  :type 'integer
  :group 'convenience)

(defun cj/--scratch-lightened-background (bg)
  "Return BG lightened by `cj/scratch-background-lighten' percent.
Return nil when BG is not a usable color string (e.g. `unspecified')."
  (when (and (stringp bg) (color-name-to-rgb bg))
    (color-lighten-name bg cj/scratch-background-lighten)))

(defun cj/scratch-apply-background ()
  "Remap the *scratch* buffer background a shade lighter than the default."
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (let ((lighter (cj/--scratch-lightened-background
                      (face-attribute 'default :background nil t))))
        (when lighter
          (face-remap-add-relative 'default :background lighter))))))

;; Move cursor to end of scratch buffer on startup, and tint its background
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (get-buffer "*scratch*")
              (with-current-buffer "*scratch*"
                (goto-char (point-max))))
            (cj/scratch-apply-background)))

;;; --------------------------------- Dictionary --------------------------------

(use-package quick-sdcv
  :bind
  ("C-h d" . quick-sdcv-search-input)
  :bind (:map quick-sdcv-mode-map
         ("q" . quit-window))
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼"))

;;; ------------------------------ Process Monitor ------------------------------

(use-package proced
  :ensure nil ;; built-in
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
