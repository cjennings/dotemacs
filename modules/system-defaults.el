;;; system-defaults --- Emacs Non-UI Preferences -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ---------------------------------- Unicode ----------------------------------

(set-locale-environment "en_US.UTF-8")
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-charset-priority 'unicode)
(setq x-select-request-type
      '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; -------------------------- Disabling Functionality --------------------------

(defun cj/disabled ()
  "Do nothing. Functionality disabled."
  (interactive))

;; VIEW EMACS NEWS
;; no news is good news
(defalias 'view-emacs-news 'cj/disabled)
(global-unset-key (kbd "C-h n"))

;; DESCRIBE GNU PROJECT
;; no gnus is good gnus
(defalias 'describe-gnu-project 'cj/disabled)
(global-unset-key (kbd "C-h g"))

;; CUSTOMIZATIONS
;; All customizations should be declared in Emacs init files.
;; any customizations go into a temp file that's never read.
(setq custom-file (make-temp-file
				   "emacs-customizations-trashbin"))

;; ------------------------- Re-Enabling Functionality -------------------------

(put 'narrow-to-region 'disabled nil) ;; narrow-to-region is extremely useful!
(put 'upcase-region 'disabled nil) ;; upcase region is useful
(put 'erase-buffer 'disabled nil)  ;; and so is erase-buffer

;; ------------------------------ Non UI Settings ------------------------------

(setq ring-bell-function 'ignore)            ;; disable the bell ring.
(setq default-directory user-home-dir)       ;; consider user home the default directory

(global-auto-revert-mode)                    ;; update the buffer when the associated file has changed
(setq global-auto-revert-non-file-buffers t) ;; do so for all buffer types (e.g., ibuffer)
(setq bidi-display-reordering nil)           ;; don't reorder bidirectional text for display
(setq bidi-paragraph-direction t)            ;; forces directionality of text for performance.

(setq system-time-locale "C")                ;; use en_US locale to format time.

;; --------------------------------- Clipboard ---------------------------------

(setq select-enable-clipboard t)             ;; cut and paste using clipboard
(setq yank-pop-change-selection t)           ;; update system clipboard when yanking in emacs
(setq save-interprogram-paste-before-kill t) ;; saves existing clipboard to kill ring before replacing

;; -------------------------------- Tab Settings -------------------------------
;; use spaces, not tabs

(setq-default tab-width 4)          ;; if tab, make them 4 spaces default
(setq-default indent-tabs-mode nil) ;; but turn off tabs by default

;; ------------------------------ Scroll Settings ------------------------------

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling

;; ----------------------------- Case Insensitivity ----------------------------
;; make user interfaces case insensitive

(setq case-fold-search t)                      ;; case-insensitive searches
(setq completion-ignore-case t)                ;; case-insensitive completion
(setq read-file-name-completion-ignore-case t) ;; case-insensitive file completion

;; ------------------------------- Async Commands ------------------------------
;; always create new async command buffers silently

(setq async-shell-command-buffer 'new-buffer)

;; never automatically display async command output buffers
;; but keep them in the buffer list for later inspection
(add-to-list 'display-buffer-alist
             '("*Async Shell Command*" display-buffer-no-window (nil)))

;; ------------------------ Mouse And Trackpad Settings ------------------------
;; provide smoothest scrolling and avoid accidental gestures

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-margin 5)             ;; scroll w/in 10 lines of top/bottom
(setq scroll-step 1)               ;; keyboard scroll one line at a time

;; disable pasting with mouse-wheel click
(global-unset-key (kbd "<mouse-2>"))

;; disable pinching gesture or mouse-wheel changing font size
(global-unset-key (kbd "<pinch>"))
(global-set-key [remap mouse-wheel-text-scale] 'cj/disabled)

;; ------------------------------- Be Quiet(er)! -------------------------------
;; reduces "helpful" instructions that distract Emacs power users.

(setq-default vc-follow-symlinks)             ;; don't ask to follow symlinks if target is version controlled
(setq kill-buffer-query-functions             ;; don't ask about killing buffers with processes, just kill them
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq confirm-kill-processes nil)             ;; automatically kill running processes on exit
(setq confirm-nonexistent-file-or-buffer nil) ;; don't ask if a file I visit with C-x C-f or C-x b doesn't exist
(setq ad-redefinition-action 'accept)         ;; silence warnings about advised functions getting redefined.
(setq large-file-warning-threshold nil)       ;; open files regardless of size
(fset 'yes-or-no-p 'y-or-n-p)                 ;; require a single letter for binary answers
(setq use-short-answers t)                    ;; same as above with Emacs 28+
(setq auto-revert-verbose nil)                ;; turn off auto revert messages
(setq custom-safe-themes t)                   ;; treat all themes as safe (stop asking)
(setq server-client-instructions nil)         ;; I already know what to do when done with the frame

;; ------------------ Reduce Garbage Collections In Minibuffer -----------------
;; triggers garbage collection when it won't impact user minibuffer entries

(defun cj/minibuffer-setup-hook ()
  "Hook to prevent garbage collection while user's in minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun cj/minibuffer-exit-hook ()
  "Hook to trigger garbage collection when exiting minibuffer."
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'cj/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'cj/minibuffer-exit-hook)

;; ----------------------------- Bookmark Settings -----------------------------
;; keep bookmarks in sync location, and save the file whenever a mark is added

;; place bookmark file sync'd org files
(setq bookmark-default-file (concat sync-dir "emacs_bookmarks"))

;; save bookmarks each (1) time it's modified.
(setq bookmark-save-flag 1)

;; -------------------------------- Recent Files -------------------------------
;; don't suggest bookmarks, packages, indexes, or recentf in recent files.

(use-package recentf
  :defer .5
  :ensure nil ;;built-in
  :config
  (setq recentf-max-saved-items 1000)
  (setq recentf-max-menu-items 50)
  (add-to-list 'recentf-exclude "emacs_bookmarks")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/elpa")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/recentf")
  (add-to-list 'recentf-exclude "\\ElfeedDB/index"))

;; -------------------------- Autosave And Lock Files --------------------------
;; don't create lockfiles or autosave (i.e., filename~) files.

(setq  auto-save-default nil)
(setq  create-lockfiles nil)

;; ------------------------------ Backup Settings ------------------------------
;; per-save backups can be invaluable, so create them in ~/.emacs.d/backups

;; BACKUP DIRECTORY CREATION
(defvar cj/backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p cj/backup-directory))
	(make-directory cj/backup-directory t))

;; BACKUP SETTINGS
(setq make-backup-files t)                                    ;; do make backup files
(setq backup-directory-alist `(("." . ,cj/backup-directory))) ;; put all originals in backup directory
(setq backup-by-copying t)                                    ;; don't clobber symlinks
(setq version-control t)                                      ;; make numeric backup versions
(setq delete-old-versions t)                                  ;; delete excess backup files w/o asking
(setq kept-new-versions 25)                                   ;; keep 25  of the newest backups made (default: 2)
(setq vc-make-backup-files t)                                 ;; also backup any files in version control

;; ---------------------------- Exec Path From Shell ---------------------------
;; ensure $PATH is the same between your normal shell and your Emacs shells.

(use-package exec-path-from-shell
  :defer .5
  :config
  (when (daemonp)
	(exec-path-from-shell-initialize)))

;; ------------------------------- GNU Ls On BSD -------------------------------
;; when on BSD use the ls from FSF sysutils/coreutils: pkg install coreutils

(cond ((eq system-type 'berkeley-unix)
		  (setq insert-directory-program "/usr/local/bin/gls")))

(provide 'system-defaults)
;;; system-defaults.el ends here
