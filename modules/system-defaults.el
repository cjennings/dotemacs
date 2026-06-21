;;; system-defaults --- Non-UI Preferences -*- lexical-binding: t; coding: utf-8-unix; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 1 (Foundation).
;; Category: F/S.
;; Load shape: eager.
;; Eager reason: establishes core Emacs behavior (encoding, clipboard,
;;   scrolling, kill/quit defaults) the first interactive session relies on.
;; Top-level side effects: mutates global defaults (many `setq'), advises
;;   `display-warning', adds a `display-buffer-alist' entry, remaps one global
;;   key.
;; Runtime requires: autorevert, server, bookmark, host-environment
;;   (`env-bsd-p'), user-constants (`user-home-dir').
;; Direct test load: yes.
;;
;; Loads during init to set sane defaults: UTF-8 everywhere, quiet prompts, synced clipboards,
;; and hands-off async shell buffers. Nothing to call—just launch Emacs and the environment is ready.
;; Native compilation is tuned for performance and its warnings get logged to comp-warnings.log.

;;; Implementation Notes:
;; `cj/log-comp-warning` advices `display-warning` so native-comp notices land in the log instead of popping Messages.
;; Remove the advice if you need stock warning buffers for debugging.

;;; Code:

(require 'autorevert)
(require 'server)
(require 'bookmark)

;; host-environment (`env-bsd-p') and user-constants (`user-home-dir') are read
;; at load time below, so require them at runtime, not only at compile time.
;; init.el already loads them earlier; the explicit requires let this module
;; load standalone.
(require 'host-environment)
(require 'user-constants)

;; -------------------------- Native Comp Preferences --------------------------

(with-eval-after-load 'comp-run
  (setopt native-comp-async-jobs-number 8) ; parallel compile workers
  (setopt native-comp-speed 3)             ; highest optimization level
  (setopt native-comp-always-compile t))   ; always native-compile

;; -------------------------- Log Native Comp Warnings -------------------------

(defvar comp-warnings-log
  (expand-file-name "comp-warnings.log" user-emacs-directory)
  "File where native-comp warnings will be appended.")

(defun cj/log-comp-warning (type message &rest args)
  "Log native-comp warnings of TYPE with MESSAGE & ARGS.
Log to buffer `comp-warnings-log'. Suppress warnings from appearing in the
*Warnings* buffer. If TYPE contains `comp', log the warning with a
timestamp to the file specified by `comp-warnings-log'. Return non-nil to
indicate the warning was handled."
  (when (memq 'comp (if (listp type) type (list type)))
    (with-temp-buffer
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (if (stringp message)
                  (apply #'format message args)
                (format "%S %S" message args)))
      (insert "\n")
      (append-to-file (point-min) (point-max) comp-warnings-log))
    ;; Return non-nil to tell `display-warning' “we handled it.”
    t))

(advice-add 'display-warning :before-until #'cj/log-comp-warning)

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
  "Do absolutely nothing and do it quickly.
Used to disable functionality with defalias \='somefunc \='cj/disabled)."
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
;; Add accidental customizations via the customization interface to a temp file that's never read.
;; Guarded so a batch module load (make validate-modules, byte-compile) doesn't
;; create a throwaway temp file on every run.
(unless noninteractive
  (setq custom-file (make-temp-file
                     "emacs-customizations-trashbin-")))

(defun cj/--warn-customize-discarded (&rest _)
  "Warn once that Customize edits land in a throwaway `custom-file'.
`custom-file' is a temp file that is never read back, so anything saved
through the Customize interface is lost on exit.  This advice fires the
first time `custom-save-all' writes, then removes itself so the warning
appears only once per session."
  (advice-remove 'custom-save-all #'cj/--warn-customize-discarded)
  (display-warning
   'cj/system-defaults
   (concat "Customize edits are discarded: `custom-file' is a throwaway "
           "temp file that is never loaded back. Edit configuration in the "
           "Elisp init files instead.")
   :warning))

(advice-add 'custom-save-all :before #'cj/--warn-customize-discarded)

;; ------------------------- Re-Enabling Functionality -------------------------

(put 'narrow-to-region 'disabled nil)               ;; narrow-to-region is extremely useful!
(put 'upcase-region 'disabled nil)                  ;; upcase region is useful
(put 'erase-buffer 'disabled nil)                   ;; and so is erase-buffer

;; ------------------------------ Non UI Settings ------------------------------

(setq ring-bell-function 'ignore)                   ;; disable the bell ring.
(setq default-directory user-home-dir)              ;; consider user home the default directory

(global-auto-revert-mode)                           ;; update the buffer when the associated file has changed
(setq global-auto-revert-non-file-buffers t)        ;; do so for all buffer types (e.g., ibuffer)

;; -------------------------------- Emacs Server -------------------------------
;; Start server so emacsclient can connect (needed for pinentry-emacs in terminal)

;; noninteractive guard: a raw module load under --batch (make validate-modules
;; on a machine with no daemon socket) would otherwise start a server.
(unless (or noninteractive (daemonp) (server-running-p))
  (server-start))

(setq system-time-locale "C")                       ;; use en_US locale to format time.

;; --------------------------------- Clipboard ---------------------------------

(setq select-enable-clipboard t)                    ;; cut and paste using clipboard
(setq yank-pop-change-selection t)                  ;; update system clipboard when yanking in emacs
(setq save-interprogram-paste-before-kill t)        ;; save existing clipboard to kill ring before replacing

;; Additional settings for better clipboard integration
(setq select-enable-primary nil)                    ;; don't use X11 primary selection (no middle-click paste)
(setq mouse-drag-copy-region nil)                   ;; don't copy region to clipboard by selecting with mouse

;; -------------------------------- Tab Settings -------------------------------

(setq-default tab-width 4)                          ;; if tab, make them 4 spaces default
(setq-default indent-tabs-mode nil)                 ;; but turn off tabs by default

;; ------------------------------ Scroll Settings ------------------------------

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling

;; ----------------------------- Case Insensitivity ----------------------------

(setq case-fold-search t)                           ;; case-insensitive searches
(setq completion-ignore-case t)                     ;; case-insensitive completion
(setq read-file-name-completion-ignore-case t)      ;; case-insensitive file completion

;; ------------------------------- Async Commands ------------------------------

(setq async-shell-command-buffer 'new-buffer)

;; never automatically display async command output buffers
;; but keep them in the buffer list for later inspection
(add-to-list 'display-buffer-alist
             '("*Async Shell Command*" display-buffer-no-window (nil)))

;; ------------------------ Mouse And Trackpad Settings ------------------------

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-margin 3)             ;; start scrolling at 3 lines from top/bottom
(setq scroll-step 1)               ;; keyboard scroll one line at a time

;; disable pasting with mouse-wheel click
(global-unset-key (kbd "<mouse-2>"))

;; disable pinching gesture or mouse-wheel changing font size
(global-unset-key (kbd "<pinch>"))
(keymap-global-set "<remap> <mouse-wheel-text-scale>" #'cj/disabled)

;; ------------------------------- Be Quiet(er)! -------------------------------

(setq-default vc-follow-symlinks t)           ;; follow version-controlled symlinks without asking
(setq kill-buffer-query-functions             ;; don't ask about killing buffers with processes, just kill them
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq confirm-kill-processes nil)             ;; automatically kill running processes on exit
(setq confirm-nonexistent-file-or-buffer nil) ;; don't ask if a file I visit with C-x C-f or C-x b doesn't exist
(setq ad-redefinition-action 'accept)         ;; silence warnings about advised functions getting redefined.
(setq large-file-warning-threshold nil)       ;; open files regardless of size
(setq use-short-answers t)                    ;; single-key y/n for ordinary yes-or-no-p prompts
                                              ;; (irreversible actions use `cj/confirm-strong', which
                                              ;; forces a typed "yes" by binding this nil for that call)
(setq auto-revert-verbose nil)                ;; turn off auto revert messages
(setq custom-safe-themes t)                   ;; treat all themes as safe (stop asking)
(setq server-client-instructions nil)         ;; I already know what to do when done with the frame

;; ----------------------------- Garbage Collection ----------------------------
;; GC is managed by gcmh in modules/gcmh-config.el: it keeps gc-cons-threshold
;; high during activity and collects on idle, replacing the old stock-800KB
;; scheme (an early-init restore plus a minibuffer setup/exit bump). gcmh lives
;; in its own module rather than here because system-defaults.el is pre-loaded
;; by the comp-errors test harness, which has no package system -- an `:ensure'
;; package loaded here would error at load time and break those tests.

;; ----------------------------- Bookmark Settings -----------------------------

;; place bookmark file sync'd org files
(setq bookmark-default-file (concat org-dir "emacs_bookmarks"))

;; save bookmarks each (1) time it's modified.
(setq bookmark-save-flag 1)

;; -------------------------------- Recent Files -------------------------------

(use-package recentf
  :init
  (recentf-mode 1)
  :ensure nil ;;built-in
  :config
  (setq recentf-max-saved-items 1000)
  (setq recentf-max-menu-items 50)
  (add-to-list 'recentf-exclude "emacs_bookmarks")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/elpa")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/recentf")
  (add-to-list 'recentf-exclude "\\ElfeedDB/index")
  (add-to-list 'recentf-exclude "airootfs"))

;; -------------------------- Autosave And Lock Files --------------------------

(setq  auto-save-default nil)
(setq  create-lockfiles nil)

;; ------------------------------ Backup Settings ------------------------------

;; Backup Directory Creation
(defvar cj/backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p cj/backup-directory))
    (make-directory cj/backup-directory t))

;; Backup Settings
(setq make-backup-files t)                                    ;; do make backup files
(setq backup-directory-alist `(("." . ,cj/backup-directory))) ;; put all originals in backup directory
(setq backup-by-copying t)                                    ;; don't clobber symlinks
(setq version-control t)                                      ;; make numeric backup versions
(setq delete-old-versions t)                                  ;; delete excess backup files w/o asking
(setq kept-new-versions 25)                                   ;; keep 25  of the newest backups made (default: 2)
(setq vc-make-backup-files t)                                 ;; also backup any files in version control

;; ------------------ Unpropertize Kill Ring For Performance -----------------

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;; ------------------------------- GNU 'ls' On BSD -------------------------------

(when (env-bsd-p)
  (setq insert-directory-program "/usr/local/bin/gls"))

(provide 'system-defaults)
;;; system-defaults.el ends here
