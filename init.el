;;; init.el --- Emacs Init File -*- lexical-binding: t; coding: utf-8; -*-
;;  author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Note: This init.el file has an early-init.el file associated with it.
;; That file defines a few variables and settings used within the rest of the config.
;; Emacs-Lisp code in this config requires version 29+.

;;; Code:

;; Enable use-package statistics for profiling (run M-x use-package-report)
;; (setq use-package-compute-statistics t)

(add-to-list 'load-path (concat user-emacs-directory "assets/"))
(add-to-list 'load-path (concat user-emacs-directory "custom/"))
(add-to-list 'load-path (concat user-emacs-directory "modules/"))

;; ---------------------------- System Configuration ---------------------------

(require 'config-utilities)      ;; enable for extra Emacs config debug helpers
(require 'user-constants)        ;; paths for files referenced in this config
(require 'host-environment)      ;; convenience functions re: host environment
(require 'system-defaults)       ;; native comp; log; unicode, backup, exec path
(require 'keybindings)           ;; system-wide keybindings and keybinding discovery

;; -------------------------- Utilities And Libraries --------------------------

(require 'custom-case)           ;; operations for upper/lower/title case
(require 'custom-comments)       ;; operations with comments                       (tests done)
(require 'custom-datetime)       ;; date/timestamp insertion in various formats    (too trivial)
(require 'custom-file-buffer)    ;; custom buffer and file operations and keymap   (tests done)
(require 'custom-line-paragraph) ;; operations on lines and paragraphs             (tests done)
(require 'custom-misc)           ;; miscellaneous functions                        (tests done)
(require 'custom-ordering)       ;; ordering and sorting operations                (tests done)
(require 'custom-text-enclose)   ;; operations to append, prepend, and surround text (tests done)
(require 'custom-whitespace)     ;; whitespace operations                          (tests done)
(require 'external-open)         ;; files to open outside of Emacs
(require 'media-utils)           ;; download and play urls

;; ------------------------- System Level Functionality ------------------------

(require 'auth-config)           ;; emacs gnupg integration
(require 'keyboard-macros)       ;; keyboard macro management                     (tests done)
(require 'system-utils)          ;; timers, process monitor
(require 'text-config)           ;; text settings and functionality
(require 'undead-buffers)        ;; bury rather than kill buffers you choose      (tests done)

;; ------------------------ User Interface Configuration -----------------------

(require 'ui-config)             ;; transparency, cursor color, icons, &c.
(require 'ui-theme)              ;; themes and theme persistency
(require 'ui-navigation)         ;; the movement and navigation of windows
(require 'font-config)           ;; font and emoji configuration
(require 'selection-framework)   ;; menu config
(require 'modeline-config)       ;; modeline (status-bar) config
(require 'mousetrap-mode)        ;; disables trackpad/mouse input only in Emacs
(require 'popper-config)         ;; moving logs, help, and other buffers to popup

;; ----------------- Emacs Built-In Functionality Configuration ----------------

(require 'chrono-tools)          ;; calendar, world clock, timers
(require 'diff-config)           ;; diff and merge functionality w/in Emacs
(require 'erc-config)            ;; seamless IRC client
(require 'eshell-vterm-config)   ;; shell and terminal configuration
(require 'help-utils)            ;; search: arch-wiki, devdoc, tldr, wikipedia
(require 'help-config)           ;; info, man, help config
(require 'tramp-config)          ;; remote shell connections

;; ---------------------- Added Features And Integrations ----------------------

(require 'calibredb-epub-config) ;; ebook reader/manager settings
(require 'dashboard-config)      ;; the nice landing page with links
(require 'dirvish-config)        ;; file manager configuration
(require 'dwim-shell-config)     ;; shell commands brought to dirvish/dired
(require 'elfeed-config)         ;; feed reader and podcast player/downloader
(require 'eww-config)            ;; text mode web browsing, w/o javascript
(require 'flyspell-and-abbrev)   ;; spell check and auto-correct
(require 'httpd-config)
(require 'latex-config)          ;; WIP need to fix
(require 'mail-config)           ;; email using mu4e and org-msg
(require 'markdown-config)
(require 'pdf-config)            ;; pdf display settings
(require 'quick-video-capture)   ;; desktop and/or audio recording via ffmpeg
;; (require 'show-kill-ring)        ;; displays and facilitates pasting from history
(require 'video-audio-recording) ;; desktop and/or audio recording via ffmpeg
(require 'weather-config)        ;; utility to display the weather

;; -------------------------------- Programming --------------------------------

(require 'prog-general)          ;; general programming functionality/settings
(require 'test-runner)
(require 'vc-config)             ;; version control packages and keybindings
(require 'flycheck-config)       ;; linting for all languages including human ones
(require 'prog-training)
(require 'prog-c)
(require 'prog-go)
(require 'prog-lisp)
(require 'prog-shell)            ;; combine elsewhere
(require 'prog-python)
(require 'prog-webdev)
(require 'prog-yaml)

;; ---------------------------------- Org Mode ---------------------------------

(require 'org-config)            ;; basic org-mode settings
(require 'org-agenda-config)     ;; agenda, task tracking, and notifications
(require 'org-babel-config)      ;; org-mode prog blocks; literate programming
(require 'org-capture-config)
(require 'org-contacts-config)   ;; fully integrated org-mode contacts management
(require 'org-drill-config)
(require 'org-export-config)
(require 'org-gcal-config)
(require 'org-refile-config)     ;; refile org-branches
(require 'org-roam-config)       ;; personal knowledge management in org mode     (tests added)
(require 'org-webclipper)        ;; "instapaper" to org-roam workflow
;; (require 'org-noter-config) ;; wip

;; -------------------------- AI Integration And Tools -------------------------

(require 'ai-config)            ;; LLM integration with GPTel and friends

(with-eval-after-load 'gptel
  (add-to-list 'load-path "~/.emacs.d/gptel-tools")
  ;; Buffer Tools
  (require 'read_buffer)
  ;; Filesystem Tools
  (require 'read_text_file)
  (require 'write_text_file)
  ;;  (require 'update_text_file) ;; BUG: issues with this tool
  (require 'list_directory_files)
  (require 'move_to_trash))

;; ------------------------- Personal Workflow Related -------------------------

(require 'reconcile-open-repos)
(require 'local-repository)

;; ------------------------------- Entertainment -------------------------------

(require 'music-config)
(require 'games-config)

;; ------------------------------ Modules In Test ------------------------------
(require 'browser-config)
;;(require 'wip)
(require 'lorem-optimum)
(require 'jumper)

;; ---------------------------------- Wrap Up ----------------------------------

(require 'wrap-up)

;;; init.el ends here
