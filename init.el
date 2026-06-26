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

(require 'system-lib)            ;; low-level system utility functions
(require 'config-utilities)      ;; enable for extra Emacs config debug helpers
(require 'user-constants)        ;; paths for files referenced in this config
(unless noninteractive
  (cj/initialize-user-directories-and-files)) ;; create configured dirs/files on real startup
(require 'host-environment)      ;; convenience functions re: host environment
(require 'keyboard-compat)       ;; terminal/GUI keyboard compatibility
(require 'system-defaults)       ;; native comp; log; unicode, backup, exec path
(require 'gcmh-config)           ;; garbage collection strategy (gcmh)
(require 'keybindings)           ;; system-wide keybindings and keybinding discovery

;; -------------------------- Utilities And Libraries --------------------------

(require 'custom-case)           ;; operations for upper/lower/title case
(require 'custom-comments)       ;; operations with comments
(require 'custom-datetime)       ;; date/timestamp insertion in various formats
(require 'custom-buffer-file)    ;; custom buffer and file operations and keymap
(require 'custom-line-paragraph) ;; operations on lines and paragraphs
(require 'custom-misc)           ;; miscellaneous functions
(require 'custom-ordering)       ;; ordering and sorting operations
(require 'custom-text-enclose)   ;; operations to append, prepend, and surround text
(require 'custom-whitespace)     ;; whitespace operations
(require 'external-open)         ;; files to open outside of Emacs
(require 'media-utils)           ;; download and play urls

;; ------------------------- System Level Functionality ------------------------

(require 'auth-config)           ;; emacs gnupg integration
(require 'keyboard-macros)       ;; keyboard macro management
(require 'system-utils)          ;; timers, process monitor
(require 'text-config)           ;; text settings and functionality
(require 'undead-buffers)        ;; bury rather than kill buffers you choose
(require 'browser-config)        ;; browser configuration/integration
(require 'coverage-core)         ;; diff-aware coverage engine + F7 binding
(require 'coverage-elisp)        ;; elisp backend for coverage-core
(require 'dev-fkeys)             ;; F4 compile+run dispatcher, F6 tests stopgap

;; ------------------------ User Interface Configuration -----------------------

(require 'ui-config)             ;; transparency, cursor color, icons, &c.
(require 'ui-theme)              ;; themes and theme persistency
(cj/load-theme-from-file)
(require 'auto-dim-config)       ;; dim non-selected windows (faces live in the theme)
(require 'ui-navigation)         ;; the movement and navigation of windows
(require 'font-config)           ;; font and emoji configuration
(require 'nerd-icons-config)     ;; nerd-icons + completion/ibuffer integration + tint
(require 'selection-framework)   ;; menu config
(require 'modeline-config)       ;; modeline (status-bar) config
(require 'mousetrap-mode)        ;; prevent accidental mouse/trackpad modifications

;; ----------------- Emacs Built-In Functionality Configuration ----------------

(require 'chrono-tools)          ;; calendar, world clock, timers
(require 'diff-config)           ;; diff and merge functionality w/in Emacs
(require 'erc-config)            ;; seamless IRC client
(require 'slack-config)          ;; slack client via emacs-slack
(require 'pearl-config)          ;; Linear.app issue tracking via pearl (deepsat + craigjennings)
(require 'telega-config)         ;; telegram client via telega.el (TDLib in docker)
(require 'signal-config)         ;; signal client via forked signel + signal-cli
(require 'eshell-config)         ;; emacs shell configuration
(require 'eat-config)            ;; EAT terminal + the F12 dock-and-remember toggle
(require 'ai-term)               ;; in-Emacs Claude launcher (vertical-split EAT terminal)
(require 'help-utils)            ;; search: arch-wiki, devdoc, tldr, wikipedia
(require 'help-config)           ;; info, man, help config
(require 'face-diagnostic)       ;; describe face/font at point (cj/describe-face-at-point)
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
(require 'latex-config)          ;; LaTeX/AUCTeX editing configuration
(require 'mail-config)           ;; email using mu4e and org-msg
(require 'markdown-config)
(require 'pdf-config)            ;; pdf display settings
(require 'quick-video-capture)   ;; download videos with a browser bookmark
(require 'video-audio-recording) ;; desktop and/or audio recording via ffmpeg
(require 'transcription-config)  ;; audio transcription
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
(require 'prog-shell)            ;; shell-script editing configuration
(require 'prog-python)
(require 'prog-webdev)
(require 'prog-json)
(require 'prog-yaml)
(require 'ledger-config)         ;; plain-text accounting (ledger format)

;; ---------------------------------- Org Mode ---------------------------------

(require 'org-config)            ;; basic org-mode settings
(require 'org-faces-config)      ;; custom themeable faces for agenda keywords + priorities
(require 'org-agenda-config)     ;; agenda, task tracking, and notifications
(require 'org-babel-config)      ;; org-mode prog blocks; literate programming
(require 'org-capture-config)
(require 'org-contacts-config)   ;; fully integrated org-mode contacts management
(require 'org-drill-config)
(require 'org-export-config)
(require 'hugo-config)          ;; ox-hugo blog workflow (C-; h)
(require 'org-reveal-config)    ;; reveal.js presentations (C-; p)

(require 'org-refile-config)     ;; refile org-branches
(require 'org-roam-config)       ;; personal knowledge management in org mode
(require 'org-webclipper)        ;; "instapaper" to org-roam workflow
(require 'org-noter-config)

;; -------------------------- AI Integration And Tools -------------------------

(require 'restclient-config)    ;; REST API client for API exploration

;; ------------------------- Personal Workflow Related -------------------------

(require 'calendar-sync)        ;; sync calendars, must come after org-agenda
(require 'google-keep-config)   ;; google keep notes as a read-only org page
(require 'reconcile-open-repos) ;; review dirty repositories and reconcile
(require 'local-repository)     ;; local repository for easy config portability

;; ------------------------------- Entertainment -------------------------------

(require 'music-config)
;; games-config: deferred (load-graph Phase 4).  malyon / 2048-game autoload
;; their own commands via package.el; games-config only supplies malyon's config,
;; so load it when malyon loads rather than requiring it at startup.
(with-eval-after-load 'malyon (require 'games-config))

;; ------------------------------- Misc Modules --------------------------------

(require 'lorem-optimum) ;; best fake latin text generator ever
(require 'jumper)  ;; navigation help for large projects/lotsa buffers
(require 'system-commands) ;; reboot, logout, etc.
(require 'gloss-config)  ;; personal glossary on C-h g; v1 in shakedown

;; ---------------------------------- Wrap Up ----------------------------------

(require 'wrap-up)

;;; init.el ends here
