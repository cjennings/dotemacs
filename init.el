;;; init.el --- Emacs Init File -*- lexical-binding: t; coding: utf-8; -*-
;;  author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Note: This init.el file has an early-init.el file associated with it.
;; That file defines a few variables and settings used within the rest of the config.

;;; Code:

;; -------------------------------- Contact Info -------------------------------

(defvar user-whole-name "Craig Jennings"
  "The user's full name.")
(defconst user-name (getenv "USER")
  "The user's name retrieved from the environment variable.")
(defvar user-mail-address "c@cjennings.net"
  "The user's email address.")

;; ---------------------------- System Configuration ---------------------------

(add-to-list 'load-path (concat user-emacs-directory "assets/"))
(add-to-list 'load-path (concat user-emacs-directory "custom/"))
(add-to-list 'load-path (concat user-emacs-directory "modules/"))

(require 'user-constants)        ;; paths for files referenced in this config
(require 'host-environment)      ;; convenience functions re: host environment
(require 'config-utilities)      ;; functions useful when modifying Emacs config
(require 'system-defaults)       ;; native comp; log; unicode, backup, exec path
(require 'keybindings)           ;; system-wide keybindings and keybinding discovery

;; -------------------------- Utilities And Libraries --------------------------

(require 'auth-config)           ;; emacs gnupg integration
(require 'custom-functions)      ;; custom function library w/ keybindings
(require 'chrono-tools)          ;;	calendar, world clock, timers
(require 'file-config)           ;; files to open outside of Emacs
(require 'keyboard-macros)       ;; keyboard macro management
(require 'system-utils)          ;; timers, process monitor
(require 'text-config)           ;; text settings and functionality
(require 'undead-buffers)        ;; bury rather than kill specific buffers

;; ------------------------------- User Interface ------------------------------

(require 'ui-config)             ;; transparency, cursor color, icons, &c.
(require 'ui-theme)              ;; themes and theme persistency
(require 'ui-navigation)         ;; the movement and navigation of windows
(require 'font-config)           ;; font and emoji configuration

;; --------------------------- Internal Functionality --------------------------

(require 'diff-config)           ;; diff and merge functionality w/in Emacs
(require 'eshell-vterm-config)   ;; shell and terminal configuration
(require 'flyspell-and-abbrev)   ;; spell check and auto-correct
(require 'help-utils)            ;; search: arch-wiki, devdoc, tldr, wikipedia
(require 'help-config)           ;; info, man, help config
(require 'latex-config)          ;; WIP need to fix
(require 'modeline-config)       ;; modeline (status-bar) config
(require 'pdf-config)            ;; pdf display settings
(require 'selection-framework)   ;; menu config
(require 'tramp-config)          ;; remote shell connections
(require 'show-kill-ring)        ;; displays and facilitates pasting from history

;; ------------------------- Features And Integrations -------------------------

(require 'calibredb-epub-config) ;; ebook reader/manager settings
(require 'dashboard-config)      ;; the nice landing page with links
(require 'dirvish-config)        ;; file manager configuration
(require 'dwim-shell-config)     ;; shell commands brought to dirvish/dired
(require 'elfeed-config)         ;; feed reader and podcast player/downloader
(require 'erc-config)            ;; seamless IRC client
(require 'eww-config)            ;; text mode web browsing, w/o javascript
(require 'httpd-config)
(require 'mail-config)           ;; email using mu4e and org-msg
(require 'markdown-config)
(require 'weather-config)        ;; utility to display the weather

;; -------------------------------- Programming --------------------------------

(require 'prog-general)          ;; general programming functionality/settings
(require 'vc-config)             ;; version control packages and keybindings
(require 'flycheck-config)       ;; linting for all languages including human ones
(require 'prog-lsp)
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
(require 'org-agenda-config)     ;; agenda and task tracking
(require 'org-babel-config)      ;; org-mode prog blocks; literate programming
(require 'org-capture-config)
(require 'org-refile-config)     ;; refile org-branches
(require 'org-drill-config)
(require 'org-export-config)
(require 'org-roam-config)       ;; personal knowledge management in org mode
(require 'org-contacts-config)   ;; fully integrated org-mode contacts management

;; ------------------------------- Ai Integration ------------------------------

(require 'ai-config)             ;; LLM integration. note: ai-directives.el

;; ------------------------- Personal Workflow Related -------------------------

(require 'reconcile-open-repos)
(require 'video-audio-recording)
(require 'local-repository)

;; ------------------------------- Entertainment -------------------------------

(require 'eradio-config)
(require 'games-config)

;; ---------------------------------- Wrap Up ----------------------------------

(require 'wrap-up)

;; ---------------------------------- In Test ----------------------------------

(require 'wip)
(require 'test-runner)
;; (require 'jumper)

;;; init.el ends here
