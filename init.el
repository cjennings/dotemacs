;;; init --- Emacs Init File -*- lexical-binding: t -*-
;;  author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Note: This init.el file has an early-init.el file associated with it. That
;; file defines some variables and settings used within the rest of this config.

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

(require 'user-constants)   ;; paths for files referenced in this config
(require 'host-environment) ;; convenience functions re: host environment
(require 'config-utilities) ;; functions useful when modifying Emacs config
(require 'system-defaults)  ;; native comp; log; unicode, backup, exec path
(require 'keybindings)      ;; system-wide keybindings and keybinding discovery

;; -------------------------- Utilities And Libraries --------------------------

(require 'auth-config)      ;; emacs gnupg integration
(require 'custom-functions) ;; custom function library w/ keybindings
(require 'file-config)      ;; files to open outside of Emacs
(require 'keyboard-macros)  ;; keyboard macro management
(require 'system-utils)     ;; timers, process monitor
(require 'text-config)      ;; text settings and functionality
(require 'undead-buffers)   ;; bury rather than kill specific buffers

;; ------------------------------- User Interface ------------------------------

(require 'ui-config)
(require 'ui-theme)
(require 'ui-navigation)
(require 'font-config)

;; --------------------------- Internal Functionality --------------------------

(require 'diff-config)         ;; ediff and ztree configuration
(require 'eshell-vterm-config) ;; shell and terminal configuration
(require 'flyspell-and-abbrev) ;; spell check and auto-correct
(require 'help-utils)          ;; search: arch-wiki, devdoc, tldr, wikipedia
(require 'help-config)         ;; info, man, help config
(require 'latex-config)        ;; need to fix
(require 'modeline-config)     ;; modeline (status-bar) config
(require 'pdf-config)          ;; pdf display settings
(require 'selection-framework) ;; menu config
(require 'tramp-config)        ;; remote shell connections
(require 'show-kill-ring)      ;; utility to display history of kill ring

;; ------------------------- Features And Integrations -------------------------

(require 'ai-config)             ;; LLM integration
(require 'calibredb-epub-config) ;; ebook reader/manager settings
(require 'dashboard-config)      ;; first page on launch
(require 'dirvish-config) ;; file manager configuration
(require 'elfeed-config)
(require 'erc-config)
(require 'eww-config)
(require 'httpd-config)
(require 'local-repository) ;; wip
(require 'mail-config)
(require 'markdown-config)
(require 'record-desktop)
(require 'weather-config) ;; utility to display the weather

;; -------------------------------- Programming --------------------------------

(require 'prog-general)
(require 'prog-comments)
(require 'vc-config)
(require 'treesitter-config)
(require 'flycheck-config)
(require 'prog-lsp)
(require 'prog-training)
(require 'prog-c)
(require 'prog-go)
(require 'prog-lisp)
(require 'prog-shell)       ;; combine elsewhere
(require 'prog-python)
(require 'prog-webdev)
(require 'prog-yaml)

;; ---------------------------------- Org Mode ---------------------------------

(require 'org-config)
(require 'org-agenda-config)
(require 'org-babel-config)
(require 'org-capture-config)
(require 'org-refile-config)
(require 'org-contacts-config)
(require 'org-drill-config)
(require 'org-export-config)
(require 'org-roam-config)

;; ------------------------------- Entertainment -------------------------------

(require 'eradio-config)
(require 'games-config)

;; ------------------------- Personal Workflow Related -------------------------

(require 'reconcile-open-repos)

;; ---------------------------------- Wrap Up ----------------------------------

(require 'test-code)
(require 'wrap-up)

;;; init.el ends here
