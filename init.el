;;; init --- Emacs Init File -*- lexical-binding: t -*-
;;  author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Note: This init.el file has an early-init.el file associated with it. That
;; file defines some variables and settings used within the rest of this config.

;;; Code:

;; -------------------------------- Contact Info -------------------------------

(defvar user-whole-name "Craig Jennings"
  "The user's full name.")
(defconst user-name         (getenv "USER")
  "The user's name retrieved from the environment variable.")
(defvar user-mail-address "c@cjennings.net"
  "The user's email address.")

;; ------------------------- Directory / File Constants ------------------------

(defconst code-dir        (concat user-home-dir "/code/"))
(defconst projects-dir    (concat user-home-dir "/projects/"))
(defconst sync-dir        (concat user-home-dir "/sync/org/"))                     ;; emacs data to be sync'd across machines
(defconst roam-dir        (concat sync-dir "/roam/"))                              ;; emacs org roam data
(defconst snippets-dir    (concat sync-dir "snippets"))                            ;; snippets

(defconst emacs-init-file (concat user-emacs-directory "init.el"))                 ;; 'user-init-file' might be compiled
(defconst emacs-early-init-file (concat user-emacs-directory "early-init.el"))
(defvar schedule-file     (concat sync-dir "schedule.org"))                        ;; scheduled events & appointments
(defvar inbox-file        (concat roam-dir "inbox.org"))                           ;; tasks and task inbox
(defvar contacts-file     (concat sync-dir "contacts.org"))                        ;; org-contacts
(defvar article-file      (concat sync-dir "article-queue.org"))                   ;; articles to read
(defvar article-archive   (concat sync-dir "article-archive.org"))                 ;; articles to keep
(defvar ledger-file       (concat user-home-dir "/projects/finances/main.ledger")) ;; $$$
(defvar macros-file       (concat sync-dir "macros.el"))                           ;; macros with cj/save-macro are saved here

;; ---------------------------- System Configuration ---------------------------

(add-to-list 'load-path (concat user-emacs-directory "modules"))
(add-to-list 'load-path (concat user-emacs-directory "assets"))
(add-to-list 'load-path (concat user-emacs-directory "custom"))

(require 'config-utilities)  ;; functions useful when modifying Emacs config
(require 'host-environment)  ;; convenience functions re: host environment
(require 'system-defaults)
(require 'keybindings)

;; -------------------------- Utilities And Libraries --------------------------

(require 'custom-functions) ;; custom function library w/ keybindings
(require 'system-utils)
(require 'epa-config)       ;; emacs gnupg integration
(require 'text-config)      ;; text settings and functionality

;; ------------------------------- User Interface ------------------------------

(require 'ui-config)
(require 'ui-theme)
(require 'ui-navigation)
(require 'font-config)
(require 'selection-framework)

;; ------------------------------- Functionality -------------------------------

(require 'ai-config)
(require 'calibredb-epub-config)
(require 'dashboard-config)
(require 'diff-config)
(require 'dirvish-config)
(require 'elfeed-config)
(require 'erc-config)
(require 'eshell-vterm-config)
(require 'eww-config)
(require 'flyspell-config)
(require 'graphviz-config)           ;; merge with latex module?
(require 'help-utils)
(require 'httpd-config)
(require 'ledger-config)
(require 'local-repository)          ;; wip
(require 'mail-config)
(require 'markdown-config)
(require 'modeline-config)
(require 'pdf-config)
(require 'show-kill-ring)
(require 'telegram-config)
(require 'tramp-config)
;; (require 'latex-config)           ;; need to fix

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
(require 'prog-shell) ;; combine elsewhere
(require 'prog-python)
(require 'prog-webdev)
(require 'prog-yaml)

;; ---------------------------------- Org Mode ---------------------------------

(require 'org-config)
(require 'org-agenda-config)
(require 'org-appearance-config)
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
