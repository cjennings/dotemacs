﻿;;; early-init.el --- Emacs Early Init -*- lexical-binding: t; -*-

;;; Commentary:

;; DEBUG FLAGS
;; Debug flags are default on while this config is loading since errors should
;; be loud and highly noticeable. They are restored to their default off once
;; the config has completed.

;; STARTUP PERFORMANCE
;; Increasing garbage collection to a very high number decreases startup time.
;; setting the file-name-handler and vc-handled-backends avoids some regexp
;; slowness during startup. All original values are restored once Emacs is
;; finished with startup.

;; LOCAL REPOSITORIES
;; This config doesn't work if the packages it relies on fail. Having local
;; package repositories also allows for full config portability behind corporate
;; firewalls and fast recovery from package issues no matter the network
;; situation.

;; The localrepo directory contains all the last known good packages for this
;; config. The directory is added as a repository to the package archive list
;; first, and given the highest priority number. This allows for a portable
;; installation and reinstallation. This directory averages ~70 MB.

;; Having a full local mirror of all elpa, melpa, and org repositories gives you
;; more flexibility but at a higher storage cost. The script
;; 'create-elpa-mirror.sh in user-emacs-directory/scripts directory will clone
;; them all locally. As of Saturday, March 30, 2024 the directory containing all
;; gnu, nongnu, melpa, melpa-stable, and org packages takes around 1.9 GB.
;; For more information on the localrepo and elpa mirrors, read the commentary
;; in local-repository.el.

;;; Code:

;; -------------------------------- Debug Flags --------------------------------
;; debugging enabled during Emacs startup. disabled again after Emacs startup.


;; uncomment when repo signatures expire and package installation is necessary
;; (setq package-check-signature nil)

(setq debug-on-error t)    ;; default nil. turn on to debug issues only.
(setq debug-on-quit t)     ;; debug on C-g (breaking out of hangs/freezes)

(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq debug-on-error nil)
			(setq debug-on-quit nil)))

;; --------------------------- Warning Notifications ---------------------------

;; log warnings, but don't popup the warnings buffer
(setq native-comp-async-report-warnings-errors 'silent)

;; skip warnings but notify me about errors
(setq warning-minimum-level :error)

;; --------------------------- Use Online Repos Flag ---------------------------
;; set to nil to only use localrepo and local elpa-mirrors (see script directory)

(defvar cj/use-online-repos t
  "Whether to check for network connectivity & use online package repositories.")

;; ---------------------------- Startup Performance ----------------------------
;; increases garbage collection threshold, and turns off file-name-handler and
;; vc-backends during startup and restores the settings once emacs has loaded.

(defvar cj/orig-gc-cons-threshold gc-cons-threshold
  "Temporary variable to allow restoration of value post-startup.")
(setq gc-cons-threshold most-positive-fixnum)

(defvar cj/orig-file-name-handler-alist file-name-handler-alist
  "Temporary variable to allow restoration of value post-startup.")
(setq file-name-handler-alist nil)

(defvar cj/orig-vc-handled-backends vc-handled-backends
  "Temporary variable to allow restoration of value post-startup.")
(setq vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold cj/orig-gc-cons-threshold
                  file-name-handler-alist cj/orig-file-name-handler-alist
                  vc-handled-backends cj/orig-vc-handled-backends)))

;; ------------------------------ Site Start Files -----------------------------
;; don't load site-start or default.el files

(setq inhibit-default-init t)

;; ------------------------------- Network Check -------------------------------
;; checks if the network is available. used for online repo enablement.

(defun internet-up-p (&optional host)
  "Test for network connectivity by pinging HOST."
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))

;; ----------------------------- Package Management ----------------------------
;; detect the availability of online and local repositories before adding them.
;; the order and priority is localrepo, local mirrors, then online repositories.

(require 'package) ;; emacs built-in

(defconst user-home-dir (getenv "HOME")
  "The user's home directory per the environment variable.")

(defconst elpa-mirror-location "/media/backup/repositories/elpa-mirror/"
  "The path to the elpa mirror location.")

(defconst localrepo-location (concat user-emacs-directory ".localrepo/")
  "The path to your local Emacs package repository.
For more information about the local Emacs package repository, see comments in
early-init.el.")

(setq package-archives nil) ;; package-archives will be added below

;; LOCAL REPOSITORY (packages in version control)
(if (file-accessible-directory-p localrepo-location)
    (progn
      (add-to-list 'package-archives (cons "localrepo" localrepo-location) t)
      (add-to-list 'package-archive-priorities '(("localrepo". 200)))))

;; LOCAL REPOSITORY ELPA MIRRORS
(if (file-accessible-directory-p (concat elpa-mirror-location "gnu"))
    (progn
      (add-to-list 'package-archives (cons "gnu-local" (concat elpa-mirror-location "gnu/"))t)
      (add-to-list 'package-archive-priorities '("gnu-local" . 125))))

(if (file-accessible-directory-p (concat elpa-mirror-location "nongnu"))
    (progn
      (add-to-list 'package-archives (cons "nongnu-local" (concat elpa-mirror-location "nongnu/"))t)
      (add-to-list 'package-archive-priorities '("nongnu-local" . 120))))

(if (file-accessible-directory-p (concat elpa-mirror-location "melpa"))
    (progn
      (add-to-list 'package-archives (cons "melpa-local" (concat elpa-mirror-location "melpa/"))t)
      (add-to-list 'package-archive-priorities '("melpa-local" . 115))))

(if (file-accessible-directory-p (concat elpa-mirror-location "stable-melpa"))
    (progn
      (add-to-list 'package-archives (cons "melpa-stable-local" (concat elpa-mirror-location "stable-melpa/"))t)
      (add-to-list 'package-archive-priorities '("melpa-stable" . 100))))

;; ONLINE REPOSITORIES
(when (and (boundp 'cj/use-online-repos) cj/use-online-repos (internet-up-p))
  (progn
	(add-to-list 'package-archives '("gnu". "https://elpa.gnu.org/packages/") t)
	(add-to-list 'package-archive-priorities '("gnu" . 25))
	(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
	(add-to-list 'package-archive-priorities '("nongnu" . 20))
	(add-to-list 'package-archives '("melpa". "https://melpa.org/packages/") t)
	(add-to-list 'package-archive-priorities '("melpa" . 15))
	;; (add-to-list 'package-archives '("org". "https://orgmode.org/packages/") t)
	;; (add-to-list 'package-archive-priorities '("org" . 10))
	(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
	(add-to-list 'package-archive-priorities '("melpa-stable" . 5))))
(package-initialize)

;; only run refresh when there's no cache
(when (not package-archive-contents)
  (package-refresh-contents))

;; always ensure package installation
(setq use-package-always-ensure t)

;; ---------------------------------- Unicode ----------------------------------
;; unicode all the things

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

;; ---------------------------- Inhibit UI Elements ----------------------------
;; setting UI preferences here before the UI is displayed

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default initial-scratch-message nil)
(setq inhibit-startup-echo-area-message t)

(provide 'early-init)
;;; early-init.el ends here.
