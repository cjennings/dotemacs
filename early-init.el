;;; early-init.el --- Startup bootstrap before init.el -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Early startup policy: make init errors loud, speed package/bootstrap work,
;; configure package archives, and suppress expensive UI defaults before the
;; first frame appears.
;;
;; Package archives prefer the checked-in localrepo, then local ELPA mirrors,
;; then online archives. Startup-only GC and file-name-handler changes are
;; paired with later session owners such as gcmh.

;;; Code:

;; ---------------------------- Benchmark Init Setup ---------------------------
;; Comprehensive startup profiling (run M-x benchmark-init/show-durations-tree)
;; To disable profiling, comment out the lines below.
;; Note: Install with M-x package-install RET benchmark-init RET

;; (when (require 'benchmark-init nil 'noerror)
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; -------------------------------- Debug Flags --------------------------------
;; debugging enabled during Emacs startup. disabled again after Emacs startup.

;; uncomment when repo signatures expire and package installation is necessary
;; (setq package-check-signature nil)

(setq debug-on-error t)    ;; default nil. on during startup to catch init errors.

;; Deliberately NOT setting `debug-on-quit' here. Leaving it on wires C-g to
;; the debugger, so the normal "break out of a hang" reflex traps you in a
;; recursive-edit instead of aborting -- and if a startup error fires before
;; the cleanup hook below runs, the flag stays on for the whole session.
;; C-g stays an escape hatch.

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq debug-on-error nil)))

;; ------------------------------ Native Compilation ---------------------------

;; Enable JIT native compilation. Packages are natively compiled on first load
;; (asynchronously, in the background) and cached as .eln for later sessions.
;; This was previously disabled via `(setq native-comp-deferred-compilation
;; nil)' -- the obsolete alias of `native-comp-jit-compilation'. Despite the old
;; comment, setting it nil turns JIT OFF entirely (not "synchronous"), so most
;; modules ran interpreted for the daemon's lifetime and the
;; `native-comp-speed'/jobs settings in system-defaults.el were dead. The old
;; "Selecting deleted buffer" async race that prompted the disable was an Emacs
;; 28/29 issue; this is 30.2.
(setq native-comp-jit-compilation t)

;; Log async-compile warnings to the *Async-native-compile-log* buffer rather
;; than popping a window. (system-defaults.el also routes `comp' display-warnings
;; to a file via `cj/log-comp-warning'.)
(setq native-comp-async-report-warnings-errors 'silent)

;; ------------------------------- Load Freshness ------------------------------
;; Prefer newer .el source over stale .elc byte-compiled files. Without this,
;; Emacs loads the .elc even when the .el has been modified more recently and
;; only emits a warning. Setting this in early-init.el ensures init.el itself
;; also benefits.

(setq load-prefer-newer t)

;; --------------------------- Warning Notifications ---------------------------

;; skip warnings but notify me about errors
(setq warning-minimum-level :error)

;; --------------------------- Use Online Repos Flag ---------------------------
;; set to nil to only use localrepo and local elpa-mirrors (see script directory)

(defgroup cj nil
  "Craig's personal Emacs configuration."
  :group 'convenience
  :prefix "cj/")

(defcustom cj/use-online-repos t
  "Whether to use online package repositories in addition to local repos.
When non-nil, online repos are added but .localrepo has highest priority
(see `cj/package-priority-localrepo' below).  Set to nil to use only
local repos."
  :type 'boolean
  :group 'cj)

;; ---------------------------- Startup Performance ----------------------------
;; Bump the GC threshold and turn off the file-name-handler during startup for
;; speed. The file-name-handler is restored once Emacs has loaded. The GC
;; threshold is deliberately NOT restored here -- `gcmh' (configured in
;; system-defaults.el) owns `gc-cons-threshold' for the rest of the session,
;; keeping it high during activity and collecting on idle. Restoring the stock
;; 800KB here would fight gcmh and bring back frequent GC pauses.

(setq gc-cons-threshold most-positive-fixnum)

(defvar cj/orig-file-name-handler-alist file-name-handler-alist
  "Temporary variable to allow restoration of value post-startup.")
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq file-name-handler-alist cj/orig-file-name-handler-alist)))

;; ------------------------------ Site Start Files -----------------------------
;; don't load site-start or default.el files

(setq inhibit-default-init t)

;; ------------------------------ Package System -------------------------------

(setq package-enable-at-startup nil)
(require 'package) ;; emacs built-in

(defconst user-home-dir (getenv "HOME")
  "The user's home directory per the environment variable.")

(defconst elpa-mirror-location
  (expand-file-name ".elpa-mirrors/" user-home-dir)
  "The path to the elpa mirror location.")

(defconst localrepo-location
  (expand-file-name ".localrepo/" user-emacs-directory)
  "The path to your local Emacs package repository.
For more information about the local Emacs package repository, see comments in
early-init.el.")

(defconst elpa-mirror-gnu-location
  (expand-file-name "gnu/" elpa-mirror-location)
  "The path to the local GNU ELPA mirror.")

(defconst elpa-mirror-nongnu-location
  (expand-file-name "nongnu/" elpa-mirror-location)
  "The path to the local NonGNU ELPA mirror.")

(defconst elpa-mirror-melpa-location
  (expand-file-name "melpa/" elpa-mirror-location)
  "The path to the local MELPA mirror.")

(defconst elpa-mirror-stable-melpa-location
  (expand-file-name "stable-melpa/" elpa-mirror-location)
  "The path to the local MELPA Stable mirror.")

(setq package-archives nil) ;; package-archives will be added below

;; Named priorities for package archives.  Local-first: project-pinned
;; .localrepo wins outright, then the local ELPA mirrors (kept on disk
;; via the elpa-mirror scripts), then the online archives in fallback
;; positions.  Within each tier the order is gnu > nongnu > melpa >
;; melpa-stable, matching the trust ranking we want for resolution.
(defconst cj/package-priority-localrepo 200
  "Priority for the project-pinned .localrepo archive (highest).")
(defconst cj/package-priority-mirror-gnu 125
  "Priority for the local GNU ELPA mirror.")
(defconst cj/package-priority-mirror-nongnu 120
  "Priority for the local NonGNU ELPA mirror.")
(defconst cj/package-priority-mirror-melpa 115
  "Priority for the local MELPA mirror.")
(defconst cj/package-priority-mirror-melpa-stable 100
  "Priority for the local stable MELPA mirror.")
(defconst cj/package-priority-online-gnu 25
  "Priority for the online GNU ELPA archive.")
(defconst cj/package-priority-online-nongnu 20
  "Priority for the online NonGNU ELPA archive.")
(defconst cj/package-priority-online-melpa 15
  "Priority for the online MELPA archive.")
(defconst cj/package-priority-online-melpa-stable 5
  "Priority for the online stable MELPA archive (lowest).")

;; LOCAL REPOSITORY (packages in version control)
(when (file-accessible-directory-p localrepo-location)
  (add-to-list 'package-archives (cons "localrepo" localrepo-location) t)
  (add-to-list 'package-archive-priorities
               (cons "localrepo" cj/package-priority-localrepo)))

;; LOCAL REPOSITORY ELPA MIRRORS
(when (file-accessible-directory-p elpa-mirror-gnu-location)
  (add-to-list 'package-archives (cons "gnu-local" elpa-mirror-gnu-location) t)
  (add-to-list 'package-archive-priorities
               (cons "gnu-local" cj/package-priority-mirror-gnu)))

(when (file-accessible-directory-p elpa-mirror-nongnu-location)
  (add-to-list 'package-archives (cons "nongnu-local" elpa-mirror-nongnu-location) t)
  (add-to-list 'package-archive-priorities
               (cons "nongnu-local" cj/package-priority-mirror-nongnu)))

(when (file-accessible-directory-p elpa-mirror-melpa-location)
  (add-to-list 'package-archives (cons "melpa-local" elpa-mirror-melpa-location) t)
  (add-to-list 'package-archive-priorities
               (cons "melpa-local" cj/package-priority-mirror-melpa)))

(when (file-accessible-directory-p elpa-mirror-stable-melpa-location)
  (add-to-list 'package-archives (cons "melpa-stable-local" elpa-mirror-stable-melpa-location) t)
  (add-to-list 'package-archive-priorities
               (cons "melpa-stable-local" cj/package-priority-mirror-melpa-stable)))

;; ONLINE REPOSITORIES
;; Added regardless of network status. If offline, package operations fail gracefully.
;; .localrepo has highest priority, so reproducible installs work offline.
(when cj/use-online-repos
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archive-priorities
               (cons "gnu" cj/package-priority-online-gnu))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archive-priorities
               (cons "nongnu" cj/package-priority-online-nongnu))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archive-priorities
               (cons "melpa" cj/package-priority-online-melpa))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archive-priorities
               (cons "melpa-stable" cj/package-priority-online-melpa-stable)))

;; Initialize package system
(package-initialize)

;; Package refresh logic - refresh only if:
;; 1. Online repos are enabled
;; 2. Any online repo cache doesn't exist or is older than 7 days
;; If offline, refresh will fail gracefully and use cached/local packages.

(when cj/use-online-repos
  (let ((cache-age-days 7)
		(needs-refresh nil))
	;; Check each online repository's cache
	(dolist (archive '("gnu" "nongnu" "melpa" "melpa-stable"))
	  (let ((cache-file (expand-file-name
						 (format "archives/%s/archive-contents" archive)
						 package-user-dir)))
		(when (or (not (file-exists-p cache-file))
				  (> (/ (float-time (time-subtract
									 (current-time)
									 (file-attribute-modification-time
									  (file-attributes cache-file))))
						86400.0) ;; == 7 days
                     cache-age-days))
          (setq needs-refresh t))))

	;; Only refresh if needed
	(when needs-refresh
	  (condition-case nil
		  (package-refresh-contents)
		(error (message "Failed to refresh package contents"))))))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
	(package-refresh-contents))
  (package-install 'use-package))

;;(require 'use-package-ensure)  ; Needed for :ensure to work
(setq use-package-always-ensure t)  ; Auto-install packages

;; Package signature checking
(setq package-check-signature nil)
;; (setq package-check-signature t)

;; Optional but recommended for better error messages during config loading
;;(setq use-package-expand-minimally nil)  ; Better error reporting
;;(setq use-package-compute-statistics t)  ; Set to t if you want timing info

;; turn on for use-package debugging
;;(setq use-package-verbose nil)

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
(push '(vertical-scroll-bars . nil) default-frame-alist)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default initial-scratch-message nil)
(setq inhibit-startup-echo-area-message (user-login-name))

;; Disable bidirectional text rendering for slight performance boost
(setq-default  bidi-display-reordering nil ;; disable bidi reordering for speed
			   bidi-paragraph-direction 'left-to-right
			   bidi-inhibit-bpa t)         ;; additional speedup

;; Disable global font lock mode until after initialization
(setq-default global-font-lock-mode nil)
(add-hook 'emacs-startup-hook #'global-font-lock-mode)

(provide 'early-init)
;;; early-init.el ends here
