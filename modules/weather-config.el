;;; weather-config.el ---  -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; optional weather command, a command-loaded deferral
;;   candidate. Degrades cleanly when wttrin is absent.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;; Call M-W to open wttrin with your preferred location list immediately.
;; Adjust the city list by editing `wttrin-default-locations` or answering wttrin prompts when asked.
;; Forecasts arrive in an Emacs buffer, so you can stay keyboard-only while checking weather.
;;
;;; Code:

(defvar wttrin-geolocation-command)

;; ----------------------------------- Wttrin ----------------------------------

(use-package wttrin
  :vc (:url "git@cjennings.net:emacs-wttrin.git"
       :branch "main"
       :rev :newest)
  ;; :load-path "~/code/emacs-wttrin"  ;; uncomment + comment :vc above for local dev
  :demand t  ;; REQUIRED: mode-line must start at Emacs startup
  :preface
  ;; Change this to t to enable debug logging
  ;; (setq wttrin-debug t)
  :bind
  ("M-S-w" . wttrin)  ;; was M-W, overrides kill-ring-save
  :config
  (setopt wttrin-unit-system "u")
  ;; Drop the "Follow @igor_chubin for wttr.in updates" footer. "F" is the
  ;; wttr.in flag for "no Follow line"; everything else (forecast, header,
  ;; colors) is unchanged.
  (setopt wttrin-display-options "F")
  (setopt wttrin-favorite-location "New Orleans, LA")
  ;; Scale the weather font to fit the window width, clamped to a floor/cap
  ;; (wttrin-font-height-min/-max, default 100/200).  setq (not setopt): the
  ;; wttrin-auto-fit-font defcustom only exists once feature/center-buffer-text
  ;; merges to main and the :vc package updates; until then this just sets a
  ;; value the old code ignores, and the later defcustom won't clobber it.
  (setq wttrin-auto-fit-font t)
  ;; Higher-accuracy geolocation via the whereami WiFi-scan script (Google-backed),
  ;; far better than IP behind a VPN or cellular hotspot.  Used by the picker's
  ;; "Current location (detect)" entry; wttrin falls back to its IP provider if the
  ;; command is missing or fails.  setq (not setopt): wttrin-geolocation-command is
  ;; defined in the lazily-loaded wttrin-geolocation sub-module, so it may be unbound
  ;; at :config time; the later defcustom won't clobber an already-set value.
  (setq wttrin-geolocation-command "/home/cjennings/.local/bin/whereami --json")
  (setopt wttrin-mode-line-refresh-interval (* 30 60)) ;; thirty minutes
  (setq wttrin-default-locations '(
                                   "New Orleans, LA"
                                   "Berkeley, CA"
                                   "Huntington Beach, CA"
                                   "Bury St Edmunds, UK"
                                   "New York, NY"
                                   "Littlestown, PA"
                                   "Soufrière, St Lucia"
                                   "London, GB"
                                   "Naples, IT"
                                   "Athens, GR"
                                   "Kyiv, UA"
                                   ))
  (wttrin-mode-line-mode 1))

(provide 'weather-config)
;;; weather-config.el ends here.
