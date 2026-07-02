;;; weather-config.el --- wttrin weather display and modeline setup -*- lexical-binding: t; coding: utf-8; -*-
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
;; Configures wttrin for favorite-location forecasts, mode-line weather, and
;; whereami-backed geolocation. M-S-w opens the weather buffer.
;;
;;; Code:

(defvar wttrin-geolocation-command)

;; ----------------------------------- Wttrin ----------------------------------

(use-package wttrin
  ;; Load from the local checkout (currently release/0.4.0) so recent wttrin
  ;; changes are testable without a package pull.  Swap back to :vc below for
  ;; production tracking.
  :load-path "~/code/emacs-wttrin"
  ;; :vc (:url "git@cjennings.net:emacs-wttrin.git"
  ;;      :branch "release/0.4.0"
  ;;      :rev :newest)
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
  ;; (wttrin-font-height-min/-max, default 100/200).
  (setopt wttrin-auto-fit-font t)
  ;; Higher-accuracy geolocation via the whereami WiFi-scan script (Google-backed),
  ;; far better than IP behind a VPN or cellular hotspot.  Used by the picker's
  ;; "Current location (detect)" entry; wttrin falls back to its IP provider if the
  ;; command is missing or fails.  setq (not setopt): wttrin-geolocation-command is
  ;; defined in the lazily-loaded wttrin-geolocation sub-module, so it may be unbound
  ;; at :config time; the later defcustom won't clobber an already-set value.
  (setq wttrin-geolocation-command "/home/cjennings/.local/bin/whereami --json")
  (setopt wttrin-mode-line-refresh-interval (* 30 60)) ;; thirty minutes
  ;; Tooltip forecast: remainder of today, tomorrow, and the next day (the
  ;; j1 feed's maximum), rendered under the current conditions on hover.
  (setopt wttrin-mode-line-tooltip-forecast-days 3)
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
