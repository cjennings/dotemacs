;;; weather-config.el ---  -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Call M-W to open wttrin with your preferred location list immediately.
;; Adjust the city list by editing `wttrin-default-locations` or answering wttrin prompts when asked.
;; Forecasts arrive in an Emacs buffer, so you can stay keyboard-only while checking weather.
;;
;;; Code:

;; ----------------------------------- Wttrin ----------------------------------

;; Load wttrin from local development directory
(add-to-list 'load-path "/home/cjennings/code/wttrin")

;; Set debug flag BEFORE loading wttrin (checked at load time)
(setq wttrin-debug nil)

(use-package wttrin
  ;; Uncomment the next line to use vc-install instead of local directory:
  ;; :vc (:url "https://github.com/cjennings/emacs-wttrin" :rev :newest)
  :defer t
  :preface
  ;; dependency for wttrin
  (use-package xterm-color
	:demand t)
  :bind
  ("M-W" . wttrin)
  :custom
  (wttrin-unit-system "u")
  (wttrin-mode-line-favorite-location "New Orleans, LA")
  (wttrin-mode-line-refresh-interval 900)  ; 15 minutes
  :init
  ;; Explicitly autoload the mode function (needed for local dev directory)
  (autoload 'wttrin-mode-line-mode "wttrin" "Toggle weather display in mode-line." t)
  ;; Enable mode-line widget AFTER Emacs finishes initializing
  ;; (url-retrieve async needs full init to work without buffer errors)
  (if (daemonp)
      ;; Daemon mode: wait for first client to connect
      (add-hook 'server-after-make-frame-hook
                (lambda () (wttrin-mode-line-mode 1))
                t) ; append to end of hook
    ;; Normal Emacs: wait for startup to complete
    (add-hook 'after-init-hook
              (lambda () (wttrin-mode-line-mode 1))
              t)) ; append to end of hook
  :config
  (setq wttrin-default-locations '(
							  "New Orleans, LA"
							  "Athens, GR"
							  "Berkeley, CA"
							  "Bury St Edmunds, UK"
							  "Kyiv, UA"
							  "Littlestown, PA"
							  "Soufrière, St Lucia"
							  "London, GB"
							  "Naples, IT"
							  "New York, NY"
							  )))

(provide 'weather-config)
;;; weather-config.el ends here.
