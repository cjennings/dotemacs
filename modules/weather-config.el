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

(use-package wttrin
  ;; Uncomment the next line to use vc-install instead of local directory:
  ;; :vc (:url "https://github.com/cjennings/emacs-wttrin" :rev :newest)
  :demand t  ;; REQUIRED: mode-line must start at Emacs startup
  :load-path "/home/cjennings/code/wttrin"
  :preface
  ;; Change this to t to enable debug logging
  ;; (setq wttrin-debug t)
  :bind
  ("M-W" . wttrin)
  :config
  (setopt wttrin-unit-system "u")
  (setopt wttrin-mode-line-favorite-location "New Orleans, LA")
  (setopt wttrin-mode-line-refresh-interval (* 30 60)) ;; thirty minutes
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
                                   ))
  (wttrin-mode-line-mode 1))

(provide 'weather-config)
;;; weather-config.el ends here.
