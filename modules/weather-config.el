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
  :defer t
  :load-path ("~/code/wttrin")
  :ensure nil ;; local package
  :preface
  ;; dependency for wttrin
  (use-package xterm-color
	:demand t)
  :bind
  ("M-W" . wttrin)
  :custom
  (wttrin-unit-system "u")
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
