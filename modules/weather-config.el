;;; weather-config.el ---  -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;

;;; Code:

;; ----------------------------------- Wttrin ----------------------------------
;; show the weather forecast in an Emacs buffer

(use-package wttrin
  :defer .5
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
							  "Athens, GR"
							  "Berkeley, CA"
							  "Bury St Edmunds, UK"
							  "Kyiv, UA"
							  "Littlestown, PA"
							  "London, GB"
							  "Naples, IT"
							  "New Orleans, LA"
							  "New York, NY"
							  )))

(provide 'weather-config)
;;; weather-config.el ends here.
