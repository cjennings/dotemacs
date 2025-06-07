;;; telegram-config.el --- Configuration for the Telegram Client -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:

;; Telegram Client Setup Notes

;; 1. Pull latest telega-server image:
;; $ docker pull zevlg/telega-server:latest

;; 2. Set telega-use-docker to non-nil  to connect to docker telega-server
;; (setq telega-use-docker t)

;; 3. Set telega-use-images to non-nil (for emacsclient)
;; (setq telega-use-images t)

;; 4. keybindings for telega only display if you bind the prefix-map for a list
;; of default keys: https://zevlg.github.io/telega.el/#telega-prefix-map, so put
;; this in your use-package declaration:
;;  :bind
;;  ("C-c T" . telega)

;; 5. Set up docker image and tell Telega to use it.
;; docker pull zevlg/telega-server:latest
;; (setq telega-use-docker t)


;;; Code:

;; ----------------------------------- Telega ----------------------------------
;; telegram client

(use-package telega
  :defer 1
  :commands (telega)
  :init
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  :bind
  ("C-c T" . telega)
  (:map telega-chat-button-map
		("DEL" . telega-chat-delete))
  :custom
  (telega-use-images t)
  (telega-emoji-use-images t)
  :config
  (setq telega-use-docker t))

(provide 'telegram-config)
;;; telegram-config.el ends here.
