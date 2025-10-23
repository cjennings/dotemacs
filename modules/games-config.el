;;; games-config.el --- emacs games -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Configuration for game packages.
;;
;; - Malyon for playing interactive fiction and text adventures in Z-machine format
;;  (stories directory: ~/sync/org/text.games/)
;; - 2048 number-tile puzzle game
;;
;;; Code:

;; ----------------------------------- Malyon ----------------------------------
;; text based adventure player

(use-package malyon
  :defer 1
  :config
  (setq malyon-stories-directory (concat org-dir "text.games/")))

;; ------------------------------------ 2048 -----------------------------------
;; combine numbered tiles to create the elusive number 2048.
(use-package 2048-game
  :defer 1)

(provide 'games-config)
;;; games-config.el ends here.
