;;; games-config.el --- emacs games -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O.
;; Load shape: command (deferred).
;; Eager reason: none; loaded on first use of `malyon' or `2048-game'.
;; Top-level side effects: package configuration via use-package (deferred).
;; Runtime requires: none.
;; Direct test load: yes.
;;
;; Configuration for game packages.
;;
;; - Malyon for playing interactive fiction and text adventures in Z-machine format
;;  (stories directory: ~/sync/org/text.games/)
;; - 2048 number-tile puzzle game
;;
;; init.el autoloads `malyon' and `2048-game' to this module instead of
;; requiring it eagerly, so the first invocation of either command loads
;; games-config, which configures and then loads the package.
;;
;;; Code:

;; ----------------------------------- Malyon ----------------------------------
;; text based adventure player

(use-package malyon
  :defer t
  :commands (malyon)
  :config
  (setq malyon-stories-directory (concat org-dir "text.games/")))

;; ------------------------------------ 2048 -----------------------------------
;; combine numbered tiles to create the elusive number 2048.
(use-package 2048-game
  :defer t
  :commands (2048-game))

(provide 'games-config)
;;; games-config.el ends here.
