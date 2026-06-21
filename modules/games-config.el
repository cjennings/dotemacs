;;; games-config.el --- emacs games -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O.
;; Load shape: command (deferred).
;; Eager reason: none; loaded by init.el when malyon loads.
;; Top-level side effects: sets malyon-stories-directory after malyon loads.
;; Runtime requires: user-constants.
;; Direct test load: yes.
;;
;; Configuration for game packages.
;;
;; - Malyon: interactive fiction / Z-machine player (stories under ~/sync/org/text.games/).
;; - 2048: number-tile puzzle.
;;
;; malyon and 2048-game autoload their own commands via package.el, so this
;; module owns neither command -- it only supplies malyon's stories directory.
;; init.el loads it via `with-eval-after-load 'malyon', so it loads on first
;; use rather than at startup.
;;
;;; Code:

(require 'user-constants)  ;; org-dir

(with-eval-after-load 'malyon
  (setq malyon-stories-directory (concat org-dir "text.games/")))

(provide 'games-config)
;;; games-config.el ends here.
