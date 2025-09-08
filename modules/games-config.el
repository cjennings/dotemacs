;;; games-config.el --- emacs games -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; The games menu is the easiest entry. "Shift-Alt-G" will get you there. Enjoy!
;;

;;; Code:

;; --------------------------------- Games Menu --------------------------------

;; (defhydra hydra-games (:color blue :hint nil)
;;   "\n"
;;   ("2" 2048-game       "2048      : Combine the Numbered Tiles to get to 2048" :column "Game")
;;   ("c" chess           "Chess     : Play the 64 Squares and Checkmate the King" :column "Game")
;;   ("d" dunnet          "Dunnet    : Emacs' Built-n Text Adventure" :column "Game")
;;   ("g" gomoku          "Gomoku    : Tic Tac Toe, but Five in a Row" :column "Game")
;;   ("m" malyon          "Malyon    : The Text Adventure Player" :column "Game")
;;   ("t" tetris          "Tetris    : Combine falling blocks and score" :column "Game"))
;; (global-set-key (kbd "M-G") 'hydra-games/body)

;; ----------------------------------- Malyon ----------------------------------
;; text based adventure player

(use-package malyon
  :defer 1
  :config
  (setq malyon-stories-directory (concat sync-dir "text.games/")))

;; ------------------------------------ 2048 -----------------------------------
;; combine numbered tiles to create the elusive number 2048.
(use-package 2048-game
  :defer 1)

;; ----------------------------------- Chess -----------------------------------
;; play the 64 squares and checkmate the opponent's king
;; (use-package chess
;;   :defer 1
;;   :config
;;   (setq chess-default-display 'chess-images)
;;   (setq chess-images-directory
;; 		(concat user-emacs-directory "assets/chess/pieces/xboard/"))
;;   (setq chess-images-dark-color "#779556")
;;   (setq chess-images-light-color "#EBECD0")
;;   (setq chess-images-default-size 100)
;;   (setq chess-full-name user-whole-name)
;;   (setq chess-default-engine 'chess-fruit))


;; Notes from source code
;; If you'd like to view or edit Portable Game Notation (PGN) files,
;; `chess-pgn-mode' provides a text-mode derived mode which can display the
;; chess position at point.

;; To improve your chess ability, `M-x chess-tutorial' provides a simple knight
;; movement exercise to get you started, and `M-x chess-puzzle' can be used
;; to solve puzzle collections in EPD or PGN format.
;; The variable `chess-default-display' controls which display modules
;; are tried when a chessboard should be displayed.  By default, chess-images
;; is tried first.  If Emacs is not running in a graphical environment,
;; chess-ics1 is used instead.  To enable the chess-plain display module,
;; customize `chess-default-display' accordingly.

;; Once this is working, the next thing to do is to customize
;; `chess-default-modules'.  This is a list of functionality modules used
;; by chess.el to provide additional functionality.  You can enable or
;; disable modules so that Emacs Chess better suites your tastes.
;; Those modules in turn often have configuration variables, and

(provide 'games-config)
;;; games-config.el ends here.
