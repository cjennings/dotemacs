;;; modeline-config --- Modeline Settings -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Minimal modeline configuration using mood-line.

;; mood-line is a lightweight, minimal modeline inspired by doom-modeline
;; but with much better performance and simpler configuration.

;; Features:
;; - Buffer status and modification indicators
;; - Major mode display
;; - Version control status
;; - Flycheck/Flymake status
;; - Cursor position and buffer percentage
;; - Anzu and multiple-cursors counters
;; - No dependencies
;; - Minimal performance overhead

;;; Code:

;; -------------------------------- mood-line ----------------------------------

(use-package mood-line
  :config
  (mood-line-mode))


(provide 'modeline-config)
;;; modeline-config.el ends here
