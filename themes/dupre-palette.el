;;; dupre-palette.el --- Color palette for dupre-theme -*- lexical-binding: t -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; This file defines the color palette and semantic mappings for dupre-theme.
;; Colors are defined as hex values for GUI Emacs.

;;; Code:

(defconst dupre-palette
  '(;;; Base colors
    (bg          "#151311")
    (bg+0        "#151311")
    (bg+1        "#252321")
    (bg+2        "#474544")

    ;; Grays
    (gray-2      "#58574e")
    (gray-1      "#6c6a60")
    (gray        "#969385")
    (gray+1      "#b4b1a2")
    (gray+2      "#d0cbc0")

    ;; Steel (blue-grays)
    (steel       "#8a9496")
    (steel+1     "#acb0b3")
    (steel+2     "#c0c7ca")

    ;; Blues
    (blue        "#67809c")
    (blue+1      "#b2c3cc")
    (blue+2      "#d9e2ff")

    ;; Greens
    (green-2     "#646d14")
    (green-1     "#869038")
    (green       "#a4ac64")
    (green+1     "#ccc768")

    ;; Reds
    (red-3       "#3f1c0f")
    (red-2       "#7c2a09")
    (red-1       "#a7502d")
    (red         "#d47c59")
    (red+1       "#edb08f")
    (red+2       "#edbca2")

    ;; Yellows
    (yellow-2    "#875f00")
    (yellow-1    "#ffd700")
    (yellow      "#d7af5f")
    (yellow+1    "#ffd75f")
    (yellow+2    "#f9ee98")

    ;; Intense/alert colors
    (intense-red "#ff2a00")

    ;; Foreground
    (fg          "#f0fef0"))
  "Color palette for dupre-theme.
Each entry is (NAME VALUE) where VALUE is a hex color string.")

(defconst dupre-semantic-mappings
  '(;;; Status colors
    (accent      yellow)
    (err         intense-red)
    (warning     yellow+1)
    (success     green)
    (info        blue+1)

    ;;; Syntax highlighting
    (keyword     blue)
    (string      green)
    (comment     gray)
    (constant    yellow+1)
    (function    red)
    (variable    blue+1)
    (type        green+1)
    (builtin     yellow)
    (preprocessor steel+1)

    ;;; UI elements
    (border      bg+2)
    (selection   bg+2)
    (hl-line     bg+0)
    (link        blue+1)
    (cursor      fg)
    (prompt      green)

    ;;; Diff colors
    (diff-added-bg    green-2)
    (diff-added-fg    green)
    (diff-removed-bg  red-3)
    (diff-removed-fg  red)
    (diff-changed-bg  yellow-2)
    (diff-changed-fg  yellow))
  "Semantic color mappings for dupre-theme.
Each entry maps a semantic name to a palette color name.")

(defun dupre-get-color (name)
  "Get the hex value for color NAME from the palette.
NAME can be a base palette color or a semantic mapping."
  (let ((semantic (assq name dupre-semantic-mappings)))
    (if semantic
        ;; Resolve semantic mapping to base color
        (dupre-get-color (cadr semantic))
      ;; Look up in base palette
      (let ((color (assq name dupre-palette)))
        (if color
            (cadr color)
          (error "Unknown color: %s" name))))))

(defmacro dupre-with-colors (&rest body)
  "Execute BODY with all palette colors bound as local variables."
  (declare (indent 0))
  `(let ,(mapcar (lambda (entry)
                   (list (car entry) (cadr entry)))
                 dupre-palette)
     ;; Also bind semantic mappings resolved to their values
     (let ,(mapcar (lambda (entry)
                     (let* ((name (car entry))
                            (target (cadr entry))
                            (resolved (cadr (assq target dupre-palette))))
                       (list name resolved)))
                   dupre-semantic-mappings)
       ,@body)))

(provide 'dupre-palette)
;;; dupre-palette.el ends here
