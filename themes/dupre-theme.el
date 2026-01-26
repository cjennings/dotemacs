;;; dupre-theme.el --- A dark and elegant theme for Emacs -*- lexical-binding: t -*-

;; Version: 1.0.0
;; Author: Craig Jennings <c@cjennings.net>
;; URL: https://github.com/cjennings/dupre-theme
;; Keywords: dark theme faces

;;; Commentary:

;; A dark, warm theme for Emacs with 150+ face definitions.
;; Originally based on the distinguished theme by Kim Silkebaekken.
;;
;; This theme is optimized for GUI Emacs.  Terminal fallbacks are basic.
;;
;; Color palette follows a warm aesthetic:
;; - Yellow (#d7af5f) as primary accent
;; - Blue (#67809c) for keywords and navigation
;; - Green (#a4ac64) for strings and success
;; - Red (#d47c59) for functions and emphasis
;;
;; File structure:
;; - dupre-theme.el (this file) - Theme definition and entry point
;; - dupre-palette.el - Color definitions and semantic mappings
;; - dupre-faces.el - All face specifications (~150 faces)

;;; Code:

(eval-and-compile
  ;; Add themes directory to load-path for require
  (when-let ((dir (file-name-directory (or load-file-name
                                           buffer-file-name
                                           (locate-library "dupre-theme")))))
    (unless (member dir load-path)
      (add-to-list 'load-path dir))))

(require 'dupre-palette)
(require 'dupre-faces)

(defgroup dupre-theme nil
  "Options for the `dupre' colour theme."
  :group 'faces)

(deftheme dupre
  "A dark and elegant theme for Emacs with warm undertones."
  :background-mode 'dark
  :kind 'color-scheme)

;; Set theme variables
(custom-theme-set-variables
 'dupre
 '(frame-background-mode 'dark)
 '(fringe-mode 8))

;; Apply all face definitions
(dupre-theme-set-faces)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dupre)
;;; dupre-theme.el ends here
