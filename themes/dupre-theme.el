;;; dupre-theme.el --- A dark and elegant theme for Emacs -*- lexical-binding: t -*-

;; Version: 0.0.2
;; Author: Craig Jennings <c@cjennings.net>
;; URL: https://github.com/cjennings/dupre-theme
;; Keywords: dark theme faces

;;; Commentary:

;; A modified version of the distinguished theme by Kim Silkebækken
;; <kim.silkebaekken@gmail.com>.  This version by Craig Jennings
;; <c@cjennings.net>.

;;; Code:

(defgroup dupre-theme nil
  "Options for the `dupre' colour theme."
  :group 'faces)

(deftheme dupre
  "A dark and elegant theme for Emacs.")

(let* ((dupre-fg          (if (display-graphic-p) "#f0fef0" "color-231"))
       (dupre-bg          (if (display-graphic-p) "#000000" "color-232"))
       (dupre-bg+0        (if (display-graphic-p) "#151311" "color-234"))
       (dupre-bg+1        (if (display-graphic-p) "#252321" "color-236"))
       (dupre-bg+2        (if (display-graphic-p) "#474544" "color-238"))
       (dupre-gray-2      (if (display-graphic-p) "#58574e" "color-240"))
       (dupre-gray-1      (if (display-graphic-p) "#6c6a60" "color-242"))
       (dupre-gray        (if (display-graphic-p) "#969385" "color-243"))
       (dupre-gray+1      (if (display-graphic-p) "#b4b1a2" "color-250"))
       (dupre-gray+2      (if (display-graphic-p) "#d0cbc0" "color-252"))
       (dupre-steel       (if (display-graphic-p) "#8a9496" "color-245"))
       (dupre-steel+1     (if (display-graphic-p) "#acb0b3" "color-247"))
       (dupre-steel+2     (if (display-graphic-p) "#c0c7ca" "color-251"))
       (dupre-blue        (if (display-graphic-p) "#67809c" "color-173"))
       (dupre-blue+1      (if (display-graphic-p) "#b2c3cc" "color-66" ))
       (dupre-blue+2      (if (display-graphic-p) "#d9e2ff" "color-69" ))
       (dupre-green-2     (if (display-graphic-p) "#646d14" "color-22" ))
       (dupre-green-1     (if (display-graphic-p) "#869038" "color-28" ))
       (dupre-green       (if (display-graphic-p) "#a4ac64" "color-143"))
       (dupre-green+1     (if (display-graphic-p) "#ccc768" "color-67" ))
       (dupre-red-3       (if (display-graphic-p) "#3f1c0f" "color-124"))
       (dupre-red-2       (if (display-graphic-p) "#7c2a09" "color-196"))
       (dupre-red-1       (if (display-graphic-p) "#a7502d" "color-160"))
       (dupre-red         (if (display-graphic-p) "#d47c59" "color-179"))
       (dupre-red+1       (if (display-graphic-p) "#edb08f" "color-173"))
       (dupre-red+2       (if (display-graphic-p) "#edbca2" "color-208"))
       (dupre-yellow-2    (if (display-graphic-p) "#875f00" "color-94"))
       (dupre-yellow-1    (if (display-graphic-p) "#ffd700" "color-220"))
       (dupre-yellow      (if (display-graphic-p) "#d7af5f" "color-226"))
       (dupre-yellow+1    (if (display-graphic-p) "#ffd75f" "color-137"))
       (dupre-yellow+2    (if (display-graphic-p) "#f9ee98" "color-228"))
       (dupre-intense-red (if (display-graphic-p) "#ff2a00" "color-202")))

  ;; Tell Emacs “this is a dark theme”
  (custom-theme-set-variables
   'dupre
   `(frame-background-mode 'dark)
   `(fringe-mode 8))

  (custom-theme-set-faces
   'dupre

   ;; basics
   `(default                ((t (:foreground ,dupre-fg :background ,dupre-bg))))
   `(cursor                 ((t (:background ,dupre-fg))))
   `(hl-line                ((t (:background ,dupre-bg+0))))
   `(minibuffer-prompt      ((t (:foreground ,dupre-green :weight bold))))
   `(region                 ((t (:background ,dupre-bg+2))))
   `(fringe                 ((t (:foreground ,dupre-gray-2 :background ,dupre-bg+1))))
   `(vertical-border        ((t (:foreground ,dupre-bg+2))))
   `(mode-line              ((t (:foreground ,dupre-gray+2 :background ,dupre-bg+2
                                             :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive     ((t (:foreground ,dupre-gray   :background ,dupre-bg+1
                                             :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id    ((t (:foreground ,dupre-yellow  :weight bold))))
   `(button                 ((t (:foreground ,dupre-blue+1 :underline t))))

   ;; font-lock
   `(font-lock-builtin-face               ((t (:foreground ,dupre-yellow :weight bold))))
   `(font-lock-comment-face               ((t (:foreground ,dupre-gray :slant italic))))
   `(font-lock-comment-delimiter-face     ((t (:foreground ,dupre-bg+2))))
   `(font-lock-doc-face                   ((t (:foreground ,dupre-gray))))
   `(font-lock-constant-face              ((t (:foreground ,dupre-yellow+1 :weight bold))))
   `(font-lock-function-name-face         ((t (:foreground ,dupre-red :weight bold))))
   `(font-lock-keyword-face               ((t (:foreground ,dupre-blue :weight bold))))
   `(font-lock-negation-char-face         ((t (:foreground ,dupre-yellow :weight bold))))
   `(font-lock-preprocessor-face          ((t (:foreground ,dupre-steel+1 :weight bold :slant italic))))
   `(font-lock-regexp-grouping-construct  ((t (:foreground ,dupre-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash  ((t (:foreground ,dupre-red :weight bold))))
   `(font-lock-string-face                ((t (:foreground ,dupre-green))))
   `(font-lock-type-face                  ((t (:foreground ,dupre-green+1 :weight bold))))
   `(font-lock-variable-name-face         ((t (:foreground ,dupre-blue+1 :slant italic))))
   `(font-lock-warning-face               ((t (:foreground ,dupre-intense-red :weight bold))))

   ;; show-paren
   `(show-paren-match      ((t (:foreground ,dupre-fg :background ,dupre-green-2 :weight bold))))
   `(show-paren-mismatch   ((t (:foreground ,dupre-fg :background ,dupre-intense-red :weight bold))))

   ;; isearch
   `(isearch               ((t (:foreground ,dupre-fg :background ,dupre-green-2
                                            :weight bold :box (:line-width -1 :style released-button)))))
   `(isearch-fail          ((t (:foreground ,dupre-fg :background ,dupre-red-1
                                            :weight bold :box (:line-width -1 :style released-button)))))
   `(lazy-highlight        ((t (:foreground ,dupre-bg :background ,dupre-yellow+1
                                            :weight bold :box (:line-width -1 :style released-button)))))

   ;; line numbers (Emacs ≥26.1)
   (when (>= emacs-major-version 26)
     `(line-number            ((t (:foreground ,dupre-gray-1 :background ,dupre-bg+1))))
     `(line-number-current-line ((t (:foreground ,dupre-yellow :background ,dupre-bg+1 :weight bold)))))

   ;; tab-line (Emacs ≥27.1)
   (when (>= emacs-major-version 27)
     `(tab-line               ((t (:inherit mode-line :background ,dupre-bg+1 :box (:line-width 4 :color ,dupre-bg+1)))))
     `(tab-line-tab           ((t (:inherit tab-line))))
     `(tab-line-tab-inactive  ((t (:inherit tab-line :foreground ,dupre-gray))))
     `(tab-line-tab-current   ((t (:background ,dupre-bg+2 …))))
     `(tab-line-highlight     ((t (:background ,dupre-bg+1 …)))))

   )    ; closes the WHEN
  )     ; closes custom-theme-set-faces

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dupre)
;;; dupre-theme.el ends here

;; --------------------------------- ERT Tests ---------------------------------

(require 'ert)

(ert-deftest dupre-theme-default-face ()
  "dupre-theme should set the default face correctly."
  (load-theme 'dupre t)
  ;; In a graphical Emacs we expect "#000000"/"#f0fef0"
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
    (should (string= bg "#000000"))
    (should (string= fg "#f0fef0"))))

(ert-deftest dupre-theme-comment-face-italic ()
  "Comments should be rendered in italic slant."
  (load-theme 'dupre t)
  (should (eq (face-attribute 'font-lock-comment-face :slant) 'italic)))
