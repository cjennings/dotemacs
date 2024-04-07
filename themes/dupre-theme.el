;;; dupre-theme.el --- A dark and elegant theme for emacs.

;; Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; Copyright © 2023 Craig Jennings
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; A modified version of the distinguished theme by Kim Silkebækken  <kim.silkebaekken@gmail.com>

;;; Code:
(deftheme dupre "Dupre color theme.")

(let ((dupre-fg          (if (display-graphic-p) "#f0fef0" "color-231"))
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

  (custom-theme-set-faces
   'dupre
   `(default                ((t (:foreground ,dupre-fg :background ,dupre-bg))))
   `(cursor                 ((t (:foreground ,dupre-bg :background ,dupre-fg))))
   `(hl-line                ((t (:background ,dupre-bg+0))))
   `(minibuffer-prompt      ((t (:foreground ,dupre-green :weight bold))))
   `(region                 ((t (:background ,dupre-bg+2))))
   `(fringe                 ((t (:foreground ,dupre-gray-2 :background ,dupre-bg+1))))
   `(secondary-selection    ((t (:inherit hl-line))))
   `(vertical-border        ((t (:foreground ,dupre-bg+2))))
   `(mode-line              ((t (:foreground ,dupre-gray+2 :background ,dupre-bg+2 :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive     ((t (:foreground ,dupre-gray   :background ,dupre-bg+1 :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id    ((t (:foreground ,dupre-yellow  :weight bold))))
   `(button                 ((t (:foreground ,dupre-blue+1 :underline t))))

   ;; ELFEED
   `(elfeed-search-date-face            ((t (:foreground ,dupre-blue :weight bold))))
   `(elfeed-search-feed-face            ((t (:foreground ,dupre-yellow))))
   `(elfeed-search-tag-face             ((t (:foreground ,dupre-gray))))
   `(elfeed-unread-search-title-face    ((t (:inherit bold-italic :foreground ,dupre-fg))))
   `(elfeed-search-title-face           ((t (:foreground ,dupre-fg))))

   ;; FONT LOCK
   `(font-lock-builtin-face                 ((t (:foreground ,dupre-yellow :weight bold))))
   `(font-lock-comment-face                 ((t (:foreground ,dupre-gray, :slant italic))))
   `(font-lock-comment-delimiter-face       ((t (:foreground ,dupre-bg+2))))
   `(font-lock-doc-face                     ((t (:foreground ,dupre-gray))))
   `(font-lock-constant-face                ((t (:foreground ,dupre-yellow+1 :weight bold))))
   `(font-lock-function-name-face           ((t (:foreground ,dupre-red :weight bold))))
   `(font-lock-keyword-face                 ((t (:foreground ,dupre-blue :weight bold))))
   `(font-lock-negation-char-face           ((t (:foreground ,dupre-yellow :weight bold))))
   `(font-lock-preprocessor-face            ((t (:foreground ,dupre-steel+1 :weight bold :slant italic))))
   `(font-lock-regexp-grouping-construct    ((t (:foreground ,dupre-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash    ((t (:foreground ,dupre-red :weight bold))))
   `(font-lock-string-face                  ((t (:foreground ,dupre-green))))
   `(font-lock-type-face                    ((t (:foreground ,dupre-green+1 :weight bold))))
   `(font-lock-variable-name-face           ((t (:foreground ,dupre-blue+1 :weight normal :slant italic))))
   `(font-lock-warning-face                 ((t (:foreground ,dupre-intense-red :weight bold))))

   ;; GIT-GUTTER
   `(git-gutter:added       ((t (:foreground ,dupre-bg :background ,dupre-green-1))))
   `(git-gutter:deleted     ((t (:foreground ,dupre-bg :background ,dupre-red-2 ))))
   `(git-gutter:modified    ((t (:foreground ,dupre-bg :background ,dupre-yellow))))
   `(git-gutter:unchanged   ((t (:inherit fringe :background ,dupre-bg+1))))
   `(git-gutter-fr:added    ((t (:foreground ,dupre-bg :background ,dupre-green-1))))
   `(git-gutter-fr:deleted  ((t (:foreground ,dupre-bg :background ,dupre-red-2))))
   `(git-gutter-fr:modified ((t (:foreground ,dupre-bg :background ,dupre-yellow))))

   ;; RAINBOW DELIMITERS
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dupre-yellow+2))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dupre-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dupre-red+1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dupre-blue+1))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dupre-yellow+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dupre-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dupre-red+1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dupre-blue+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dupre-yellow+2))))
   ;; rainbow-delimiters-mismatched-face
   ;; rainbow-delimiters-unmatched-face
   ;; rainbow-delimiters-base-error-face


   ;; python-mode
   `(py-pseudo-keyword-face ((t (:foreground ,dupre-blue+1 :weight bold))))
   `(py-object-reference-face ((t (:foreground ,dupre-blue :weight bold :slant italic))))
   `(py-variable-name-face ((t (:foreground ,dupre-blue+1 :weight normal :slant italic))))
   `(py-number-face ((t (:foreground ,dupre-yellow-1 :weight normal :slant italic))))
   `(py-import-from-face ((t (:foreground ,dupre-red))))
   `(py-def-class-face ((t (:foreground ,dupre-blue :weight bold))))
   `(py-decorators-face ((t (:foreground ,dupre-yellow :slant italic))))
   `(py-class-name-face ((t (:foreground ,dupre-green+1 :weight bold))))
   `(py-exception-name-face ((t (:foreground ,dupre-red :weight bold))))

   ;; js2-mode
   `(js2-external-variable ((t (:foreground ,dupre-yellow+1 :weight normal :slant italic))))
   `(js2-function-param ((t (:foreground ,dupre-red+1 :weight normal :slant italic))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,dupre-yellow-1 :weight normal :slant italic))))

   ;; basic whitespace-mode (tabs/newlines)
   `(whitespace-tab ((t (:foreground ,dupre-bg+1 :background nil :weight normal))))
   `(whitespace-newline ((t (:foreground ,dupre-red-3 :background nil :weight normal))))

   ;; show parens
   `(show-paren-mismatch ((t (:foreground ,dupre-fg :background ,dupre-intense-red :weight bold))))
   `(show-paren-match ((t (:foreground ,dupre-fg :background ,dupre-green-2 :weight bold))))

   ;; search highlight
   `(isearch ((t (:foreground ,dupre-fg :background ,dupre-green-2 :weight bold :slant normal :box (:line-width -1 :style released-button)))))
   `(isearch-fail ((t (:foreground ,dupre-fg :background ,dupre-red-1 :weight bold :slant normal :box (:line-width -1 :style released-button)))))
   `(lazy-highlight ((t (:foreground ,dupre-bg :background ,dupre-yellow+1 :weight bold :slant normal :box (:line-width -1 :style released-button)))))

   ;; flx
   `(flx-highlight-face ((t (:foreground ,dupre-red :weight bold :underline ,dupre-red-2))))

   ;; flycheck
   `(flycheck-error
     ((((supports :underline (:style line)))
	   (:underline (:style line :color ,dupre-red-1) :inherit unspecified))
	  (t (:foreground ,dupre-red-1 :weight bold :underline t))))
   `(flycheck-warning
	 ((((supports :underline (:style line)))
	   (:underline (:style line :color ,dupre-yellow) :inherit unspecified))
	  (t (:foreground ,dupre-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style line)))
	   (:underline (:style line :color ,dupre-blue) :inherit unspecified))
	  (t (:foreground ,dupre-blue :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:inherit fringe :foreground ,dupre-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:inherit fringe :foreground ,dupre-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:inherit fringe :foreground ,dupre-blue :weight bold))))
   `(flycheck-error-list-filename ((t (:foreground ,dupre-blue :weight bold))))
   `(flycheck-error-list-info ((t (:foreground ,dupre-green :weight bold))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,dupre-gray-2 :background nil
											   :weight normal :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,dupre-bg :background ,dupre-intense-red
											   :weight bold :slant normal :inverse-video nil :box (:line-width -1 :style released-button)))))

   ;; auto-complete
   `(ac-candidate-face ((t (:foreground ,dupre-gray :background ,dupre-bg+1))))
   `(ac-completion-face ((t (:foreground ,dupre-gray-1 :background nil :weight normal :slant normal :underline ,dupre-bg+1))))
   `(ac-selection-face ((t (:foreground ,dupre-fg :background ,dupre-blue :weight bold))))
   `(popup-tip-face ((t (:foreground ,dupre-bg :background ,dupre-yellow))))
   `(popup-scroll-bar-foreground-face ((t (:background ,dupre-gray+2))))
   `(popup-scroll-bar-background-face ((t (:background ,dupre-bg+2))))
   `(popup-isearch-match ((t (:foreground ,dupre-fg :background ,dupre-bg))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,dupre-fg :background ,dupre-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,dupre-gray :background ,dupre-bg+1 :slant italic))))
   `(company-tooltip-selection ((t (:foreground ,dupre-fg :background ,dupre-blue :weight bold))))
   `(company-tooltip-mouse ((t (:background ,dupre-bg+2))))
   `(company-tooltip-common ((t (:foreground ,dupre-gray :background ,dupre-bg+1 :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground ,dupre-blue+2 :background ,dupre-blue :weight bold))))
   `(company-scrollbar-fg ((t (:background ,dupre-gray))))
   `(company-scrollbar-bg ((t (:background ,dupre-bg+2))))
   `(company-preview ((t (:background ,dupre-blue))))
   `(company-preview-common ((t (:foreground ,dupre-fg :background ,dupre-yellow :weight bold))))

   ;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face ((t (:foreground ,dupre-gray+1 :background ,dupre-bg+0))))

   ;; diff
   `(diff-added ((t (:foreground ,dupre-green :background ,dupre-green-2))))
   `(diff-changed ((t (:foreground ,dupre-yellow-1 :background ,dupre-yellow-2))))
   `(diff-file-header ((t (:background ,dupre-green+1 :weight bold))))
   `(diff-function ((t (:background ,dupre-gray-1))))
   `(diff-header ((t (:background ,dupre-gray-1))))
   `(diff-hunk-header ((t (:background ,dupre-gray-1))))
   `(diff-index ((t (:background ,dupre-blue+1))))
   `(diff-indicator-added ((t (:foreground ,dupre-green-1 :weight bold))))
   `(diff-indicator-changed ((t (:foreground ,dupre-yellow-1 :weight bold))))
   `(diff-indicator-removed ((t (:foreground ,dupre-red-1 :weight bold))))
   `(diff-refine-added ((t (:foreground ,dupre-green :background ,dupre-green-1 :weight bold))))
   `(diff-refine-change ((t (:foreground ,dupre-yellow-1 :background ,dupre-red+2 :weight bold))))
   `(diff-refine-removed ((t (:foreground ,dupre-red-1 :background ,dupre-red+1))))
   `(diff-removed ((t (:foreground ,dupre-red-1))))

   ;; WIP FACES TO DEFINE

   ;; ert-test-result-expected
   ;; ert-test-result-unexpected

   ;; which-key-command-description-face
   ;; which-key-docstring-face
   ;; which-key-group-description-face
   ;; which-key-highlighted-command-face
   ;; which-key-key-face
   ;; which-key-local-map-description-face
   ;; which-key-note-face
   ;; which-key-separator-face
   ;; which-key-special-key-face

   ;; mu4e-compose-separator-face                          abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-contact-face                                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-context-face                                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-draft-face                                      abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-flagged-face                                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-footer-face                                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-forwarded-face                                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-header-face                                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-header-field-face                               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-header-highlight-face                           abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-header-key-face                                 abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-header-marks-face                               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-header-title-face                               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-header-value-face                               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-highlight-face                                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-link-face                                       abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-modeline-face                                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-ok-face                                         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-region-code                                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-related-face                                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-replied-face                                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-special-header-value-face                       abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-system-face                                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-title-face                                      abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-trashed-face                                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-unread-face                                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-url-number-face                                 abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mu4e-warning-face                                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ

   ;; mode-line                                            abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mode-line-active                                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mode-line-buffer-id                                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mode-line-emphasis                                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mode-line-highlight                                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mode-line-inactive                                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-buffer-name                                abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-buffer-status-modified                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-buffer-status-narrowed                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-buffer-status-read-only                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-encoding                                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-major-mode                                 abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-status-error                               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-status-info                                abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-status-neutral                             abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-status-success                             abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-status-warning                             abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
   ;; mood-line-unimportant                                abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ


   ;; emacs >= 26.1
   (when (>= emacs-major-version 26)
	 (custom-theme-set-faces
	  'dupre
	  `(line-number ((t (:inherit fringe))))
	  `(line-number-current-line ((t (:inherit fringe :foreground ,dupre-yellow :weight bold :slant italic))))))
   ;; ;; emacs >= 27.1
   ;; (when (>= emacs-major-version 27)
   ;;  (custom-theme-set-faces
   ;;   'dupre
   ;;   `(line-number ((t (:background ,bg2 :foreground ,fg4))))
   ;;   `(line-number-current-line ((t (:background ,bg2 :foreground ,fg1))))
   ;;   `(tab-line ((,class (:inherit fringe :box (:line-width 4 :color ,bg2)))))
   ;;   `(tab-line-tab ((,class (:inherit tab-line))))
   ;;   `(tab-line-tab-inactive ((,class (:inherit tab-line :foreground ,comment))))
   ;;   `(tab-line-tab-current  ((,class (:background ,bg4 :foreground ,fg1 :box (:line-width 4 :color ,bg4)))))
   ;;   `(tab-line-highlight    ((,class (:background ,bg1 :foreground ,fg2 :box (:line-width 4 :color ,bg1))))))))

   ))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
			   (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'dupre)
(provide 'dupre-theme)
;;; dupre-theme.el ends here
