;;; font-config --- Font Defaults and Related Functionality -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

;; ----------------------- Font Family And Size Selection ----------------------
;; preset your fixed and variable fonts, then apply them to text as a set

(use-package fontaine
  :demand t
  :bind
  ("M-F" . fontaine-set-preset)
  :config
  (setq fontaine-presets
		'(
          (default
		   :default-family "FiraCode Nerd Font Mono"
		   :default-weight regular
		   :default-height 110
		   :fixed-pitch-family nil          ;; falls back to :default-family
		   :fixed-pitch-weight nil          ;; falls back to :default-weight
		   :fixed-pitch-height 1.0
           :variable-pitch-family "Merriweather"
           :variable-pitch-weight light
           :variable-pitch-height 1.0)
          (Hack
           :default-family "Hack Nerd Font Mono"
           :variable-pitch-family "Hack Nerd Font Mono")
		  (FiraCode-Literata
		   :default-family "Fira Code Nerd Font"
		   :variable-pitch-family "Literata")
		  (JetBrains-Lato
		   :default-family "JetBrains Mono"
		   :variable-pitch-family "Lato")
		  (Codelia-Only
		   :default-family "Codelia Ligatures")
		  (Liberation_Mono-Only
           :default-family "Liberation Mono")
          (24-point-font
           :default-height 240)
          (20-point-font
           :default-height 200)
          (16-point-font
           :default-height 160)
		  (14-point-font
		   :default-height 140)
		  (13-point-font
		   :default-height 130)
		  (12-point-font
		   :default-height 120)
		  (11-point-font
		   :default-height 110)
		  (10-point-font
		   :default-height 100)
		  (t                                ;; shared fallback properties go here
		   :default-family "FiraCode Nerd Font Mono"
		   :default-weight regular
		   :default-height 110
		   :fixed-pitch-family nil          ;; falls back to :default-family
		   :fixed-pitch-weight nil          ;; falls back to :default-weight
		   :fixed-pitch-height 1.0
		   :fixed-pitch-serif-family nil    ;; falls back to :default-family
		   :fixed-pitch-serif-weight nil    ;; falls back to :default-weight
		   :fixed-pitch-serif-height 1.0
		   :variable-pitch-family "Merriweather"
		   :variable-pitch-weight light
		   :variable-pitch-height 1.0
		   :bold-family nil                 ;; use whatever the underlying face has
		   :bold-weight bold
		   :italic-family nil
		   :italic-slant italic
		   :line-spacing nil))))

(with-eval-after-load 'fontaine
  (defun cj/apply-font-settings-after-ui-creation()
	"Apply font settings when Emacsclient is run in daemon mode.
Emacsclient connects to a headless Emacs daemon, when no UI
exists yet to apply these settings.  This solution requests Emacsclient
to apply the default font settings after the server creates a frame.
Note that server-after-make-frame-hook is available only in Emacs 27+."
	(interactive)
	(fontaine-set-preset 'default)
	(if (daemonp)
        (remove-hook 'server-after-make-frame-hook #'cj/apply-font-settings-after-ui-creation)))

  (if (daemonp)
	  (add-hook 'server-after-make-frame-hook #'cj/apply-font-settings-after-ui-creation)
	(cj/apply-font-settings-after-ui-creation)))

;; ----------------------------- Font Install Check ----------------------------
;; convenience function to indicate whether a font is available by name.

(defun cj/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
	  t
	nil))

;; ------------------------------- All The Icons -------------------------------
;; icons made available through fonts

(use-package all-the-icons
  :demand t
  :config
  (when (and (not (cj/font-installed-p "all-the-icons"))
			 (window-system))
	(all-the-icons-install-fonts t)))

(use-package all-the-icons-nerd-fonts
  :after all-the-icons
  :demand t
  :config
  (all-the-icons-nerd-fonts-prefer))

;; ----------------------------- Emoji Fonts Per OS ----------------------------
;; these are in reverse order of priority

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;; ---------------------------------- Emojify ----------------------------------
;; converts emoji identifiers into emojis; allows for easy emoji entry.

(use-package emojify
  :hook ((erc-mode . emojify-mode)
		 (org-mode . emojify-mode)
		 (prog-mode . (lambda () (emojify-mode -1)))
		 (gptel-mode . (lambda () (emojify-mode -1))))
  :custom
  (emojify-download-emojis-p t) ;; don't ask, just download emojis
  :bind
  ("C-c e i" . emojify-insert-emoji)  ;; emoji insert
  ("C-c e l" . emojify-list-emojis)   ;; emoji list
  :config
  (setq emojify-show-help nil)
  (setq emojify-point-entered-behaviour 'uncover)
  (setq emojify-display-style 'image)
  (setq emojify-emoji-styles '(ascii unicode github)))

;; -------------------------- Display Available Fonts --------------------------
;; display all available fonts on the system in a side panel

(defun cj/display-available-fonts ()
  "Display a list of all font faces with sample text in another read-only buffer."
  (interactive)
  (pop-to-buffer "*Available Fonts*" '(display-buffer-in-side-window . ((side . right)(window-width . fit-window-to-buffer))))
  (let ((font-list (font-family-list)))
    (setq font-list (cl-remove-duplicates (cl-sort font-list 'string-lessp :key 'downcase)))
    (with-current-buffer "*Available Fonts*"
      (erase-buffer)
      (dolist (font-family font-list)
        (insert (propertize (concat font-family) 'face `((:foreground "Light Blue" :weight bold))))
        (insert (concat "\n"(propertize "Regular: ")))
        (insert (propertize (concat "The quick brown fox jumps over the lazy dog I 1 l ! : ; . , 0 O o [ { ( ) } ] ?")
                            'face `((:family, font-family))))
        (insert (concat "\n" (propertize "Bold: ")))
        (insert (propertize (concat "The quick brown fox jumps over the lazy dog I 1 l ! : ; . , 0 O o [ { ( ) } ] ?")
                            'face `((:family, font-family :weight bold))))
        (insert (concat "\n" (propertize "Italic: ")))
        (insert (propertize (concat "The quick brown fox jumps over the lazy dog I 1 l ! : ; . , 0 O o [ { ( ) } ] ?")
                            'face `((:family, font-family :slant italic))))
        (insert (concat "\n\n"))))
    (move-to-window-line 0)
    (special-mode)))

(global-set-key (kbd "C-z F") 'cj/display-available-fonts)

;; ----------------------- Increase / Decrease Font Size -----------------------
;; make it easy to enlarge or shrink font sizes with keybindings

(setq text-scale-mode-step 1.08)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; --------------------------------- Ligatures --------------------------------- '
;; fancy programming glyphs make code easier to read

(use-package ligature
  :defer 1
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww, if `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable ligatures in markdown mode
  (ligature-set-ligatures 'markdown-mode '(("=" (rx (+ "=") (? (| ">" "<"))))
										   ("-" (rx (+ "-")))))
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
									   ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
									   "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
									   "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
									   "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
									   "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
									   "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
									   "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
									   "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
									   "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

(provide 'font-config)
;;; font-config.el ends here
