;;; font-config --- Font Defaults and Related Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; This module provides font configuration, including:
;;
;; 1. Font Management:
;;    - Dynamic font preset switching via `fontaine' package
;;    - Separate configurations for fixed-pitch and variable-pitch fonts
;;    - Multiple size presets for different viewing contexts
;;    - Per-frame font configuration tracking for daemon mode compatibility
;;
;; 2. Icon Support:
;;    - All-the-icons integration with automatic font installation
;;    - Nerd fonts support for enhanced icons in terminals and GUI
;;    - Platform-specific emoji font configuration (Noto, Apple, Segoe)
;;    - Emojify package for emoji rendering and insertion
;;
;; 3. Typography Enhancements:
;;    - Programming ligatures via `ligature' package
;;    - Mode-specific ligature rules for markdown and programming
;;    - Text scaling keybindings for quick size adjustments
;;
;; 4. Utility Functions:
;;    - `cj/font-installed-p': Check font availability
;;    - `cj/display-available-fonts': Interactive font browser with samples
;;    - Frame-aware font application for client/server setups
;;
;; Configuration Notes:
;; - Default font: FiraCode Nerd Font Mono at 110 height
;; - Variable pitch: Merriweather Light for prose-heavy modes
;; - Handles both standalone and daemon mode Emacs instances
;; - Emoji fonts selected based on OS availability
;;
;; Keybindings:
;; - M-F: Select font preset
;; - C-z F: Display available fonts
;; - C-+/C-=: Increase text scale
;; - C--/C-_: Decrease text scale
;;
;;
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
		   :default-family "Berkeley Mono"
		   :default-weight regular
		   :default-height 110
		   :fixed-pitch-family nil          ;; falls back to :default-family
		   :fixed-pitch-weight nil          ;; falls back to :default-weight
		   :fixed-pitch-height 1.0
		   :variable-pitch-family "Lexend"
		   :variable-pitch-weight regular
		   :variable-pitch-height 1.0)
		  (FiraCode
		   :default-family "FiraCode Nerd Font Mono"
		   :variable-pitch-family "Merriweather"
		   :variable-pitch-weight light)
		  (Hack
		   :default-family "Hack Nerd Font Mono"
		   :variable-pitch-family "Hack Nerd Font Mono")
		  (BerkeleyMono
		   :default-family "Berkeley Mono"
		   :variable-pitch-family "Charis SIL")
		  (FiraCode-Literata
		   :default-family "Fira Code Nerd Font"
		   :variable-pitch-family "Literata")
          (EBook
           :default-family "Merriweather"
           :default-weight regular
           :default-height 200
		   :variable-pitch-family "Merriweather")
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
  ;; Track which frames have had fonts applied
  (defvar cj/fontaine-configured-frames nil
	"List of frames that have had fontaine configuration applied.")

  (defun cj/apply-font-settings-to-frame (&optional frame)
    "Apply font settings to FRAME if not already configured.
If FRAME is nil, uses the selected frame."
	(let ((target-frame (or frame (selected-frame))))
	  (unless (member target-frame cj/fontaine-configured-frames)
		(with-selected-frame target-frame
		  (when (display-graphic-p target-frame)
			(fontaine-set-preset 'default)
			(push target-frame cj/fontaine-configured-frames))))))

  (defun cj/cleanup-frame-list (frame)
	"Remove FRAME from the configured frames list when deleted."
	(setq cj/fontaine-configured-frames
		  (delq frame cj/fontaine-configured-frames)))

  ;; Handle daemon mode and regular mode
  (if (daemonp)
	  (progn
		;; Apply to each new frame in daemon mode
		(add-hook 'server-after-make-frame-hook #'cj/apply-font-settings-to-frame)
		;; Clean up deleted frames from tracking list
		(add-hook 'delete-frame-functions #'cj/cleanup-frame-list))
	;; Apply immediately in non-daemon mode
	(when (display-graphic-p)
	  (cj/apply-font-settings-to-frame))))

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
  ;; Check for font installation after frame creation
  (defun cj/maybe-install-all-the-icons-fonts (&optional _frame)
	"Install all-the-icons fonts if needed and we have a GUI."
	(when (and (display-graphic-p)
			   (not (cj/font-installed-p "all-the-icons")))
	  (all-the-icons-install-fonts t)
	  ;; Remove this hook after successful installation
	  (remove-hook 'server-after-make-frame-hook #'cj/maybe-install-all-the-icons-fonts)))

  ;; Handle both daemon and non-daemon modes
  (if (daemonp)
	  (add-hook 'server-after-make-frame-hook #'cj/maybe-install-all-the-icons-fonts)
	(cj/maybe-install-all-the-icons-fonts)))

(use-package all-the-icons-nerd-fonts
  :after all-the-icons
  :demand t
  :config
  (all-the-icons-nerd-fonts-prefer))

;; -------------------------------- Nerd Icons ---------------------------------
;; Modern icon fonts for UI elements

(use-package nerd-icons
  :defer .5)

;; ----------------------------- Emoji Fonts Per OS ----------------------------
;; Set emoji fonts in priority order (first found wins)

(cond
 ;; Prefer Noto Color Emoji (Linux)
 ((member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))
 ;; Then Apple Color Emoji (macOS)
 ((member "Apple Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
 ;; Finally Segoe UI Emoji (Windows)
 ((member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend)))

;; ---------------------------------- Emojify ----------------------------------
;; converts emoji identifiers into emojis; allows for easy emoji entry.

(use-package emojify
  :defer 1
  :hook ((erc-mode . emojify-mode)
		 (org-mode . emojify-mode))
  :custom
  (emojify-download-emojis-p t) ;; don't ask, just download emojis
  :bind
  ("C-c E i" . emojify-insert-emoji)  ;; emoji insert
  ("C-c E l" . emojify-list-emojis)   ;; emoji list
  :config
  (setq emojify-show-help nil)
  (setq emojify-point-entered-behaviour 'uncover)
  (setq emojify-display-style 'image)
  (setq emojify-emoji-styles '(ascii unicode github))

  ;; Disable emojify in programming and gptel modes
  (defun cj/disable-emojify-mode ()
	"Disable emojify-mode in the current buffer."
	(emojify-mode -1))

  (add-hook 'prog-mode-hook #'cj/disable-emojify-mode)
  (add-hook 'gptel-mode-hook #'cj/disable-emojify-mode))

;; -------------------------- Display Available Fonts --------------------------
;; display all available fonts on the system in a side panel

(defun cj/display-available-fonts ()
  "Display a list of all font faces with sample text in another read-only buffer."
  (interactive)
  (pop-to-buffer "*Available Fonts*"
				 '(display-buffer-in-side-window . ((side . right)(window-width . fit-window-to-buffer))))
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

(keymap-global-set "C-z F" #'cj/display-available-fonts)

;; ----------------------- Increase / Decrease Font Size -----------------------
;; make it easy to enlarge or shrink font sizes with keybindings

(setq text-scale-mode-step 1.08)
(keymap-global-set "C-+" #'text-scale-increase)
(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "C-_" #'text-scale-decrease)
(keymap-global-set "C--" #'text-scale-decrease)

;; --------------------------------- Ligatures ---------------------------------
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

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c E" "emojify menu"
    "C-c E i" "insert emoji"
    "C-c E l" "list emojis"))

(provide 'font-config)
;;; font-config.el ends here
