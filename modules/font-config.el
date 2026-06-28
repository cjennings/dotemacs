;;; font-config --- Font Defaults and Related Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P/S.
;; Load shape: eager.
;; Eager reason: first-frame font setup and font keybindings.
;; Top-level side effects: font keys, font checks, package config.
;; Runtime requires: host-environment, keybindings.
;; Direct test load: yes.
;;
;; Configures fontaine presets, text scaling keys, icon/emoji fonts, and
;; programming ligatures. Presets are applied per frame so daemon clients get
;; the intended fixed/variable pitch sizes.
;;
;; Also carries font-rendering safeguards for known HarfBuzz/font-cache crashes
;; triggered by emoji and Arabic shaping in this setup.

;;; Code:

(require 'host-environment)
(require 'keybindings)  ;; establishes the C-z prefix used for "C-z F" below

(defvar text-scale-mode-step)
(declare-function cj/disable-emojify-mode "font-config")

;; ---------------------- HarfBuzz Font Cache Crash Fix -----------------------
;; Prevents Emacs from compacting font caches during GC. Without this, GC can
;; free font cache entries that HarfBuzz still references, causing SIGSEGV
;; crashes during glyph shaping (e.g., rendering emoji in mu4e headers).
;; See: Emacs bug#12746, coredump traces through hb_shape_full.

(setq inhibit-compacting-font-caches t)

;; --------------- Disable Arabic Shaping (Prevents HarfBuzz SIGSEGV) ---------
;; Emacs 30.2 + HarfBuzz crashes (SIGSEGV in hb_shape_full) when
;; arabic-shape-gstring is called on emoji characters in mu4e headers.
;; Since bidi display is already disabled in early-init.el and Arabic text
;; shaping is not needed, remove it from the composition function table.
;; This prevents HarfBuzz from ever using the Arabic shaper. Programming
;; ligatures and emoji rendering use different shapers and are unaffected.

(with-eval-after-load 'misc-lang
  (dolist (range '((#x0600 . #x06FF)    ;; Arabic
                   (#x0750 . #x077F)    ;; Arabic Supplement
                   (#x08A0 . #x08FF)    ;; Arabic Extended-A
                   (#xFB50 . #xFDFF)    ;; Arabic Presentation Forms-A
                   (#xFE70 . #xFEFF)))  ;; Arabic Presentation Forms-B
    (set-char-table-range composition-function-table range nil)))

;; ----------------------- Font Family And Size Selection ----------------------
;; preset your fixed and variable fonts, then apply them to text as a set

(use-package fontaine
  :demand t
  :bind
  ("M-S-f" . fontaine-set-preset)  ;; was M-F, overrides forward-word
  :config
  (setq fontaine-presets
		`(
		  (default
		   :default-family "BerkeleyMono Nerd Font"
		   :default-weight regular
		   :default-height ,(if (env-laptop-p) 120 140)
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
           :default-family "Lexend"
           :default-weight regular
           :default-height 200
		   :variable-pitch-family "Lexend")
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
		   :default-height 120
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

;; Track which frames have had fonts applied
(defvar cj/fontaine-configured-frames nil
  "List of frames that have had fontaine configuration applied.")

(declare-function fontaine-set-preset "fontaine")

(defun cj/apply-font-settings-to-frame (&optional frame)
  "Apply font settings to FRAME if not already configured.
If FRAME is nil, uses the selected frame."
  (let ((target-frame (or frame (selected-frame))))
    (unless (member target-frame cj/fontaine-configured-frames)
      (with-selected-frame target-frame
        (when (env-gui-p)
          (fontaine-set-preset 'default)
          (push target-frame cj/fontaine-configured-frames))))))

(defun cj/cleanup-frame-list (frame)
  "Remove FRAME from the configured frames list when deleted."
  (setq cj/fontaine-configured-frames
        (delq frame cj/fontaine-configured-frames)))

(with-eval-after-load 'fontaine
  ;; Handle daemon mode and regular mode
  (if (daemonp)
      (progn
        ;; Apply to each new frame in daemon mode
        (add-hook 'server-after-make-frame-hook #'cj/apply-font-settings-to-frame)
        ;; Clean up deleted frames from tracking list
        (add-hook 'delete-frame-functions #'cj/cleanup-frame-list))
    ;; Apply immediately in non-daemon mode
    (when (env-gui-p)
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

(declare-function all-the-icons-install-fonts "all-the-icons")

(defun cj/maybe-install-all-the-icons-fonts (&optional _frame)
  "Install all-the-icons fonts if needed and we have a GUI."
  (when (and (env-gui-p)
             (not (cj/font-installed-p "all-the-icons")))
    (all-the-icons-install-fonts t)
    ;; Remove this hook after successful installation
    (remove-hook 'server-after-make-frame-hook #'cj/maybe-install-all-the-icons-fonts)))

(use-package all-the-icons
  :demand t
  :config
  ;; Handle both daemon and non-daemon modes
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'cj/maybe-install-all-the-icons-fonts)
    (cj/maybe-install-all-the-icons-fonts)))

(use-package all-the-icons-nerd-fonts
  :after all-the-icons
  :demand t
  :config
  (all-the-icons-nerd-fonts-prefer))

;; ----------------------------- Emoji Fonts Per OS ----------------------------
;; Set emoji fonts in priority order (first found wins)

(defun cj/setup-emoji-fontset (&optional _frame)
  "Set emoji fonts in priority order (first found wins).
No-op unless a GUI frame is present.  Safe to run per-frame: setting
the fontset repeatedly is harmless, so it can be called from
`server-after-make-frame-hook' in daemon mode."
  (when (env-gui-p)
    (cond
     ;; Prefer Noto Color Emoji (Linux)
     ((member "Noto Color Emoji" (font-family-list))
      (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))
     ;; Then Apple Color Emoji (macOS)
     ((member "Apple Color Emoji" (font-family-list))
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
     ;; Finally Segoe UI Emoji (Windows)
     ((member "Segoe UI Emoji" (font-family-list))
      (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend)))))

;; In daemon mode `env-gui-p' is nil at load time (no GUI frame yet), so run
;; the setup per-frame as GUI frames are created.  Otherwise run it now.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'cj/setup-emoji-fontset)
  (cj/setup-emoji-fontset))

;; ---------------------------------- Emojify ----------------------------------
;; converts emoji identifiers into emojis; allows for easy emoji entry.

(use-package emojify
  :defer 1
  :hook ((erc-mode . emojify-mode))
  :custom
  (emojify-download-emojis-p t) ;; don't ask, just download emojis
  :bind
  ("C-c E i" . emojify-insert-emoji)  ;; emoji insert
  ("C-c E l" . emojify-list-emojis)   ;; emoji list
  :config
  (setq emojify-show-help nil)
  (setq emojify-point-entered-behaviour 'uncover)
  (setq emojify-display-style (if (env-gui-p) 'image 'unicode))
  (setq emojify-emoji-styles '(ascii unicode github))

  ;; Disable emojify in programming modes
  (defun cj/disable-emojify-mode ()
	"Disable emojify-mode in the current buffer."
	(emojify-mode -1))

  (add-hook 'prog-mode-hook #'cj/disable-emojify-mode))

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
		(insert (propertize (concat font-family) 'face '(font-lock-keyword-face (:weight bold))))
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
