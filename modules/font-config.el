;;; font-config.el --- Font Defaults and Related Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P/S.
;; Load shape: eager.
;; Eager reason: first-frame font setup and font keybindings.
;; Top-level side effects: font keys, font checks, package config.
;; Runtime requires: host-environment, font-profiles, keybindings.
;; Direct test load: yes.
;;
;; Configures task-oriented Fontaine profiles, text scaling keys, icon/emoji
;; fonts, and programming ligatures. The selected profile is global, persists
;; across restarts, and applies to every daemon frame without per-frame resets.
;;
;; Also carries font-rendering safeguards for known HarfBuzz/font-cache crashes
;; triggered by emoji and Arabic shaping in this setup.

;;; Code:

(require 'host-environment)
(require 'font-profiles)
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

;; ------------------------- Workflow Font Profiles ----------------------------
;; Each choice is a complete destination.  Font size adjustments within one
;; buffer remain on C-+/C--; Fontaine owns the global workflow typography.

(defconst cj/fontaine-profile-order
  cj/font-profile-order
  "Fontaine profiles in picker order.")

(defconst cj/fontaine-profile-names
  '((everyday . "Everyday")
    (writing . "Writing")
    (reading . "Reading")
    (coding-xs . "Coding XS")
    (coding . "Coding")
    (coding-xl . "Coding XL")
    (presentation . "Presentation"))
  "Human names for Fontaine workflow profiles.")

(defconst cj/fontaine-profile-fonts
  '((everyday . "Berkeley Mono + Lexend")
    (writing . "Berkeley Mono + Merriweather")
    (reading . "Merriweather")
    (coding-xs . "Berkeley Mono")
    (coding . "Berkeley Mono")
    (coding-xl . "Berkeley Mono")
    (presentation . "Berkeley Mono + Lexend"))
  "Human-readable font combinations for Fontaine workflow profiles.")

(defconst cj/fontaine-profile-heights
  (mapcar (lambda (profile)
            (cons profile
                  (plist-get (cj/font-profile-properties profile)
                             :default-height)))
          cj/fontaine-profile-order)
  "Default face heights for Fontaine workflow profiles.")

(defconst cj/fontaine-ui-family "BerkeleyMono Nerd Font"
  "Font family reserved for the mode line, echo area, and minibuffer.")

(defvar fontaine-current-preset)
(defvar fontaine-preset-history)
(defvar fontaine-presets)
(defvar enable-theme-functions)
(defvar cj/fontaine-profile-history nil
  "Minibuffer history for `cj/fontaine-select-profile'.")

(declare-function fontaine-mode "fontaine")
(declare-function fontaine-restore-latest-preset "fontaine")
(declare-function fontaine-set-preset "fontaine")
(declare-function face-remap-set-base "face-remap")

(defun cj/fontaine-profile-p (profile)
  "Return non-nil when PROFILE is a configured workflow profile."
  (cj/font-profile-p profile))

(defun cj/fontaine-profile-label (profile)
  "Return the complete picker label for PROFILE."
  (when (cj/fontaine-profile-p profile)
    (format "%s — %s · %d pt"
            (alist-get profile cj/fontaine-profile-names)
            (alist-get profile cj/fontaine-profile-fonts)
            (/ (alist-get profile cj/fontaine-profile-heights) 10))))

(defun cj/fontaine-profile-candidates ()
  "Return complete labels for all Fontaine workflow profiles."
  (mapcar #'cj/fontaine-profile-label cj/fontaine-profile-order))

(defun cj/fontaine-profile-from-label (label)
  "Return the workflow profile represented by LABEL, or nil."
  (seq-find (lambda (profile)
              (equal label (cj/fontaine-profile-label profile)))
            cj/fontaine-profile-order))

(defun cj/fontaine-profile-annotation (candidate)
  "Mark CANDIDATE when it represents the active Fontaine profile."
  (if (eq (cj/fontaine-profile-from-label candidate)
          fontaine-current-preset)
      "  current"
    ""))

(defun cj/fontaine-apply-profile (profile)
  "Apply workflow PROFILE and record it for Fontaine persistence."
  (unless (cj/fontaine-profile-p profile)
    (user-error "Unknown font profile: %s" profile))
  (add-to-history 'fontaine-preset-history (symbol-name profile))
  (fontaine-set-preset profile))

(defun cj/fontaine-select-profile ()
  "Select and apply one complete Fontaine workflow profile."
  (interactive)
  (let* ((candidates (cj/fontaine-profile-candidates))
         (default (cj/fontaine-profile-label
                   (if (cj/fontaine-profile-p fontaine-current-preset)
                       fontaine-current-preset
                     'everyday)))
         (completion-extra-properties
          '(:annotation-function cj/fontaine-profile-annotation))
         (choice (completing-read "Font profile: " candidates nil t
                                  nil 'cj/fontaine-profile-history default)))
    (cj/fontaine-apply-profile (cj/fontaine-profile-from-label choice))))

(defun cj/fontaine-restored-or-default-profile ()
  "Return the saved Fontaine profile, or the `everyday' fallback."
  (let ((restored (fontaine-restore-latest-preset)))
    (if (cj/fontaine-profile-p restored) restored 'everyday)))

(defalias 'cj/fontaine-profile-properties #'cj/font-profile-properties)
(defalias 'cj/fontaine-remap-buffer-to-profile #'cj/font-profile-remap-buffer)

(defun cj/fontaine-remap-ui-buffer ()
  "Keep the current minibuffer or echo-area buffer in Berkeley Mono."
  (face-remap-set-base
   'default `(:family ,cj/fontaine-ui-family)))

(defun cj/fontaine-keep-ui-chrome-monospace (&rest _ignored)
  "Keep mode-line, minibuffer, and echo-area chrome in Berkeley Mono."
  (dolist (face '(mode-line mode-line-active mode-line-inactive
                  minibuffer-prompt))
    (when (facep face)
      (set-face-attribute face nil :family cj/fontaine-ui-family)))
  (dolist (name '(" *Echo Area 0*" " *Echo Area 1*"))
    (when-let* ((buffer (get-buffer name)))
      (with-current-buffer buffer
        (cj/fontaine-remap-ui-buffer)))))

;; Fontaine 3 is global rather than frame-specific.  Remove the retired hooks
;; as well as omitting them below, so a live module reload migrates cleanly.
(remove-hook 'server-after-make-frame-hook #'cj/apply-font-settings-to-frame)
(remove-hook 'delete-frame-functions #'cj/cleanup-frame-list)

(use-package fontaine
  :demand t
  :bind
  ("M-S-f" . cj/fontaine-select-profile)  ;; was M-F, overrides forward-word
  :config
  (setq fontaine-presets
        (append (copy-tree cj/font-profile-definitions)
                (list (cons t (copy-sequence
                               cj/font-profile-shared-properties)))))
  (fontaine-mode 1)
  (add-hook 'fontaine-set-preset-hook
            #'cj/fontaine-keep-ui-chrome-monospace)
  (add-hook 'enable-theme-functions
            #'cj/fontaine-keep-ui-chrome-monospace)
  (add-hook 'minibuffer-setup-hook #'cj/fontaine-remap-ui-buffer)
  (cj/fontaine-keep-ui-chrome-monospace)
  (when (or (daemonp) (env-gui-p))
    (cj/fontaine-apply-profile (cj/fontaine-restored-or-default-profile))))

;; ----------------------------- Font Install Check ----------------------------
;; convenience function to indicate whether a font is available by name.

(defun cj/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
	  t
	nil))

;; ------------------------------- Nerd Icons fonts ----------------------------
;; nerd-icons (configured in nerd-icons-config.el) renders glyphs from the
;; "Symbols Nerd Font Mono" font.  Auto-install it on the first GUI frame when
;; it is missing -- the same convenience the dropped all-the-icons setup gave.

(declare-function nerd-icons-install-fonts "nerd-icons")

(defun cj/maybe-install-nerd-icons-fonts (&optional _frame)
  "Install the nerd-icons font if it is missing and we have a GUI."
  (when (and (env-gui-p)
             (not (cj/font-installed-p "Symbols Nerd Font Mono")))
    (nerd-icons-install-fonts t)
    ;; Remove this hook after successful installation
    (remove-hook 'server-after-make-frame-hook #'cj/maybe-install-nerd-icons-fonts)))

;; nerd-icons loads after this module (see init.el order), so defer the wiring
;; until it is present.  Daemon: install on the first GUI frame; otherwise now.
(with-eval-after-load 'nerd-icons
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'cj/maybe-install-nerd-icons-fonts)
    (cj/maybe-install-nerd-icons-fonts)))

;; ----------------------------- Emoji Fonts Per OS ----------------------------

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

(defvar emojify-display-style)          ;; emojify's, forward-declared for the helper

(defun cj/set-emojify-display-style ()
  "Set `emojify-display-style' to `image' on a graphical frame, else `unicode'.
Image emoji only render on a GUI frame.  In daemon mode no GUI frame exists when
emojify loads, so this runs per-frame from `server-after-make-frame-hook';
otherwise the value would latch to `unicode' and GUI frames never get images."
  (setq emojify-display-style (if (env-gui-p) 'image 'unicode)))

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
  ;; In daemon mode `env-gui-p' is nil at :config time (no GUI frame yet), so
  ;; recompute the display style per-frame; otherwise set it now.
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'cj/set-emojify-display-style)
    (cj/set-emojify-display-style))
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
	  ;; The buffer is left in `special-mode' (read-only) after the first call,
	  ;; so re-running must relax read-only to erase and rewrite it.
	  (let ((inhibit-read-only t))
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
	  (special-mode))))

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
