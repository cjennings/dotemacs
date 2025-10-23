;;; keybindings --- General Keyboard Shortcuts -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Global keybinding configuration and custom keymap framework.
;;
;; Main features include:
;; - custom keymap prefix  C-; for all custom commands,
;; - jump-to-file commands C-c j <key> for frequently used files
;; - which-key integration for keybinding discovery
;; - free-keys for finding available keybindings
;;
;; Key principles:
;; - avoid keybindings close to commonly-used keys that have painful results
;;
;;; Code:

;; Loaded earlier in init.el
(eval-when-compile (require 'user-constants))

;; ------------------------------- Custom Keymap -------------------------------

(defvar-keymap cj/custom-keymap
  :doc "User custom prefix keymap base for nested keymaps.")
(keymap-global-set "C-;" cj/custom-keymap)

;; ------------------------------ Jump To Commands -----------------------------

(defun cj/jump-open-var (var)
  "Open the file whose path is stored in VAR.
Errors if VAR is unbound, not a non-empty string, or the file does not exist."
  (unless (boundp var)
	(user-error "Variable %s is not bound" var))
  (let ((path (symbol-value var)))
	(unless (and (stringp path) (> (length path) 0))
	  (user-error "Variable %s does not contain a valid file path" var))
	(unless (file-exists-p path)
	  (user-error "File does not exist: %s" path))
	(find-file path)))

(defconst cj/jump--specs
  '(("r" reference      reference-file)
	("s" schedule       schedule-file)
	("i" inbox          inbox-file)
	("c" contacts       contacts-file)
	("m" macros         macros-file)
	("n" reading-notes  reading-notes-file)
	("w" webclipped     webclipped-file)
	("g" gcal           gcal-file)
	("I" emacs-init     emacs-init-file))
  "Specs for jump commands: each entry is (KEY NAME-SYM VAR-SYM).")

(defvar-keymap cj/jump-map
  :doc "Key map for quick jumps to commonly used files.")

;; Define commands and populate the keymap from the specs.
(dolist (spec cj/jump--specs)
  (pcase-let ((`(,key ,name ,var) spec))
	(let* ((fn (intern (format "cj/jump-to-%s" name)))
		   (doc (format "Open the file from variable `%s'." var)))
	  ;; Define a named command that opens the file from VAR.
	  (defalias fn
		`(lambda ()
		   ,doc
		   (interactive)
		   (cj/jump-open-var ',var)))
	  ;; Bind it under the prefix map.
	  (keymap-set cj/jump-map key fn))))

;; Bind the prefix globally (user-reserved prefix).
(keymap-global-set "C-c j" cj/jump-map)

;; nicer prefix label in which-key
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c j" "Jump to common files."))

;; ---------------------------- Keybinding Discovery ---------------------------

(use-package free-keys
  :commands (free-keys)
  :bind (:map help-map
		 ("C-k" . free-keys)))

(use-package which-key
  :commands (which-key-mode)
  :hook (emacs-startup . which-key-mode)
  :custom
  (which-key-idle-delay 1.0)
  (which-key-popup-type 'side-window)
  :config
  (which-key-setup-side-window-bottom)
  ;; never show keybindings that have been 'cj/disabled'
  (push '((nil . "cj/disabled") . t) which-key-replacement-alist))

;; ---------------------------- General Keybindings ----------------------------

;; Avoid hostile bindings
(keymap-global-unset  "C-x C-f")   ;; find-file-read-only
(keymap-global-set  "C-x C-f" #'find-file)
(keymap-global-unset  "C-z")       ;; suspend-frame is accidentally hit often
(keymap-global-unset  "M-o")       ;; facemenu-mode

;; Add commonly-used general keybindings
(keymap-global-set  "M-*"     #'calculator)
(keymap-global-set  "M-Y"     #'yank-media)

;; Normally bound to ESC ESC ESC, hit ESC once to get out of unpleasant situations.
(keymap-global-set  "<escape>" #'keyboard-escape-quit)

;; remap C-x \ to sort-lines (from remap activate-transient-input-method)
(keymap-global-unset  "C-x \\")
(keymap-global-set  "C-x \\" #'sort-lines)

;; training myself to use C-/ for undo (bound internally) as it's faster.
(keymap-global-unset  "C-x u")
(keymap-global-set "C-x u"
			#'(lambda () (interactive)
				(message (concat "Seriously, " user-name
								 "? Use 'C-/'. It's faster."))))

(provide 'keybindings)
;;; keybindings.el ends here
