;;; keybindings.el --- General Keyboard Shortcuts -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 1 (Foundation).
;; Category: F/C.
;; Load shape: eager.
;; Eager reason: owns `cj/custom-keymap' and the global C-; prefix that feature
;;   modules register into; must exist before they load.
;; Top-level side effects: defines `cj/custom-keymap'/`cj/jump-map', binds
;;   global keys (C-;, C-z, and others), registers which-key labels after-load.
;; Runtime requires: user-constants (currently eval-when-compile only),
;;   which-key, free-keys.
;; Direct test load: conditional (binds global keys; needs user-constants).
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
;; C-; is GUI-only; terminals can't encode Control-semicolon.  Mirror the same
;; keymap under C-c ; (the standard user prefix, always TTY-encodable) so the
;; whole command family works in a terminal frame with no leaf-key relearning.
(keymap-global-set "C-c ;" cj/custom-keymap)

;; ------------------------ Custom Keymap Registration -------------------------

;; Feature modules register into the C-; prefix through these helpers rather
;; than mutating `cj/custom-keymap' directly.  This keeps keybindings.el the
;; sole owner of the prefix and removes each module's hidden assumption that
;; the keymap already exists.  KEY is a `keymap-set'-style key relative to the
;; C-; prefix (e.g. "c" binds C-; c).  Modules must (require 'keybindings).

(defun cj/register-prefix-map (key map &optional label)
  "Bind prefix keymap MAP under KEY within `cj/custom-keymap'.
When LABEL is non-nil, register it as the which-key description for the
\"C-; KEY\" prefix once which-key loads."
  (keymap-set cj/custom-keymap key map)
  (when label
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements (concat "C-; " key) label))))

(defun cj/register-command (key command &optional label)
  "Bind COMMAND under KEY within `cj/custom-keymap'.
When LABEL is non-nil, register it as the which-key description for the
\"C-; KEY\" key once which-key loads."
  (keymap-set cj/custom-keymap key command)
  (when label
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements (concat "C-; " key) label))))

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

;; Bind the prefix to custom keymap
(keymap-set cj/custom-keymap "j" cj/jump-map)

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; j" "jump to files menu"
    "C-; j r" "jump to reference"
    "C-; j s" "jump to schedule"
    "C-; j i" "jump to inbox"
    "C-; j c" "jump to contacts"
    "C-; j m" "jump to macros"
    "C-; j n" "jump to reading notes"
    "C-; j w" "jump to webclipped"
    "C-; j g" "jump to gcal"
    "C-; j I" "jump to emacs init"))

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
(keymap-global-set  "C-z" (make-sparse-keymap)) ;; replace suspend-frame with prefix map
(keymap-global-unset  "M-o")       ;; facemenu-mode

;; Add commonly-used general keybindings
(keymap-global-set  "M-*"     #'calculator)
(keymap-global-set  "M-S-y" #'yank-media)  ;; was M-Y, overrides yank-pop

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
