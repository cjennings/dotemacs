;;; flyspell-and-abbrev.el --- Spell Check Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P.
;; Load shape: eager.
;; Eager reason: text-mode spelling and abbrev hooks.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: cl-lib.
;; Direct test load: yes.
;;
;; On-demand Flyspell workflow with automatic abbrev creation from accepted
;; corrections. C-' checks/corrects nearby misspellings; C-c f toggles Flyspell
;; with mode-aware behavior.
;;
;; Flyspell is not enabled globally. Programming buffers check comments/strings
;; when enabled; prose buffers check all text. The default C-; Flyspell binding
;; is intentionally left free for cj/custom-keymap.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(eval-when-compile (defvar org-dir))
(defvar flyspell-mode)
(defvar ispell-list-command)
(defvar ispell-skip-region-alist)
(declare-function flyspell-overlay-p "flyspell")
(declare-function flyspell-auto-correct-previous-hook "flyspell")
(declare-function flyspell-correct-at-point "flyspell-correct")
(declare-function flyspell-buffer "flyspell")
(declare-function flyspell-prog-mode "flyspell")
(declare-function thing-at-point "thingatpt")

;; ----------------------------------- Abbrev ----------------------------------

;; Defer abbrev configuration until first use
(with-eval-after-load 'abbrev
  (setq abbrev-file-name (concat user-emacs-directory "assets/abbrev_defs"))
  (setq save-abbrevs 'silently))

;; Enable abbrev-mode by default (lightweight, okay to do eagerly)
(setq-default abbrev-mode t)

;; ---------------------------- Ispell And Flyspell ----------------------------

(use-package ispell
  :ensure nil ;; built-in
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  ;; Use setq consistently (setopt requires Emacs 29+)
  (setq text-mode-ispell-word-completion nil)
  (setq ispell-alternate-dictionary nil)

  (setq ispell-dictionary "american") ; better for aspell
  ;; use aspell rather than ispell
  (setq ispell-program-name "aspell")
  ;; aspell is in /usr/local/ on BSD
  (cond ((eq system-type 'berkeley-unix)
		 (setq ispell-program-name "/usr/local/bin/aspell")))

  ;; in aspell "-l" means --list, not --lang
  (setq ispell-list-command "--list")
  (setq ispell-extra-args '("--sug-mode=ultra" "-W" "3" "--lang=en_US"))
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
		'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[''']"
		   t ;; Many other characters
		   ("-d" "en_US") nil utf-8)))
  ;; personal directory goes with sync'd files
  (setq ispell-personal-dictionary
		(concat org-dir "aspell-personal-dictionary"))
  ;; Skip code blocks in org mode.  The # must be literal and the + escaped:
  ;; "#+" in regex means one-or-more #, which matches no real begin_src line,
  ;; so ispell used to spell-check inside every org code block.
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(use-package flyspell
  :ensure nil ;; built-in
  :commands (flyspell-mode flyspell-prog-mode flyspell-buffer)
  :config
  ;; unset keybinding as we're using it for cj/custom keymap
  (keymap-unset flyspell-mode-map "C-;")
  ;; don't print message for every word when checking
  (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :commands flyspell-correct-at-point)

;; ------------------------------ Flyspell Toggle ------------------------------
;; easy toggling flyspell and also leverage the 'for-buffer-type' functionality.

(defconst cj/--spell-checker-executables
  '("aspell" "ispell" "hunspell")
  "Spell-checker executables `cj/--require-spell-checker' looks for.")

(defun cj/--require-spell-checker ()
  "Signal `user-error' unless a known spell-checker is on PATH.
Looked-up executables: `cj/--spell-checker-executables'.  Single point
of change when a new checker (e.g. nuspell) is added."
  (unless (cl-some #'executable-find cj/--spell-checker-executables)
    (user-error
     "No spell checker found. Install one of: %s"
     (mapconcat #'identity cj/--spell-checker-executables ", "))))

(defun cj/flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.

When turning on, it uses `cj/flyspell-on-for-buffer-type' so code-vs-text is
handled appropriately."
  (interactive)
  (cj/--require-spell-checker)
  (if (bound-and-true-p flyspell-mode)
      (progn ; flyspell is on, turn it off
        (flyspell-mode -1)
        (message "Flyspell off"))
    ;; else - flyspell is off, turn it on
    (cj/flyspell-on-for-buffer-type)
    (message "Flyspell on")))

;; ------------------------ Flyspell On For Buffer Type ------------------------
;; check strings and comments in prog mode; check everything in text mode

(defun cj/flyspell-on-for-buffer-type ()
  "Enable Flyspell for the major mode and check the current buffer.

If flyspell is already enabled, do nothing.  If the mode is derived from
`prog-mode', enable `flyspell-prog-mode' so only strings and comments get
checked.  If the buffer is text based `flyspell-mode' is enabled to check
all text."
  (interactive)
  (unless (bound-and-true-p flyspell-mode) ; if not already on
    (cond
     ((derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
      (flyspell-buffer))
     ((derived-mode-p 'text-mode)
      (flyspell-mode 1)
      (flyspell-buffer)))))

;; Note: NOT auto-enabling on hooks - user activates manually with C-' or C-c f
;; (add-hook 'after-change-major-mode-hook 'cj/flyspell-on-for-buffer-type)
;; (add-hook 'find-file-hook 'cj/flyspell-on-for-buffer-type)

;; ---------------------------- Flyspell Then Abbrev ---------------------------
;; Spell check the buffer and create abbrevs to avoid future misspellings.

(defun cj/find-previous-flyspell-overlay (position)
  "Locate the Flyspell overlay immediately previous to a given POSITION."
  (require 'cl-lib)
  ;; Sort overlays into position order (non-destructive copy)
  (let ((overlay-list (sort (cl-copy-list (overlays-in (point-min) position))
							(lambda (a b)
							  (> (overlay-start a) (overlay-start b))))))
    ;; Search for previous flyspell overlay
	(while (and overlay-list
				(or (not (flyspell-overlay-p (car overlay-list)))
					;; Check if its face has changed
					(not (eq (get-char-property
							  (overlay-start (car overlay-list)) 'face)
							 'flyspell-incorrect))))
	  (setq overlay-list (cdr overlay-list)))
    ;; If no previous overlay exists, return nil
    (when overlay-list
      ;; Otherwise, return the overlay start position
      (overlay-start (car overlay-list)))))


(defun cj/flyspell-goto-previous-misspelling (position)
  "Go to the first misspelled word before the given POSITION.
Return the misspelled word if found or nil if not. Leave the point at the
beginning of the misspelled word. Setting the hook on pre-command ensures that
any started Flyspell corrections complete before running other commands in the
buffer."
  (interactive "d")
  (require 'thingatpt)  ;; Lazy-load only when function is called
  (add-hook 'pre-command-hook
            (function flyspell-auto-correct-previous-hook) t t)
  (let* ((overlay-position (cj/find-previous-flyspell-overlay position))
         (misspelled-word (when overlay-position
                            (goto-char overlay-position)
                            (thing-at-point 'word))))
    (if misspelled-word
        (downcase misspelled-word)
      nil)))

(defun cj/flyspell-then-abbrev (p)
  "Find and correct the previous misspelled word, creating an abbrev.

Finds the nearest misspelled word before point, prompts for correction,
and creates an abbrev so the misspelling auto-expands in the future.

With prefix argument P, the abbrev is created in the local abbrev table.
Without prefix argument, it's created in the global abbrev table.

Press C-' repeatedly to step through misspellings one at a time."
  (interactive "P")
  (cj/--require-spell-checker)
  ;; Enable Flyspell for the buffer type so the mode sticks and the buffer is
  ;; scanned once.  A bare flyspell-buffer here never turned the mode on, so
  ;; the guard never tripped and every C-' press re-scanned the whole buffer.
  (cj/flyspell-on-for-buffer-type)

  (let ((misspelled-word (cj/flyspell-goto-previous-misspelling (point))))
    (if (not misspelled-word)
        (message "No misspellings found before point")
      ;; Found a misspelling, correct it
      (call-interactively 'flyspell-correct-at-point)
      (let ((corrected-word (downcase (or (thing-at-point 'word) ""))))
        (when (and misspelled-word corrected-word
                   (not (string= corrected-word misspelled-word)))
          (message "\"%s\" now expands to \"%s\" %sally"
                   misspelled-word corrected-word (if p "loc" "glob"))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            misspelled-word corrected-word))))))

;; -------------------------------- Keybindings --------------------------------
;; Global keybindings for spell checking commands

;; Set global keybindings
(keymap-set global-map "C-c f" #'cj/flyspell-toggle)
(keymap-set global-map "C-'" #'cj/flyspell-then-abbrev)

;; Override org-mode's C-' binding (org-cycle-agenda-files)
;; Org-mode binds C-' globally, but we want our spell check binding instead
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-'" #'cj/flyspell-then-abbrev))

;; which-key labels
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c f" "flyspell toggle"
    "C-'" "flyspell then abbrev"))

(provide 'flyspell-and-abbrev)
;;; flyspell-and-abbrev.el ends here.
