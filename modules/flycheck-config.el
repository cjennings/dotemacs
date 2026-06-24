;;; flycheck-config --- Syntax/Grammar Check -*- lexical-binding: t; coding: utf-8; -*-
;;  author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P.
;; Load shape: eager.
;; Eager reason: general linting setup; spec target is hook-loaded, a deferral
;;   candidate.
;; Top-level side effects: package configuration via use-package, binds into
;;   cj/custom-keymap through use-package :map.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; This file configures Flycheck for on-demand syntax and grammar checking.
;; - Flycheck starts automatically only in sh-mode and emacs-lisp-mode

;; - This binds a custom helper (=cj/flycheck-list-errors=) to "C-; ?"
;;   for popping up Flycheck's error list in another window.

;; - It also customizes Checkdoc to suppress only the "sentence-end-double-space"
;;   and "warn-escape" warnings.

;; - It registers LanguageTool for comprehensive grammar checking of prose files
;;   (text-mode, markdown-mode, gfm-mode, org-mode).

;; Note: Grammar checking is on-demand only to avoid performance issues.
;; Hitting "C-; ?" runs cj/flycheck-prose-on-demand if in an org buffer.

;; The cj/flycheck-prose-on-demand function:
;; - Turns on flycheck for the local buffer
;; - Enables LanguageTool checker
;; - Triggers an immediate check
;; - Displays errors in the *Flycheck errors* buffer

;; Installation:
;; On Arch Linux:
;;   sudo pacman -S languagetool
;;
;; The wrapper script at scripts/languagetool-flycheck formats LanguageTool's
;; JSON output into flycheck-compatible format.  It requires Python 3.

;;; Code:

(require 'keybindings) ;; provides cj/custom-keymap (use-package :map below)

;; ------------------------------- Declarations --------------------------------

(declare-function flycheck-mode "flycheck")
(declare-function flycheck-list-errors "flycheck")
(declare-function flycheck-add-mode "flycheck")
(declare-function flycheck-buffer "flycheck")
(declare-function cj/flycheck-prose-on-demand "flycheck-config")

(defun cj/prose-helpers-on ()
  "Ensure that `abbrev-mode' and `flycheck-mode' are on in the current buffer."
  (interactive)
  (unless (bound-and-true-p abbrev-mode)
    (abbrev-mode 1))
  (unless (bound-and-true-p flycheck-mode)
    (flycheck-mode 1)))

;; ---------------------------------- Linting ----------------------------------

(use-package flycheck
  :defer t
  :commands (flycheck-list-errors
             cj/flycheck-list-errors)
  :hook ((sh-mode emacs-lisp-mode) . flycheck-mode)
  :bind
   (:map cj/custom-keymap
			  ("?" . cj/flycheck-list-errors))
  :custom
  ;; Only disable these two Checkdoc warnings; leave all others intact.
  (checkdoc-arguments
   '(("sentence-end-double-space" nil)
     ("warn-escape"               nil)))
  ;; Modeline customization (rendered via mode-line-format in modeline-config.el).
  ;; The count portion picks up `error' / `warning' faces because
  ;; `flycheck-mode-line-color' stays t (the default).
  (flycheck-mode-line-prefix "🐛")
  (flycheck-mode-success-indicator " ✓")
  :config

  ;; use the load-path of the currently running Emacs instance
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Define LanguageTool checker for comprehensive grammar checking.
  ;; The :command executable must be a string literal at macro-expansion
  ;; time (flycheck rejects `(eval ...)' in the first position), so we
  ;; backquote-splice the expanded path into the form and eval it
  ;; explicitly.  Survives a non-standard `user-emacs-directory'.
  (eval
   `(flycheck-define-checker languagetool
      "A grammar checker using LanguageTool.
Uses a wrapper script to format output for flycheck."
      :command (,(expand-file-name "scripts/languagetool-flycheck"
                                   user-emacs-directory)
                source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": "
                (message) line-end))
      :modes (text-mode markdown-mode gfm-mode org-mode)))
  (add-to-list 'flycheck-checkers 'languagetool)

  (defun cj/flycheck-list-errors ()
    "Display flycheck's error list and switch to its buffer.
Runs flycheck-prose-on-demand if in an org-buffer."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (cj/flycheck-prose-on-demand))

    (flycheck-list-errors)
    (switch-to-buffer-other-window "*Flycheck errors*"))

  (defun cj/flycheck-prose-on-demand ()
    "Enable Flycheck with LanguageTool in this buffer, run it, and show errors."
    (interactive)
    ;; turn on Flycheck locally
    (flycheck-mode 1)
    ;; ensure LanguageTool is valid for current mode
    (flycheck-add-mode 'languagetool major-mode)
    ;; select LanguageTool as the checker
    (setq-local flycheck-checker 'languagetool)
    ;; trigger immediate check
    (flycheck-buffer)))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-; ?" "list errors"))

(provide 'flycheck-config)
;;; flycheck-config.el ends here
