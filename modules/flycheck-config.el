;;; flycheck-config.el --- Syntax/Grammar Check -*- lexical-binding: t; coding: utf-8; -*-
;;  author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/P.
;; Load shape: eager.
;; Eager reason: linting keymap and mode hooks; could become hook-loaded.
;; Top-level side effects: package config and C-; ? binding.
;; Runtime requires: keybindings.
;; Direct test load: yes.
;;
;; Flycheck configuration for automatic shell/Elisp linting and on-demand prose
;; grammar checks. C-; ? opens the Flycheck error list, enabling prose checking
;; first when appropriate.
;;
;; LanguageTool uses scripts/languagetool-flycheck to adapt JSON output to
;; Flycheck's checker protocol.

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
