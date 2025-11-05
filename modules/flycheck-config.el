;;; flycheck-config --- Syntax/Grammar Check -*- lexical-binding: t; coding: utf-8; -*-
;;  author Craig Jennings <c@cjennings.net>

;;; Commentary:

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

(defun cj/prose-helpers-on ()
  "Ensure that abbrev, flyspell, and flycheck are all on."
  (interactive)
  (if (not (abbrev-mode))
      (abbrev-mode))
  ;;  (flyspell-on-for-buffer-type)
  (if (not (flycheck-mode))
      (flycheck-mode)))

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
  :config

  ;; use the load-path of the currently running Emacs instance
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Define LanguageTool checker for comprehensive grammar checking
  (flycheck-define-checker languagetool
    "A grammar checker using LanguageTool.
Uses a wrapper script to format output for flycheck."
    :command ("~/.emacs.d/scripts/languagetool-flycheck"
              source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (message) line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
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
