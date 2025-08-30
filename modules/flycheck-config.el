;;; flycheck-config --- Syntax/Grammar Check -*- lexical-binding: t; -*-
;;  author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; This file configures Flycheck for on-demand syntax and grammar checking.
;; - Flycheck starts automatically only in sh-mode and emacs-lisp-mode

;; - This binds a custom helper (=cj/flycheck-list-errors=) to “C-; ?”
;;   for popping up Flycheck's error list in another window.

;; - It also customizes Checkdoc to suppress only the “sentence-end-double-space”
;;   and “warn-escape” warnings.

;; - It registers a Proselint checker for prose files
;;   (text-mode, markdown-mode, gfm-mode).

;; Note: I do use proselint quite a bit in emails and org-mode files. However, some
;; org-files can be large and running proselint on them will slow Emacs to a crawl.
;; Therefore, hitting "C-; ?" also runs cj/flycheck-prose-on-demand if in an org buffer.
;;
;; The cj/flycheck-prose-on-demand function:
;; - Turns on flycheck for the local buffer
;; - ensures proselint is added
;; - triggers an immediate check
;;
;; Since this is called within cj/flycheck-list-errors, flycheck's error list will still
;; display and the focus transferred to that buffer.

;; OS Dependencies:
;; proselint (in the Arch AUR)

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
  :bind (("C-; ?" . cj/flycheck-list-errors))
  :custom
  ;; Only disable these two Checkdoc warnings; leave all others intact.
  (checkdoc-arguments
   '(("sentence-end-double-space" nil)
     ("warn-escape"               nil)))
  :config

  ;; Define the prose checker (installed separately via OS).
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint)

  (defun cj/flycheck-list-errors ()
    "Display flycheck's error list and switch to its buffer.
Runs flycheck-prose-on-demand if in an org-buffer."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (cj/flycheck-prose-on-demand))

    (flycheck-list-errors)
    (switch-to-buffer-other-window "*Flycheck errors*"))

  (defun cj/flycheck-prose-on-demand ()
    "Enable Flycheck+Proselint in this buffer, run it, and show errors."
    (interactive)
    ;; turn on Flycheck locally
    (flycheck-mode 1)
    ;; ensure proselint is valid for org/text
    (flycheck-add-mode 'proselint major-mode)
    ;; trigger immediate check
    (flycheck-buffer)))

(provide 'flycheck-config)
;;; flycheck-config.el ends here
