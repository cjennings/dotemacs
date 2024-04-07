;;; flycheck-config --- Syntax/Grammar Check -*- lexical-binding: t; -*-
;;  author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

(defun cj/prose-helpers-on ()
  "Ensure that abbrev, flyspell, and flycheck are on."
  (interactive)
  (if (not (abbrev-mode))
	  (abbrev-mode))
  (flyspell-on-for-buffer-type)
  (if (not (flycheck-mode))
	  (flycheck-mode)))

;;;; ---------------------------------- Linting --------------------------------

(use-package flycheck
  :defer .5
  :hook (sh-mode emacs-lisp-mode)
  :bind ("C-; ?" . flycheck-list-errors)
  :config
  ;; don't warn about double-spaces after period.
  (setq-default checkdoc-arguments '("sentence-end-double-space" nil "warn-escape" nil))

  ;; proselint must be installed via the OS
  (flycheck-define-checker proselint
	"A linter for prose."
	:command ("proselint" source-inplace)
	:error-patterns
	((warning line-start (file-name) ":" line ":" column ": "
			  (id (one-or-more (not (any " "))))
			  (message) line-end))
	:modes (text-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint))

;; ;; https://github.com/emacs-grammarly/flycheck-grammarly
;; (use-package flycheck-grammarly
;;   :defer 1
;;   :after flycheck
;;   :config
;;   (with-eval-after-load 'flycheck
;; 	(flycheck-grammarly-setup)))

(provide 'flycheck-config)
;;; flycheck-config.el ends here
