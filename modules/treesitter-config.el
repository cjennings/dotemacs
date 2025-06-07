;;; treesitter-config.el --- Treesitter Code Highlighting Configuration -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:

;; Tree-sitter's now built into Emacs 29+

;;; Code:


;; ------------------ Installation And Configuration -----------------

(use-package tree-sitter
  :defer .5)

;; ----------------------- Grammar Installation ----------------------
;; installs tree-sitter grammars if they're absent

(use-package treesit-auto
  :defer .5
  :custom
  (treesit-auto-install t)
;;  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'treesitter-config)
;;; treesitter-config.el ends here.
