;;; prog-shell --- Shell Programming Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;

;;; Code:

(use-package sh-script
  :defer .5
  :hook (sh-mode . flycheck-mode))

(provide 'prog-shell)
;;; prog-shell.el ends here
