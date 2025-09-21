;;; prog-shell --- Shell Programming Settings and Functionality -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Open any *.sh buffer and sh-mode loads with Flycheck attached, so syntax errors appear immediately.
;; Re-save or invoke C-c ! l to refresh diagnostics while you iterate on scripts.

;;; Code:

(use-package sh-script
  :defer .5
  :hook (sh-mode . flycheck-mode))

(provide 'prog-shell)
;;; prog-shell.el ends here
