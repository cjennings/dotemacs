;;; prog-training.el --- Training -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;

;;; Code:


;; ----------------------------- Exercism ----------------------------

(use-package exercism
  :defer t
  :commands (exercism)
  :bind
  ("C-h E" . exercism))


;;; ----------------------------- Leetcode ----------------------------

(use-package leetcode
  :defer t
  :commands (leetcode)
  :bind ("C-h L" . leetcode)
  :custom
  (url-debug t)
  :config
  (setq leetcode-prefer-language "golang")
  (setq leetcode-directory (concat code-dir "/leetcode"))
  (setq leetcode-save-solutions t))


(provide 'prog-training)
;;; prog-training.el ends here.
