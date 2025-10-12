;;; prog-training.el --- Training -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; Use C-h E to launch Exercism when you want to fetch or submit practice problems.
;; Use C-h L for LeetCode sessions; the package drops solved files under ~/code/leetcode in Go format.
;; Both bindings autoload their packages, so invoking the key is the whole workflow.

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
