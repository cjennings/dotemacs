;;; prog-training.el --- Training -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:


;; ----------------------------- Exercism ----------------------------

(use-package exercism
  :commands (exercism)
  :bind
  ("C-h E" . exercism))


;;; ----------------------------- Leetcode ----------------------------

(use-package leetcode
  :commands (leetcode)
  :custom
  (url-debug t)
  :config
  (setq leetcode-prefer-language "golang")
  (setq leetcode-directory "~/code/leetcode")
  (setq leetcode-save-solutions t))


(provide 'prog-training)
;;; prog-training.el ends here.
