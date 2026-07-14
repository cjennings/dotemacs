;;; prog-training.el --- Training -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; the C-h E / C-h L bindings already autoload their
;;   packages, so the eager require can drop to autoloads in Phase 4.
;; Top-level side effects: package configuration via use-package (autoloaded).
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;; Use C-h E to launch Exercism when you want to fetch or submit practice problems.
;; Use C-h L for LeetCode sessions; the package drops solved files under ~/code/leetcode in Go format.
;; Both bindings autoload their packages, so invoking the key is the whole workflow.

;;; Code:

(defvar code-dir)  ;; user-constants.el; read lazily in leetcode's :config

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
  :config
  ;; No (url-debug t) here: that was a debugging leftover, and it turned on
  ;; GLOBAL url.el request logging for the whole session once leetcode loaded.
  (setq leetcode-prefer-language "golang")
  (setq leetcode-directory (concat code-dir "/leetcode"))
  (setq leetcode-save-solutions t))


(provide 'prog-training)
;;; prog-training.el ends here.
