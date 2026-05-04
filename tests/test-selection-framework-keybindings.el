;;; test-selection-framework-keybindings.el --- Tests for selection framework keys -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke coverage for the final search binding owned by selection-framework.el.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defmacro use-package (&rest _args)
  "Ignore package configuration while loading selection-framework in tests."
  nil)

(defun consult-line (&rest _args)
  "Test stub for `consult-line'.")

(defun vertico-repeat (&rest _args)
  "Test stub for `vertico-repeat'.")

(require 'selection-framework)

(ert-deftest test-selection-framework-c-s-runs-consult-line-or-repeat ()
  "C-s should resolve to the final consult-line-or-repeat command."
  (should (eq (key-binding (kbd "C-s"))
              #'cj/consult-line-or-repeat)))

(provide 'test-selection-framework-keybindings)
;;; test-selection-framework-keybindings.el ends here
