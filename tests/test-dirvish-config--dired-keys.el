;;; test-dirvish-config--dired-keys.el --- dired d=diff / D=delete bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression: d and D in dired (and dirvish, which uses dired-mode-map) are the
;; diff and delete pair, matching the convention under C-; b and in ibuffer.  A
;; mismatch -- or a swapped which-key label -- once led to deleting a file while
;; trying to diff it.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dired)
(require 'dirvish-config)

(ert-deftest test-dirvish-dired-d-diffs-D-deletes ()
  "Normal: dired d runs the ediff diff and D deletes, matching the d=diff /
D=delete convention used under C-; b and in ibuffer."
  (should (eq (keymap-lookup dired-mode-map "d") #'cj/dired-ediff-files))
  (should (eq (keymap-lookup dired-mode-map "D") #'dired-do-delete)))

(provide 'test-dirvish-config--dired-keys)
;;; test-dirvish-config--dired-keys.el ends here
