;;; test-prog-c-mode-keybindings.el --- Tests for cj/c-mode-keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies the C mode hook installs the S-modifier overrides on the
;; current buffer's local keymap.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-c)

(ert-deftest test-prog-c-mode-keybindings-normal-installs-fkey-overrides ()
  "Normal: cj/c-mode-keybindings binds S-F5 to cj/disabled and S-F6 to gdb."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (cj/c-mode-keybindings)
    (should (eq (lookup-key (current-local-map) (kbd "S-<f5>"))
                'cj/disabled))
    (should (eq (lookup-key (current-local-map) (kbd "S-<f6>"))
                'gdb))))

(provide 'test-prog-c-mode-keybindings)
;;; test-prog-c-mode-keybindings.el ends here
