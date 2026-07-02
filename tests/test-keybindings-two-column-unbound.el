;;; test-keybindings-two-column-unbound.el --- 2C-command is unbound -*- lexical-binding: t; -*-

;;; Commentary:
;; Two-column mode's global F2 / C-x 6 prefix replaces a buffer's
;; mode-line-format with its own layout when triggered by a stray
;; keypress (the 2026-07-01 archsetup agent-buffer incident).
;; keybindings.el retires both bindings; these tests pin that.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'keybindings)

(ert-deftest test-keybindings-two-column-f2-unbound ()
  "Normal: F2 no longer reaches the 2C-command prefix."
  (should-not (keymap-lookup global-map "<f2>")))

(ert-deftest test-keybindings-two-column-c-x-6-unbound ()
  "Normal: C-x 6 no longer reaches the 2C-command prefix."
  (should-not (keymap-lookup global-map "C-x 6")))

(provide 'test-keybindings-two-column-unbound)
;;; test-keybindings-two-column-unbound.el ends here
