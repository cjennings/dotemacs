;;; test-keybindings-tty-mirror.el --- TTY mirror prefix for the C-; family -*- lexical-binding: t; -*-

;;; Commentary:
;; The personal prefix C-; is GUI-only — terminals can't encode Control-semicolon,
;; so the whole custom command family is unreachable in a TTY frame (emacs -nw,
;; emacsclient -nw, Emacs inside vterm/tmux).  keybindings.el binds the single
;; `cj/custom-keymap' under a TTY-safe mirror prefix C-c ; alongside C-;, so the
;; same leaf keys reach the identical map in both GUI and terminal.  These tests
;; pin that load-time global binding.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)

(ert-deftest test-keybindings-tty-mirror-gui-prefix-resolves ()
  "Normal: the GUI prefix C-; resolves to cj/custom-keymap globally."
  (should (eq (keymap-lookup (current-global-map) "C-;") cj/custom-keymap)))

(ert-deftest test-keybindings-tty-mirror-tty-prefix-resolves ()
  "Normal: the TTY mirror C-c ; resolves to the same cj/custom-keymap."
  (should (eq (keymap-lookup (current-global-map) "C-c ;") cj/custom-keymap)))

(ert-deftest test-keybindings-tty-mirror-both-prefixes-share-one-map ()
  "Boundary: both prefixes point at the identical keymap object, so a leaf
key registered once is reachable under either prefix."
  (should (eq (keymap-lookup (current-global-map) "C-;")
              (keymap-lookup (current-global-map) "C-c ;"))))

(provide 'test-keybindings-tty-mirror)
;;; test-keybindings-tty-mirror.el ends here
