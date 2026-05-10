;;; test-dirvish-config-wallpaper-program.el --- Tests for the wallpaper command resolver -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--wallpaper-program-for' is the pure dispatch behind
;; `cj/set-wallpaper': given a display-server symbol it returns the
;; (PROGRAM . PRE-FILE-ARGS) cons that the interactive wrapper passes
;; to `call-process' alongside the wallpaper file path.  The wrapper
;; handles environment detection (`env-x11-p' / `env-wayland-p'), the
;; `executable-find' check, and the user-visible message.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)

(ert-deftest test-cj--wallpaper-program-for-x11 ()
  "Normal: x11 dispatches to feh with --bg-fill."
  (should (equal (cj/--wallpaper-program-for 'x11)
                 '("feh" "--bg-fill"))))

(ert-deftest test-cj--wallpaper-program-for-wayland ()
  "Normal: wayland dispatches to swww with the img subcommand."
  (should (equal (cj/--wallpaper-program-for 'wayland)
                 '("swww" "img"))))

(ert-deftest test-cj--wallpaper-program-for-unknown-returns-nil ()
  "Boundary: an unknown environment returns nil so the wrapper can fall back."
  (should-not (cj/--wallpaper-program-for 'tty))
  (should-not (cj/--wallpaper-program-for nil))
  (should-not (cj/--wallpaper-program-for 'mac)))

(provide 'test-dirvish-config-wallpaper-program)
;;; test-dirvish-config-wallpaper-program.el ends here
