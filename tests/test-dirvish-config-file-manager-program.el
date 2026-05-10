;;; test-dirvish-config-file-manager-program.el --- Tests for the file-manager dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--file-manager-program-for' is the pure dispatch behind
;; `cj/dirvish-open-file-manager-here'.  Given whether xdg-open is
;; present and the running `system-type', it returns the program name
;; the wrapper should call -- or nil to signal the wrapper should fall
;; back to a shell-command.  Keeping `executable-find' and `system-type'
;; outside lets the helper be tested without faking the live machine.

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

(ert-deftest test-cj--file-manager-program-for-xdg-open-on-linux ()
  "Normal: xdg-open present on Linux returns xdg-open."
  (should (equal (cj/--file-manager-program-for t 'gnu/linux)
                 "xdg-open")))

(ert-deftest test-cj--file-manager-program-for-xdg-open-wins-on-macos ()
  "Boundary: xdg-open present even on macOS returns xdg-open (Linux-isms ported)."
  (should (equal (cj/--file-manager-program-for t 'darwin)
                 "xdg-open")))

(ert-deftest test-cj--file-manager-program-for-darwin-no-xdg ()
  "Normal: macOS without xdg-open returns open."
  (should (equal (cj/--file-manager-program-for nil 'darwin)
                 "open")))

(ert-deftest test-cj--file-manager-program-for-windows-no-xdg ()
  "Normal: Windows without xdg-open returns explorer."
  (should (equal (cj/--file-manager-program-for nil 'windows-nt)
                 "explorer")))

(ert-deftest test-cj--file-manager-program-for-linux-without-xdg-falls-back ()
  "Boundary: Linux without xdg-open returns nil so the wrapper shells out."
  (should-not (cj/--file-manager-program-for nil 'gnu/linux)))

(ert-deftest test-cj--file-manager-program-for-unknown-system-falls-back ()
  "Boundary: an unknown `system-type' with no xdg-open returns nil."
  (should-not (cj/--file-manager-program-for nil 'haiku)))

(provide 'test-dirvish-config-file-manager-program)
;;; test-dirvish-config-file-manager-program.el ends here
