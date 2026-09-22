;;; test-keybindings--fullscreen.el --- F11 toggles frame fullscreen -*- lexical-binding: t; -*-

;;; Commentary:
;; Stock Emacs binds F11 to `toggle-frame-fullscreen', the same key every
;; other application on the desktop uses for it.  This config never rebound
;; it; dirvish-config's :bind put dirvish-side on F11 and shadowed the stock
;; binding for as long as that entry existed.  It moved to F9 on 2026-09-22
;; (see test-dirvish-config--side-key.el, which pins the un-shadowing).
;;
;; These tests pin the foundation layer's side of that: keybindings.el must
;; leave the stock F11 binding in place.  Only keybindings.el is loaded here
;; on purpose.  A feature module that later claimed F11 would fail the
;; dirvish-side regression test, not this one.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)

;;; Normal

(ert-deftest test-keybindings-f11-toggles-fullscreen ()
  "Normal: F11 runs `toggle-frame-fullscreen' once the foundation layer has
loaded.  Stock Emacs provides the binding; this pins that keybindings.el does
not unset or rebind it."
  (should (eq (keymap-lookup global-map "<f11>") #'toggle-frame-fullscreen)))

;;; Boundary

(ert-deftest test-keybindings-f11-is-the-only-fullscreen-chord ()
  "Boundary: the plain key carries it; the shifted chord is left free so a
grazed Shift does not toggle the frame twice."
  (should-not (eq (keymap-lookup global-map "S-<f11>")
                  #'toggle-frame-fullscreen)))

;;; Error

(ert-deftest test-keybindings-f11-target-is-a-command ()
  "Error (positive control): the target is a real interactive command, so a
press does not fail with a `commandp' error at press time."
  (should (commandp (keymap-lookup global-map "<f11>"))))

(provide 'test-keybindings--fullscreen)
;;; test-keybindings--fullscreen.el ends here
