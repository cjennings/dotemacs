;;; test-dirvish-config--side-key.el --- dirvish-side lives on F9 -*- lexical-binding: t; -*-

;;; Commentary:
;; The sidebar toggle moved from F11 to F9 on 2026-09-22 so F11 could go back
;; to `toggle-frame-fullscreen', the stock binding every other application
;; shares and which the :bind had been shadowing.  F9 was the free bare
;; F-key: ai-term left it for M-SPC in June and the recording chord came off
;; it the same day this moved.  F2 is the preview key and F5 is reserved for
;; the debug backend, so neither was a candidate.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)
(require 'dirvish-config)

;;; Normal

(ert-deftest test-dirvish-config-side-on-f9 ()
  "Normal: F9 toggles the dirvish sidebar."
  (should (eq (keymap-lookup global-map "<f9>") #'dirvish-side)))

;;; Boundary

(ert-deftest test-dirvish-config-side-not-on-f11 ()
  "Boundary/regression: F11 no longer reaches dirvish-side, so the stock
`toggle-frame-fullscreen' binding is no longer shadowed."
  (should-not (eq (keymap-lookup global-map "<f11>") #'dirvish-side))
  (should (eq (keymap-lookup global-map "<f11>") #'toggle-frame-fullscreen)))

;;; Error

(ert-deftest test-dirvish-config-side-target-is-a-command ()
  "Error (positive control): the binding points at a real interactive
command, so a keypress does not fail with a `commandp' error at press time."
  (should (commandp (keymap-lookup global-map "<f9>"))))

(provide 'test-dirvish-config--side-key)
;;; test-dirvish-config--side-key.el ends here
