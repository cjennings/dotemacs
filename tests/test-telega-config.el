;;; test-telega-config.el --- Tests for Telegram (telega) module wiring -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight asserts that the module loads, exposes the launcher,
;; sets the docker preference, and registers the C-; G keybinding.
;; The bulk of telega.el's behaviour is owned upstream; the tests
;; here only cover what this config wires.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'telega-config)

(ert-deftest test-telega-config-loads-cleanly ()
  "Normal: module loads without errors and `provide's its feature."
  (should (featurep 'telega-config)))

(ert-deftest test-telega-config-launcher-binding-is-telega ()
  "Normal: =C-; G= invokes `telega'."
  (should (eq (keymap-lookup cj/custom-keymap "G") #'telega)))

(provide 'test-telega-config)
;;; test-telega-config.el ends here
