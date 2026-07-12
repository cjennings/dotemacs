;;; test-org-reveal-config-keymap.el --- Tests for org-reveal-config prefix keymap -*- lexical-binding: t; -*-

;;; Commentary:
;; The presentation commands are reached through `cj/reveal-map', a prefix
;; keymap registered under "C-; p" via `cj/register-prefix-map'.  These
;; tests pin that structure so the module can't silently regress to raw
;; `global-set-key' calls (which carry a hidden load-order dependency on
;; keybindings.el establishing "C-;" as a prefix first).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)
(require 'org-reveal-config)

(ert-deftest test-org-reveal-config-reveal-map-is-a-keymap ()
  "Normal: `cj/reveal-map' is a keymap."
  (should (keymapp cj/reveal-map)))

(ert-deftest test-org-reveal-config-reveal-map-bindings ()
  "Normal: each presentation command is reachable under `cj/reveal-map'."
  (dolist (pair '(("SPC" . cj/reveal-present)
                  ("e"   . cj/reveal-export)
                  ("p"   . cj/reveal-preview-start)
                  ("s"   . cj/reveal-preview-stop)
                  ("h"   . cj/reveal-insert-header)
                  ("H"   . cj/reveal-remove-headers)
                  ("n"   . cj/reveal-new)))
    (should (eq (keymap-lookup cj/reveal-map (car pair)) (cdr pair)))))

(ert-deftest test-org-reveal-config-reveal-map-registered-under-p ()
  "Normal: `cj/reveal-map' is registered under \"p\" in `cj/custom-keymap'."
  (should (eq (keymap-lookup cj/custom-keymap "p") cj/reveal-map)))

(provide 'test-org-reveal-config-keymap)
;;; test-org-reveal-config-keymap.el ends here
