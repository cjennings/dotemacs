;;; test-hugo-config--keymap.el --- Tests for the Hugo prefix keymap -*- lexical-binding: t; -*-

;;; Commentary:
;; Pins the eight Hugo commands reachable under the "C-; h" prefix.
;;
;; The module used to install these with eight raw `global-set-key' calls plus
;; a hand-written which-key block, writing into the global map directly instead
;; of going through `cj/register-prefix-map' the way its siblings
;; (erc-config, custom-ordering, org-reveal-config) do.  These tests were added
;; alongside that conversion so the refactor is checkable: every key must still
;; reach the same command afterward.
;;
;; Test organization:
;; - Normal Cases: each of the eight keys resolves to its command
;; - Boundary Cases: case-distinct pairs stay distinct; the map is a prefix map
;; - Error Cases: an unbound key in the map resolves to nothing
;;
;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(provide 'ox-hugo)
(require 'hugo-config)
(require 'keybindings)

(defconst test-hugo--expected-bindings
  '(("n" . cj/hugo-new-post)
    ("e" . cj/hugo-export-post)
    ("o" . cj/hugo-open-blog-dir)
    ("O" . cj/hugo-open-blog-dir-external)
    ("d" . cj/hugo-open-draft)
    ("D" . cj/hugo-toggle-draft)
    ("p" . cj/hugo-preview)
    ("P" . cj/hugo-publish))
  "Every key the Hugo prefix map must carry, and the command it runs.")

;;; Normal Cases

(ert-deftest test-hugo-config-keymap-binds-every-command ()
  "Normal: each Hugo key resolves to its command inside the prefix map."
  (dolist (pair test-hugo--expected-bindings)
    (should (eq (cdr pair) (keymap-lookup cj/hugo-keymap (car pair))))))

(ert-deftest test-hugo-config-keymap-registered-under-custom-prefix ()
  "Normal: the map is reachable at \"h\" within `cj/custom-keymap'."
  (should (eq cj/hugo-keymap (keymap-lookup cj/custom-keymap "h"))))

;;; Boundary Cases

(ert-deftest test-hugo-config-keymap-case-pairs-stay-distinct ()
  "Boundary: the shifted variants run different commands than their lowercase
counterparts, which a case-folding binding would silently collapse."
  (should-not (eq (keymap-lookup cj/hugo-keymap "o")
                  (keymap-lookup cj/hugo-keymap "O")))
  (should-not (eq (keymap-lookup cj/hugo-keymap "d")
                  (keymap-lookup cj/hugo-keymap "D")))
  (should-not (eq (keymap-lookup cj/hugo-keymap "p")
                  (keymap-lookup cj/hugo-keymap "P"))))

(ert-deftest test-hugo-config-keymap-is-a-keymap ()
  "Boundary: the value registered as a prefix is an actual keymap."
  (should (keymapp cj/hugo-keymap)))

;;; Error Cases

(ert-deftest test-hugo-config-keymap-unbound-key-is-nil ()
  "Error: a key the map does not define resolves to nothing."
  (should-not (keymap-lookup cj/hugo-keymap "z")))

(provide 'test-hugo-config--keymap)
;;; test-hugo-config--keymap.el ends here
