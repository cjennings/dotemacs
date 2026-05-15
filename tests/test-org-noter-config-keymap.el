;;; test-org-noter-config-keymap.el --- Lock the cj/org-noter-map shape -*- lexical-binding: t; -*-

;;; Commentary:
;; Pins the bindings under `C-; n' so a casual edit doesn't drift the
;; layout without intent.  Mnemonic: the most-used action (insert
;; note) sits on the doubled-prefix letter `n', sibling-stepping goes
;; on the angle-bracket pair, and `.' stays the "this entry" key.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-noter-config)

(ert-deftest test-org-noter-keymap-insert-note-on-doubled-n ()
  "Normal: `C-; n n' invokes the insert-note command (the most-used
action, on the doubled-prefix letter)."
  (should (eq (keymap-lookup cj/org-noter-map "n")
              #'cj/org-noter-insert-note-dwim)))

(ert-deftest test-org-noter-keymap-sync-siblings-on-angle-brackets ()
  "Normal: sibling-stepping lives on `>' (next) and `<' (prev).
The angle brackets read naturally as direction; this freed up `n'
and `p' for the insert and other future actions."
  (should (eq (keymap-lookup cj/org-noter-map ">")
              #'org-noter-sync-next-note))
  (should (eq (keymap-lookup cj/org-noter-map "<")
              #'org-noter-sync-prev-note)))

(ert-deftest test-org-noter-keymap-sync-current-stays-on-dot ()
  "Normal: `.' still invokes `org-noter-sync-current-note'."
  (should (eq (keymap-lookup cj/org-noter-map ".")
              #'org-noter-sync-current-note)))

(ert-deftest test-org-noter-keymap-no-stale-i-binding ()
  "Regression: the old `i' binding for insert-note is gone (insert
moved to `n').  Leaving `i' around would create two ways to invoke
the same command and a stale which-key hint."
  (should-not (keymap-lookup cj/org-noter-map "i")))

(provide 'test-org-noter-config-keymap)
;;; test-org-noter-config-keymap.el ends here
