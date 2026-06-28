;;; test-system-commands-keymap.el --- Tests for system command keymap -*- lexical-binding: t; -*-

;;; Commentary:

;; C-; ! is bound directly to the completing-read menu.  The per-command leaf
;; keys (s/r/e/l/L/E/S) were removed to reclaim the key real-estate; every
;; command stays reachable through the menu (see the menu-dispatch test).

;;; Code:

(require 'ert)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub custom keymap for system-commands tests.")

(require 'system-commands)

(ert-deftest test-system-commands-keymap-normal-menu-bound-directly ()
  "Normal: C-; ! is the completing-read menu command, not a prefix keymap."
  (let ((binding (keymap-lookup cj/custom-keymap "!")))
    (should (eq binding 'cj/system-command-menu))
    (should (commandp binding))))

(ert-deftest test-system-commands-keymap-normal-leaf-subkeys-removed ()
  "Normal: no subkeys hang off C-; !, and the commands remain defined."
  ;; "!" is now a command, not a prefix, so there is no submap to walk into.
  (should-not (keymapp (keymap-lookup cj/custom-keymap "!")))
  ;; The commands themselves stay defined and reachable via the menu.
  (dolist (cmd '(cj/system-cmd-logout cj/system-cmd-reboot cj/system-cmd-shutdown
                 cj/system-cmd-suspend cj/system-cmd-lock cj/system-cmd-exit-emacs
                 cj/system-cmd-restart-emacs))
    (should (fboundp cmd))))

(provide 'test-system-commands-keymap)
;;; test-system-commands-keymap.el ends here
