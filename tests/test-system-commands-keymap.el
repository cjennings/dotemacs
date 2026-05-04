;;; test-system-commands-keymap.el --- Tests for system command keymap -*- lexical-binding: t; -*-

;;; Commentary:

;; The system command keymap should remain mounted as a prefix under C-; ! so
;; which-key can show the documented subcommands.

;;; Code:

(require 'ert)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub custom keymap for system-commands tests.")

(require 'system-commands)

(ert-deftest test-system-commands-keymap-normal-prefix-mounted ()
  "Normal: C-; ! remains a prefix keymap, not a direct command."
  (should (eq (keymap-lookup cj/custom-keymap "!")
              cj/system-command-map)))

(ert-deftest test-system-commands-keymap-normal-documented-subkeys ()
  "Normal: documented system command subkeys resolve under the prefix."
  (dolist (binding '(("!" . cj/system-command-menu)
                     ("L" . cj/system-cmd-logout)
                     ("r" . cj/system-cmd-reboot)
                     ("s" . cj/system-cmd-shutdown)
                     ("S" . cj/system-cmd-suspend)
                     ("l" . cj/system-cmd-lock)
                     ("E" . cj/system-cmd-exit-emacs)
                     ("e" . cj/system-cmd-restart-emacs)))
    (should (eq (keymap-lookup cj/system-command-map (car binding))
                (cdr binding)))))

(provide 'test-system-commands-keymap)
;;; test-system-commands-keymap.el ends here
