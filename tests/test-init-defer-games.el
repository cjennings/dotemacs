;;; test-init-defer-games.el --- games-config Phase 4 deferral -*- lexical-binding: t; -*-

;;; Commentary:
;; games-config is deferred (load-graph Phase 4): init.el autoloads `malyon'
;; and `2048-game' instead of requiring the module eagerly.  These tests guard
;; that the game commands stay reachable with the module unloaded, and that
;; loading the module still applies the one setting it owns.

;;; Code:

(require 'ert)
(require 'package)

(ert-deftest test-init-defer-games-commands-autoload-without-module ()
  "Normal: the game commands resolve with games-config unloaded.
This is the safety net for the deferral -- dropping the eager require keeps
malyon and 2048-game reachable only because the packages autoload their own
commands, so assert that holds."
  (package-initialize)
  (should-not (featurep 'games-config))
  (should (commandp 'malyon))
  (should (commandp '2048-game)))

(ert-deftest test-init-defer-games-config-applies-malyon-stories-dir ()
  "Normal: loading games-config still applies malyon's stories directory.
The module is the config owner; deferring it must not drop the one setting it
adds (`malyon-stories-directory'), which use-package applies when malyon loads."
  (package-initialize)
  (add-to-list 'load-path (expand-file-name "modules" default-directory))
  (require 'user-constants)
  (let ((org-dir "/tmp/games-defer-test/"))
    (load "games-config" nil t)
    (unless (require 'malyon nil t)
      (ert-skip "malyon package not available"))
    (should (equal malyon-stories-directory "/tmp/games-defer-test/text.games/"))))

(provide 'test-init-defer-games)
;;; test-init-defer-games.el ends here
