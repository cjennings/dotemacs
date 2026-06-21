;;; test-init-defer-games.el --- games-config Phase 4 deferral -*- lexical-binding: t; -*-

;;; Commentary:
;; games-config is deferred (load-graph Phase 4): malyon and 2048-game autoload
;; their own commands via package.el, and init.el loads games-config (which only
;; supplies malyon's config) via `with-eval-after-load 'malyon'.  These tests
;; guard the command availability and exercise the real autoload-invocation path
;; that M-x uses, which is where an earlier cut regressed ("Autoloading
;; games-config.el failed to define function malyon").

;;; Code:

(require 'ert)
(require 'package)

(ert-deftest test-init-defer-games-commands-autoload-without-module ()
  "Normal: the game commands resolve with games-config unloaded.
Dropping the eager require keeps malyon and 2048-game reachable only because the
packages autoload their own commands, so assert that holds."
  (package-initialize)
  (should-not (featurep 'games-config))
  (should (commandp 'malyon))
  (should (commandp '2048-game)))

(ert-deftest test-init-defer-games-malyon-loads-and-configures ()
  "Normal: resolving malyon's autoload yields a real command and applies config.
Reproduces the M-x malyon path via `autoload-do-load': malyon autoloads from its
own package, init.el's `with-eval-after-load 'malyon' loads games-config, and
games-config sets the stories directory.  This is the regression guard for the
earlier cut that autoloaded malyon to games-config, where Emacs errored that the
load failed to define malyon."
  (package-initialize)
  (add-to-list 'load-path (expand-file-name "modules" default-directory))
  (require 'user-constants)
  (unless (and (fboundp 'malyon) (autoloadp (symbol-function 'malyon)))
    (ert-skip "malyon package not available as an autoload"))
  (let ((org-dir "/tmp/games-defer-test/"))
    (with-eval-after-load 'malyon (require 'games-config))   ; the init.el wiring
    (should-not (featurep 'games-config))
    (should (functionp (autoload-do-load (symbol-function 'malyon) 'malyon)))
    (should (commandp 'malyon))
    (should (featurep 'games-config))
    (should (equal malyon-stories-directory "/tmp/games-defer-test/text.games/"))))

(provide 'test-init-defer-games)
;;; test-init-defer-games.el ends here
