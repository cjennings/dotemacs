;;; test-term-config--f8-in-term.el --- F8 reaches Emacs from inside a ghostel buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; <f8> is a global binding (`cj/main-agenda-display', set in org-agenda-config).
;; ghostel's semi-char mode forwards every key NOT in `ghostel-keymap-exceptions'
;; to the terminal program, so a plain <f8> typed while point is in a ghostel
;; buffer would be sent to the program instead of opening the agenda.  Unlike the
;; F9 family, F8 is NOT re-bound in `ghostel-mode-map' -- it simply falls through
;; to the global map once the semi-char map stops forwarding it, so the only
;; wiring term-config.el adds is the keymap-exceptions entry plus the rebuild.
;; These tests require ghostel (so term-config's `with-eval-after-load' fires)
;; BEFORE term-config, then confirm the exception landed and the rebuilt
;; semi-char map no longer forwards <f8>.  `(require 'ghostel)' does not load the
;; native module, so this stays light.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ghostel)
(require 'term-config)

(ert-deftest test-term-config-f8-in-keymap-exceptions ()
  "Regression: <f8> is in `ghostel-keymap-exceptions' so semi-char mode lets it
reach Emacs instead of forwarding it to the terminal program.  This is what lets
the global agenda binding work from inside a ghostel buffer."
  (should (member "<f8>" ghostel-keymap-exceptions)))

(ert-deftest test-term-config-f8-not-forwarded-by-semi-char-map ()
  "Regression: the rebuilt semi-char map must no longer forward <f8> to the pty.
`add-to-list' updates the exceptions list but not the already-built map -- only
`ghostel--rebuild-semi-char-keymap' (run in term-config's :init) drops the
forwarding binding so <f8> falls through to the global agenda command."
  (should-not (eq (keymap-lookup ghostel-semi-char-mode-map "<f8>")
                  'ghostel--send-event)))

(provide 'test-term-config--f8-in-term)
;;; test-term-config--f8-in-term.el ends here
