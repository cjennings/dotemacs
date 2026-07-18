;;; test-dwim-shell-config-runtime-requires.el --- dwim-shell-config declares its deps -*- lexical-binding: t; -*-

;;; Commentary:
;; dwim-shell-config.el calls `cj/xdg-open' (external-open) to open a
;; conversion's output file, but declared it only with `declare-function'
;; and never required external-open.  The binding works at runtime only
;; because init.el happens to load external-open first — fragile under init
;; order, and the "Direct test load: yes" header claims otherwise.  This is
;; a dependency-contract smoke test: requiring dwim-shell-config in isolation
;; must pull external-open in.  Run with `make test-file' for a clean signal;
;; in the full suite another file may already have loaded external-open.

;;; Code:

(require 'ert)
(require 'dwim-shell-config)

(ert-deftest test-dwim-shell-config-loads-external-open ()
  "Normal: requiring dwim-shell-config pulls in external-open at runtime."
  (should (featurep 'external-open))
  (should (fboundp 'cj/xdg-open)))

(provide 'test-dwim-shell-config-runtime-requires)
;;; test-dwim-shell-config-runtime-requires.el ends here
