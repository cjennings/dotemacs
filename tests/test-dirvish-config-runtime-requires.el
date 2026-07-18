;;; test-dirvish-config-runtime-requires.el --- dirvish-config declares its deps -*- lexical-binding: t; -*-

;;; Commentary:
;; dirvish-config.el builds `dirvish-quick-access-entries' from `code-dir',
;; `music-dir', `pix-dir' (and friends) at load time and binds keys to
;; `cj/xdg-open' (external-open) and `cj/open-file-with-command'
;; (system-utils), so it depends on user-constants, system-utils, and
;; external-open at runtime.  This is a dependency-contract smoke test:
;; requiring dirvish-config in isolation must pull those features in, so it
;; fails if the requires are dropped entirely.  Run it with `make test-file'
;; for a clean signal: in the full suite another file may already have loaded
;; external-open, masking a regression here.

;;; Code:

(require 'ert)
(require 'dirvish-config)

(ert-deftest test-dirvish-config-loads-user-constants ()
  "Normal: requiring dirvish-config pulls in user-constants at runtime."
  (should (featurep 'user-constants)))

(ert-deftest test-dirvish-config-loads-system-utils ()
  "Normal: requiring dirvish-config pulls in system-utils at runtime."
  (should (featurep 'system-utils)))

(ert-deftest test-dirvish-config-loads-external-open ()
  "Normal: requiring dirvish-config pulls in external-open at runtime.
The keys `o' and the OS-handler fallback call `cj/xdg-open', which lives
in external-open; without the require the binding works only when init
order happens to load external-open first."
  (should (featurep 'external-open))
  (should (fboundp 'cj/xdg-open)))

(provide 'test-dirvish-config-runtime-requires)
;;; test-dirvish-config-runtime-requires.el ends here
