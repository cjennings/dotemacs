;;; test-dirvish-config-runtime-requires.el --- dirvish-config declares its deps -*- lexical-binding: t; -*-

;;; Commentary:
;; dirvish-config.el builds `dirvish-quick-access-entries' from `code-dir',
;; `music-dir', `pix-dir' (and friends) at load time and binds keys to
;; `cj/xdg-open' / `cj/open-file-with-command', so it depends on user-constants
;; and system-utils at runtime.  Those were declared with `eval-when-compile',
;; which leaves the compiled module without the requires at load — fragile
;; under init order.  This is a dependency-contract smoke test: requiring
;; dirvish-config in isolation must pull both features in, so it fails if the
;; requires are dropped entirely.  (It can't catch a downgrade back to
;; `eval-when-compile', since that form still runs when the file loads as
;; source, which the test harness does — that regression is guarded by keeping
;; the plain requires in review, not by this test.)

;;; Code:

(require 'ert)
(require 'dirvish-config)

(ert-deftest test-dirvish-config-loads-user-constants ()
  "Normal: requiring dirvish-config pulls in user-constants at runtime."
  (should (featurep 'user-constants)))

(ert-deftest test-dirvish-config-loads-system-utils ()
  "Normal: requiring dirvish-config pulls in system-utils at runtime."
  (should (featurep 'system-utils)))

(provide 'test-dirvish-config-runtime-requires)
;;; test-dirvish-config-runtime-requires.el ends here
