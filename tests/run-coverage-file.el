;;; run-coverage-file.el --- Undercover setup for per-file coverage runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded via `-l tests/run-coverage-file.el' by the Makefile's coverage
;; target before each test file runs.  Ensures undercover is active and
;; configured to merge into the shared LCOV output so coverage data
;; accumulates across all test-file invocations.
;;
;; Per-file isolation matches the project's `make test-unit' pattern:
;; each test file runs in its own Emacs process, so tests that work
;; under `make test' will also work under `make coverage'.  See
;; docs/design/coverage.org for the rationale.

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(unless (require 'undercover nil t)
  (message "")
  (message "ERROR: undercover not installed.")
  (message "Start Emacs interactively to install it via use-package,")
  (message "or run: emacs --batch --eval \"(progn (package-refresh-contents) (package-install 'undercover))\"")
  (message "")
  (kill-emacs 1))

;; Force coverage collection even when not in CI.  Must happen AFTER
;; `(require 'undercover)' because undercover.el's top-level
;; `(setq undercover-force-coverage (getenv "UNDERCOVER_FORCE"))'
;; would otherwise overwrite our value.
(setq undercover-force-coverage t)

(undercover "modules/*.el"
			(:report-format 'simplecov)
			(:report-file ".coverage/simplecov.json")
			(:merge-report t)
			(:send-report nil))

;;; run-coverage-file.el ends here
