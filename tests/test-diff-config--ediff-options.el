;;; test-diff-config--ediff-options.el --- Tests for ediff diff options -*- lexical-binding: t -*-

;;; Commentary:
;; Pins the removal of the global "-w" default for `ediff-diff-options'.
;; With "-w", every ediff session ignores ALL whitespace, so
;; indentation-only changes (significant in Python, Makefiles, YAML)
;; compare as identical.  Whitespace-ignoring is a per-session toggle
;; (ediff's `##'), not a global default.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'diff-config)

;;; Normal Cases

(ert-deftest test-diff-config-ediff-options-no-global-whitespace-ignore ()
  "Normal: after ediff loads, no global -w sits in ediff-diff-options.
The use-package :custom values apply when the deferred package loads,
so the assertion must run with ediff actually loaded."
  (require 'ediff)
  (should (not (string-match-p "-w" (or ediff-diff-options "")))))

(provide 'test-diff-config--ediff-options)
;;; test-diff-config--ediff-options.el ends here
