;;; test-system-lib-executable-exists-p.el --- Tests for cj/executable-exists-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/executable-exists-p function from system-lib.el.
;; Tests whether external programs are correctly detected in PATH.

;;; Code:

(require 'ert)
(require 'system-lib)

;;; Normal Cases

(ert-deftest test-system-lib-executable-exists-p-normal-existing-program-returns-path ()
  "Test that existing program in PATH returns non-nil.

Standard case: checking for a program that definitely exists on all systems."
  (should (cj/executable-exists-p "ls")))

(ert-deftest test-system-lib-executable-exists-p-normal-diff-exists-returns-path ()
  "Test that diff program exists and is detected.

Tests specifically for diff which we use in our diff functionality."
  (should (cj/executable-exists-p "diff")))

;;; Boundary Cases

(ert-deftest test-system-lib-executable-exists-p-boundary-empty-string-returns-nil ()
  "Test that empty string returns nil.

Boundary case: empty string is not a valid program name."
  (should-not (cj/executable-exists-p "")))

(ert-deftest test-system-lib-executable-exists-p-boundary-whitespace-only-returns-nil ()
  "Test that whitespace-only string returns nil.

Boundary case: strings containing only whitespace are not valid programs."
  (should-not (cj/executable-exists-p "   ")))

(ert-deftest test-system-lib-executable-exists-p-boundary-absolute-path-returns-path ()
  "Test that absolute path to executable returns the path.

Boundary case: executable-find accepts both program names and full paths."
  (should (cj/executable-exists-p "/usr/bin/ls")))

;;; Error Cases

(ert-deftest test-system-lib-executable-exists-p-error-nil-input-returns-nil ()
  "Test that nil input returns nil gracefully.

Error case: nil is not a valid program name."
  (should-not (cj/executable-exists-p nil)))

(ert-deftest test-system-lib-executable-exists-p-error-number-input-returns-nil ()
  "Test that numeric input returns nil gracefully.

Error case: number is not a valid program name."
  (should-not (cj/executable-exists-p 42)))

(ert-deftest test-system-lib-executable-exists-p-error-nonexistent-program-returns-nil ()
  "Test that nonexistent program returns nil.

Error case: program that definitely doesn't exist in PATH."
  (should-not (cj/executable-exists-p "this-program-definitely-does-not-exist-xyz123")))

(ert-deftest test-system-lib-executable-exists-p-error-special-characters-returns-nil ()
  "Test that program name with special characters returns nil.

Error case: invalid characters in program name."
  (should-not (cj/executable-exists-p "program-with-$pecial-ch@rs")))

(provide 'test-system-lib-executable-exists-p)
;;; test-system-lib-executable-exists-p.el ends here
