;;; test-dev-fkeys--f6-source-stem.el --- Tests for cj/--f6-source-stem -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the stem extractor. Given any source-or-test filename, returns
;; the source module name with directory, extension, and any test-pattern
;; prefix/suffix stripped. Used to map source files to their tests and
;; vice versa.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases — Elisp

(ert-deftest test-dev-fkeys-f6-source-stem-elisp-source ()
  "Normal: a regular elisp source file's stem is its basename."
  (should (string= (cj/--f6-source-stem "modules/foo.el") "foo")))

(ert-deftest test-dev-fkeys-f6-source-stem-elisp-test ()
  "Normal: an elisp test file's stem strips the `test-' prefix."
  (should (string= (cj/--f6-source-stem "tests/test-foo.el") "foo")))

(ert-deftest test-dev-fkeys-f6-source-stem-elisp-test-double-dash ()
  "Normal: an elisp per-helper test file `test-<module>--<helper>.el' yields
the module name (everything from `--' onward is dropped)."
  (should (string= (cj/--f6-source-stem "tests/test-dev-fkeys--detect-project-type.el")
                   "dev-fkeys")))

;;; Normal Cases — Python

(ert-deftest test-dev-fkeys-f6-source-stem-python-source ()
  "Normal: a regular Python source file's stem is its basename."
  (should (string= (cj/--f6-source-stem "pkg/foo.py") "foo")))

(ert-deftest test-dev-fkeys-f6-source-stem-python-test-prefix ()
  "Normal: a `test_<name>.py' file's stem strips the `test_' prefix."
  (should (string= (cj/--f6-source-stem "tests/test_foo.py") "foo")))

(ert-deftest test-dev-fkeys-f6-source-stem-python-test-suffix ()
  "Normal: a `<name>_test.py' file's stem strips the `_test' suffix."
  (should (string= (cj/--f6-source-stem "pkg/foo_test.py") "foo")))

;;; Normal Cases — Go

(ert-deftest test-dev-fkeys-f6-source-stem-go-source ()
  "Normal: a regular Go source file's stem is its basename."
  (should (string= (cj/--f6-source-stem "pkg/foo.go") "foo")))

(ert-deftest test-dev-fkeys-f6-source-stem-go-test ()
  "Normal: a `<name>_test.go' file's stem strips the `_test' suffix."
  (should (string= (cj/--f6-source-stem "pkg/foo_test.go") "foo")))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f6-source-stem-elisp-multi-segment-name ()
  "Boundary: hyphenated module names round-trip cleanly."
  (should (string= (cj/--f6-source-stem "modules/calendar-sync.el") "calendar-sync"))
  (should (string= (cj/--f6-source-stem "tests/test-calendar-sync.el") "calendar-sync")))

(ert-deftest test-dev-fkeys-f6-source-stem-elisp-test-with-dashes-after-double-dash ()
  "Boundary: a per-helper test file with hyphens in both module and helper:
`test-foo-bar--baz-qux.el' → `foo-bar' (stops at first `--')."
  (should (string= (cj/--f6-source-stem "tests/test-foo-bar--baz-qux.el")
                   "foo-bar")))

(ert-deftest test-dev-fkeys-f6-source-stem-unknown-language-falls-back-to-basename ()
  "Boundary: an unsupported extension just returns the basename without extension."
  (should (string= (cj/--f6-source-stem "foo.rs") "foo"))
  (should (string= (cj/--f6-source-stem "foo.txt") "foo")))

;;; Error Cases

(ert-deftest test-dev-fkeys-f6-source-stem-nil-returns-nil ()
  "Error: nil filename returns nil without erroring."
  (should (null (cj/--f6-source-stem nil))))

(ert-deftest test-dev-fkeys-f6-source-stem-empty-returns-nil ()
  "Error: empty string returns nil."
  (should (null (cj/--f6-source-stem ""))))

(provide 'test-dev-fkeys--f6-source-stem)
;;; test-dev-fkeys--f6-source-stem.el ends here
