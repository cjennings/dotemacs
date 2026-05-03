;;; test-dev-fkeys--f6-buffer-is-test-file-p.el --- Tests for cj/--f6-buffer-is-test-file-p -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the test-file detector. Naming heuristics per language:
;;
;;   elisp:  basename starts with "test-"
;;   python: basename starts with "test_" OR ends with "_test.py"
;;   go:     basename ends with "_test.go"
;;   ts/js:  basename contains ".test." or ".spec."
;;
;; Anything else (including unsupported languages) returns nil.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases — Elisp

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-elisp-test ()
  "Normal: an elisp file with `test-` prefix is a test file."
  (should (cj/--f6-buffer-is-test-file-p "tests/test-foo.el")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-elisp-source ()
  "Normal: an elisp file without `test-` prefix is not a test file."
  (should-not (cj/--f6-buffer-is-test-file-p "modules/foo.el")))

;;; Normal Cases — Python

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-python-prefix ()
  "Normal: a Python file with `test_` prefix is a test file."
  (should (cj/--f6-buffer-is-test-file-p "tests/test_foo.py")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-python-suffix ()
  "Normal: a Python file with `_test.py` suffix is a test file."
  (should (cj/--f6-buffer-is-test-file-p "pkg/foo_test.py")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-python-source ()
  "Normal: a Python file with neither prefix nor suffix is not a test file."
  (should-not (cj/--f6-buffer-is-test-file-p "pkg/foo.py")))

;;; Normal Cases — Go

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-go-test ()
  "Normal: a Go file with `_test.go` suffix is a test file."
  (should (cj/--f6-buffer-is-test-file-p "pkg/foo_test.go")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-go-source ()
  "Normal: a Go file without `_test` suffix is not a test file."
  (should-not (cj/--f6-buffer-is-test-file-p "pkg/foo.go")))

;;; Normal Cases — TS / JS

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-typescript-test ()
  "Normal: a .ts file with `.test.` infix is a test file."
  (should (cj/--f6-buffer-is-test-file-p "src/foo.test.ts")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-typescript-spec ()
  "Normal: a .ts file with `.spec.` infix is a test file."
  (should (cj/--f6-buffer-is-test-file-p "src/foo.spec.ts")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-javascript-test ()
  "Normal: a .js file with `.test.` infix is a test file."
  (should (cj/--f6-buffer-is-test-file-p "src/foo.test.js")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-typescript-source ()
  "Normal: a .ts file with no test/spec marker is not a test file."
  (should-not (cj/--f6-buffer-is-test-file-p "src/foo.ts")))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-double-dash-elisp ()
  "Boundary: `test-foo--bar.el' (project's per-helper convention) is a test file."
  (should (cj/--f6-buffer-is-test-file-p "tests/test-foo--bar.el")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-test-in-name-elisp ()
  "Boundary: a non-test elisp file that just happens to contain `test` somewhere
in the basename is not a test file (must start with `test-`)."
  (should-not (cj/--f6-buffer-is-test-file-p "modules/contest.el")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-just-test-py ()
  "Boundary: `test.py' alone is not a test file (no underscore separator)."
  (should-not (cj/--f6-buffer-is-test-file-p "test.py")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-go-suffix-only ()
  "Boundary: only `_test.go' suffix counts; `test_foo.go' does not (Go
convention is suffix, not prefix)."
  (should-not (cj/--f6-buffer-is-test-file-p "pkg/test_foo.go")))

;;; Error Cases

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-nil ()
  "Error: nil filename returns nil without erroring."
  (should-not (cj/--f6-buffer-is-test-file-p nil)))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-empty ()
  "Error: empty string returns nil."
  (should-not (cj/--f6-buffer-is-test-file-p "")))

(ert-deftest test-dev-fkeys-f6-buffer-is-test-file-p-unsupported-language ()
  "Error: a file in an unsupported language returns nil even if the basename
matches a generic pattern."
  (should-not (cj/--f6-buffer-is-test-file-p "test-foo.rs"))
  (should-not (cj/--f6-buffer-is-test-file-p "foo_test.rb")))

(provide 'test-dev-fkeys--f6-buffer-is-test-file-p)
;;; test-dev-fkeys--f6-buffer-is-test-file-p.el ends here
