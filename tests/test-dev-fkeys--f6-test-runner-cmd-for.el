;;; test-dev-fkeys--f6-test-runner-cmd-for.el --- Tests for cj/--f6-test-runner-cmd-for -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the per-language test-runner-command builder.
;;
;; Inputs are five primitives the orchestrator pre-computes:
;;
;;   LANGUAGE        symbol from `cj/--f6-language-detect'
;;   IS-TEST-FILE    boolean from `cj/--f6-buffer-is-test-file-p'
;;   REL-PATH        file path relative to project root
;;   STEM            source module stem from `cj/--f6-source-stem'
;;   REL-DIR         file's directory relative to project root
;;
;; Returns a shell command string to pipe through `compile', or nil for
;; languages we don't have a runner for. The TS / JS handlers are punted
;; for v1 — they return nil.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases — Elisp

(ert-deftest test-dev-fkeys-f6-cmd-for-elisp-test-file ()
  "Normal: an elisp test file runs via `make test-file FILE=<basename>'.
The project Makefile prepends `tests/' to whatever FILE you pass, so the
runner command must use just the basename (not the rel-path) to avoid
`tests/tests/...' double-prefix."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'elisp t "tests/test-foo.el" "foo" "tests")
           "make test-file FILE=test-foo.el")))

(ert-deftest test-dev-fkeys-f6-cmd-for-elisp-source-file ()
  "Normal: an elisp source file runs via `make test-name TEST=^test-<stem>-'.
The trailing hyphen anchors the regex so test names from a different
module that happen to share a prefix don't get pulled in."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'elisp nil "modules/foo.el" "foo" "modules")
           "make test-name TEST=^test-foo-")))

;;; Normal Cases — Python

(ert-deftest test-dev-fkeys-f6-cmd-for-python-test-file ()
  "Normal: a Python test file runs via `pytest <rel-path>'."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'python t "tests/test_foo.py" "foo" "tests")
           "pytest tests/test_foo.py")))

(ert-deftest test-dev-fkeys-f6-cmd-for-python-source-file ()
  "Normal: a Python source file runs via `pytest tests/test_<stem>.py'."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'python nil "pkg/foo.py" "foo" "pkg")
           "pytest tests/test_foo.py")))

;;; Normal Cases — Go

(ert-deftest test-dev-fkeys-f6-cmd-for-go-test-file ()
  "Normal: a Go test file runs the package via `go test ./<rel-dir>'."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'go t "pkg/foo_test.go" "foo" "pkg")
           "go test ./pkg")))

(ert-deftest test-dev-fkeys-f6-cmd-for-go-source-file ()
  "Normal: a Go source file runs the same package via `go test ./<rel-dir>'.
Go test scope is per-package; running the source's package picks up its
test files."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'go nil "pkg/foo.go" "foo" "pkg")
           "go test ./pkg")))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f6-cmd-for-elisp-test-file-with-double-dash ()
  "Boundary: a per-helper test file runs only that file, not the whole
test-name prefix. `make test-file FILE=...' is precise; `test-name'
would over-match. Pass just the basename — the Makefile re-prepends
`tests/' itself."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'elisp t "tests/test-foo--bar.el" "foo" "tests")
           "make test-file FILE=test-foo--bar.el")))

(ert-deftest test-dev-fkeys-f6-cmd-for-go-source-at-root ()
  "Boundary: a Go source file at project root runs `go test ./'."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'go nil "main.go" "main" "")
           "go test ./")))

(ert-deftest test-dev-fkeys-f6-cmd-for-python-test-file-quotes-spaces ()
  "Boundary: a Python test file path with spaces is shell-escaped."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'python t "tests/dir with spaces/test_foo.py" "foo" "tests/dir with spaces")
           "pytest tests/dir\\ with\\ spaces/test_foo.py")))

(ert-deftest test-dev-fkeys-f6-cmd-for-elisp-source-quotes-test-regex ()
  "Boundary: an elisp source stem with shell metacharacters is escaped."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'elisp nil "modules/foo;bar.el" "foo;bar" "modules")
           "make test-name TEST=\\^test-foo\\;bar-")))

(ert-deftest test-dev-fkeys-f6-cmd-for-go-source-quotes-spaces ()
  "Boundary: a Go package path with spaces is shell-escaped."
  (should (string=
           (cj/--f6-test-runner-cmd-for
            'go nil "pkg/with spaces/foo.go" "foo" "pkg/with spaces")
           "go test ./pkg/with\\ spaces")))

;;; Error Cases

(ert-deftest test-dev-fkeys-f6-cmd-for-typescript-returns-nil ()
  "Error: TypeScript is punted for v1 and returns nil."
  (should (null (cj/--f6-test-runner-cmd-for
                 'typescript t "src/foo.test.ts" "foo" "src"))))

(ert-deftest test-dev-fkeys-f6-cmd-for-javascript-returns-nil ()
  "Error: JavaScript is punted for v1 and returns nil."
  (should (null (cj/--f6-test-runner-cmd-for
                 'javascript t "src/foo.test.js" "foo" "src"))))

(ert-deftest test-dev-fkeys-f6-cmd-for-unknown-returns-nil ()
  "Error: an unknown language returns nil."
  (should (null (cj/--f6-test-runner-cmd-for
                 'unknown nil "Makefile" "Makefile" ""))))

(provide 'test-dev-fkeys--f6-test-runner-cmd-for)
;;; test-dev-fkeys--f6-test-runner-cmd-for.el ends here
