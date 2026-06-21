;;; test-dev-fkeys--f6-current-file-tests-impl.el --- Tests for cj/--f6-current-file-tests-impl -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the "Current file's tests" orchestrator. Takes (FILE
;; PROJECT-ROOT), composes the per-language test-runner command, and runs
;; it through `compile' with `default-directory' bound to PROJECT-ROOT.
;; Errors when no file, no project root, or no test runner is available
;; for the file's language.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-elisp-source ()
  "Normal: an elisp source file runs `make test-name TEST=^test-<stem>-'.

Components integrated:
- `cj/--f6-current-file-tests-impl' (unit under test)
- `cj/--f6-language-detect' (real)
- `cj/--f6-source-stem' (real)
- `cj/--f6-buffer-is-test-file-p' (real)
- `cj/--f6-test-runner-cmd-for' (real)
- `compile' (MOCKED — captures cmd and default-directory)"
  (let (seen-cmd seen-dir)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq seen-cmd cmd
                                   seen-dir default-directory))))
      (cj/--f6-current-file-tests-impl
       "/home/u/proj/modules/foo.el"
       "/home/u/proj/")
      (should (string= seen-cmd "make test-name TEST=^test-foo-"))
      (should (string= (file-name-as-directory seen-dir)
                       (file-name-as-directory "/home/u/proj/"))))))

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-elisp-test-file ()
  "Normal: an elisp test file runs `make test-file FILE=<rel-path>'."
  (let (seen-cmd)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq seen-cmd cmd))))
      (cj/--f6-current-file-tests-impl
       "/home/u/proj/tests/test-foo.el"
       "/home/u/proj/")
      (should (string= seen-cmd "make test-file FILE=test-foo.el")))))

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-python-source ()
  "Normal: a Python source file maps to `pytest tests/test_<stem>.py'."
  (let (seen-cmd)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq seen-cmd cmd))))
      (cj/--f6-current-file-tests-impl
       "/home/u/proj/pkg/foo.py"
       "/home/u/proj/")
      (should (string= seen-cmd "pytest tests/test_foo.py")))))

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-go-source ()
  "Normal: a Go source file runs the package via `go test ./<rel-dir>'."
  (let (seen-cmd)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq seen-cmd cmd))))
      (cj/--f6-current-file-tests-impl
       "/home/u/proj/pkg/foo.go"
       "/home/u/proj/")
      (should (string= seen-cmd "go test ./pkg")))))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-go-source-at-root ()
  "Boundary: a Go source file at project root runs `go test ./'."
  (let (seen-cmd)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq seen-cmd cmd))))
      (cj/--f6-current-file-tests-impl
       "/home/u/proj/main.go"
       "/home/u/proj/")
      (should (string= seen-cmd "go test ./")))))

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-elisp-double-dash-test ()
  "Boundary: a per-helper elisp test file runs `make test-file FILE=...' so
just that file's tests run, not the whole module's prefix."
  (let (seen-cmd)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq seen-cmd cmd))))
      (cj/--f6-current-file-tests-impl
       "/home/u/proj/tests/test-foo--bar.el"
       "/home/u/proj/")
      (should (string= seen-cmd "make test-file FILE=test-foo--bar.el")))))

;;; Error Cases

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-nil-file-errors ()
  "Error: nil FILE signals a user-error."
  (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil)))
    (should-error (cj/--f6-current-file-tests-impl nil "/home/u/proj/")
                  :type 'user-error)))

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-nil-root-errors ()
  "Error: nil PROJECT-ROOT signals a user-error."
  (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil)))
    (should-error (cj/--f6-current-file-tests-impl
                   "/home/u/proj/modules/foo.el" nil)
                  :type 'user-error)))

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-typescript-runs-jest ()
  "TypeScript now routes to the `npx --no-install jest|vitest <path>'
runner instead of erroring as unsupported."
  (let ((compile-called nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd) (setq compile-called cmd)))
              ((symbol-function 'executable-find) (lambda (_ &rest _) nil)))
      (cj/--f6-current-file-tests-impl
       "/home/u/proj/src/foo.test.ts" "/home/u/proj/")
      (should (stringp compile-called))
      (should (string-match-p "jest src/foo.test.ts" compile-called)))))

(ert-deftest test-dev-fkeys-f6-current-file-tests-impl-unknown-language-errors ()
  "Error: an unknown extension signals a user-error rather than running
something the user didn't ask for."
  (cl-letf (((symbol-function 'compile) (lambda (_cmd) nil)))
    (should-error (cj/--f6-current-file-tests-impl
                   "/home/u/proj/Makefile" "/home/u/proj/")
                  :type 'user-error)))

(provide 'test-dev-fkeys--f6-current-file-tests-impl)
;;; test-dev-fkeys--f6-current-file-tests-impl.el ends here
