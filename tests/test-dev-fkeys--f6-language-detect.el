;;; test-dev-fkeys--f6-language-detect.el --- Tests for cj/--f6-language-detect -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the language classifier used by the F6 test runner. Detection
;; runs purely off the file extension. The classifier returns one of
;; \\='elisp / \\='python / \\='go / \\='typescript / \\='javascript / \\='unknown.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'dev-fkeys)

;;; Normal Cases

(ert-deftest test-dev-fkeys-f6-language-detect-elisp ()
  "Normal: .el classifies as elisp."
  (should (eq (cj/--f6-language-detect "foo.el") 'elisp)))

(ert-deftest test-dev-fkeys-f6-language-detect-python ()
  "Normal: .py classifies as python."
  (should (eq (cj/--f6-language-detect "foo.py") 'python)))

(ert-deftest test-dev-fkeys-f6-language-detect-go ()
  "Normal: .go classifies as go."
  (should (eq (cj/--f6-language-detect "foo.go") 'go)))

(ert-deftest test-dev-fkeys-f6-language-detect-typescript ()
  "Normal: .ts and .tsx classify as typescript."
  (should (eq (cj/--f6-language-detect "foo.ts") 'typescript))
  (should (eq (cj/--f6-language-detect "foo.tsx") 'typescript)))

(ert-deftest test-dev-fkeys-f6-language-detect-javascript ()
  "Normal: .js and .jsx classify as javascript."
  (should (eq (cj/--f6-language-detect "foo.js") 'javascript))
  (should (eq (cj/--f6-language-detect "foo.jsx") 'javascript)))

;;; Boundary Cases

(ert-deftest test-dev-fkeys-f6-language-detect-with-directory-path ()
  "Boundary: classifier ignores the path, looks only at extension."
  (should (eq (cj/--f6-language-detect "/abs/path/to/foo.el") 'elisp))
  (should (eq (cj/--f6-language-detect "rel/dir/foo.py") 'python)))

(ert-deftest test-dev-fkeys-f6-language-detect-test-file-name ()
  "Boundary: a filename that is itself a test file still classifies by extension.
The test-file-vs-source distinction is a separate helper."
  (should (eq (cj/--f6-language-detect "tests/test-foo.el") 'elisp))
  (should (eq (cj/--f6-language-detect "tests/test_foo.py") 'python))
  (should (eq (cj/--f6-language-detect "pkg/foo_test.go") 'go)))

(ert-deftest test-dev-fkeys-f6-language-detect-uppercase-extension ()
  "Boundary: extension match is case-insensitive (.PY, .EL, etc.)."
  (should (eq (cj/--f6-language-detect "foo.EL") 'elisp))
  (should (eq (cj/--f6-language-detect "foo.PY") 'python)))

(ert-deftest test-dev-fkeys-f6-language-detect-no-extension ()
  "Boundary: a filename with no extension classifies as unknown."
  (should (eq (cj/--f6-language-detect "Makefile") 'unknown))
  (should (eq (cj/--f6-language-detect "README") 'unknown)))

(ert-deftest test-dev-fkeys-f6-language-detect-unsupported-extension ()
  "Boundary: an extension we do not classify returns unknown."
  (should (eq (cj/--f6-language-detect "foo.rs") 'unknown))
  (should (eq (cj/--f6-language-detect "foo.rb") 'unknown))
  (should (eq (cj/--f6-language-detect "foo.txt") 'unknown)))

;;; Error Cases

(ert-deftest test-dev-fkeys-f6-language-detect-nil-filename ()
  "Error: nil filename returns unknown without erroring (defensive against
buffers that aren't backed by files)."
  (should (eq (cj/--f6-language-detect nil) 'unknown)))

(ert-deftest test-dev-fkeys-f6-language-detect-empty-filename ()
  "Error: empty string returns unknown."
  (should (eq (cj/--f6-language-detect "") 'unknown)))

(provide 'test-dev-fkeys--f6-language-detect)
;;; test-dev-fkeys--f6-language-detect.el ends here
