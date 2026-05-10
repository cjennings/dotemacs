;;; test-system-lib-shell-quote-argument-readable.el --- Tests for cj/shell-quote-argument-readable -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/shell-quote-argument-readable' is the readable-quote helper.
;; When ARGUMENT consists only of characters safe in a shell string,
;; return it unchanged so the surrounding command stays human-readable
;; (compile / test command lines, log inspection).  When it contains
;; whitespace or shell metacharacters, fall back to
;; `shell-quote-argument' so the result is safe to interpolate.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-lib)

(ert-deftest test-cj-shell-quote-argument-readable-simple-path-unchanged ()
  "Normal: an alphanumeric path is returned unchanged."
  (should (equal (cj/shell-quote-argument-readable "tests/test-foo.el")
                 "tests/test-foo.el")))

(ert-deftest test-cj-shell-quote-argument-readable-test-regex-unchanged ()
  "Normal: a typical test-name regex with safe punctuation is unchanged."
  (should (equal (cj/shell-quote-argument-readable "^test-dev-fkeys-")
                 "^test-dev-fkeys-")))

(ert-deftest test-cj-shell-quote-argument-readable-flags-unchanged ()
  "Normal: arguments with `=', `+', `:' read as-is (FLAG=value, addr:port)."
  (should (equal (cj/shell-quote-argument-readable "FILE=tests/test-foo.el")
                 "FILE=tests/test-foo.el"))
  (should (equal (cj/shell-quote-argument-readable "host:1234")
                 "host:1234")))

(ert-deftest test-cj-shell-quote-argument-readable-spaces-quoted ()
  "Boundary: an argument containing spaces falls back to shell-quote-argument."
  (let ((quoted (cj/shell-quote-argument-readable "path/with space.el")))
    ;; shell-quote-argument either backslash-escapes or wraps in single
    ;; quotes; either way the raw input cannot survive verbatim.
    (should-not (equal quoted "path/with space.el"))
    (should (string-match-p "space" quoted))))

(ert-deftest test-cj-shell-quote-argument-readable-shell-metachars-quoted ()
  "Boundary: arguments with `$', `;', `&', backticks, `*' are quoted."
  (dolist (arg '("$HOME" "a;b" "foo&bar" "back`tick`" "glob*"))
    (let ((quoted (cj/shell-quote-argument-readable arg)))
      (should-not (equal quoted arg)))))

(ert-deftest test-cj-shell-quote-argument-readable-empty-string-quoted ()
  "Boundary: empty string is unsafe in a command line and is quoted."
  (let ((quoted (cj/shell-quote-argument-readable "")))
    (should-not (equal quoted ""))))

(provide 'test-system-lib-shell-quote-argument-readable)
;;; test-system-lib-shell-quote-argument-readable.el ends here
