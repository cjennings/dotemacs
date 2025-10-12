;;; test-fixup-whitespace.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Test cj/fixup-whitespace-line-or-region in custom-functions.el

;; The function under test should:
;; - ensure there is exactly one space between words
;; - remove tab characters
;; - remove leading and trailing whitespace
;; - operate on a line, or a region, if selected

;;; Code:


(require 'ert)
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'custom-functions)

(ert-deftest test-cj/fixup-whitespace-positive-first-line-only ()
  "Test a positive case with two lines.
Both lines have whitespace at the beginning and the end. This tests that when
this function is called on the first line, only that line is affected."
  (let ((testdata  "    Hello,  world!  \n  Foo     bar  ")
		(expected  "Hello, world!\n  Foo     bar  ")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (cj/fixup-whitespace-line-or-region)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-positive-first-line-only-tabs ()
  "Test a positive case with two lines.
Both lines have extraneous whitespace at the beginning and the end, includuing
tabs. This tests that when this function is called on the first line, only that
line is affected."
  (let ((testdata  "    Hello,\t  world!  \n  Foo\tbar  ")
		(expected  "Hello, world!\n  Foo\tbar  ")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (cj/fixup-whitespace-line-or-region)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-positive-first-line-only-tabs2 ()
  "Test a positive case with two lines.
Both lines have extraneous whitespace at the beginning and the end, includuing
tabs. This tests that when this function is called on the first line, only that
line is affected."
  (let ((testdata  "\t    Hello,\tworld!  \n  Foo\t     bar\t  ")
		(expected  "Hello, world!\n  Foo\t     bar\t  ")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (cj/fixup-whitespace-line-or-region)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-negative-first-line-only ()
  "Test a negative case with two lines.
Only the second line has whitespace at the beginning and the end. This tests
that when this function is called on the first line, neither line changes."
  (let ((testdata  "Hello, world!\n  Foo     bar    ")
		(expected  "Hello, world!\n  Foo     bar    ")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (cj/fixup-whitespace-line-or-region)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-positive-second-line-only ()
  "Test a positive case with two lines.
Both lines have whitespace at the beginning and the end. This tests that when
function is called on the second line, only that line is affected."
  (let ((testdata  "    Hello,  world!  \n  Foo     bar  ")
		(expected  "    Hello,  world!  \nFoo bar")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (forward-line)
      (cj/fixup-whitespace-line-or-region)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-negative-second-line-only ()
  "Test a negative case with two lines.
Only the first line has whitespace at the beginning and the end. This tests
that when this function is called on the first line, neither line changes."
  (let ((testdata  "    Hello,  world!  \nFoo bar")
		(expected  "    Hello,  world!  \nFoo bar")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (forward-line)
      (cj/fixup-whitespace-line-or-region)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-positive-region ()
  "Test a positive case with a region.
Two lines have whitespace at the beginning, the middle, and the end. This tests
that when this function is called with a region, all whitespace is cleaned up as
expected."
  (let ((testdata  "    Hello,  world!  \n  Foo     bar   ")
		(expected  "Hello, world!\nFoo bar")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (cj/fixup-whitespace-line-or-region t)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-positive-region-tabs ()
  "Test a positive case with a region and tabs.
Two lines have extraneous whitespace at the beginning, the middle, and the end.
This tests that when this function is called with a region, all whitespace is
cleaned up as expected."
  (let ((testdata  "  \t \t  Hello,  world!  \n  Foo\t bar   ")
		(expected  "Hello, world!\nFoo bar")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (cj/fixup-whitespace-line-or-region t)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(ert-deftest test-cj/fixup-whitespace-negative-region ()
  "Test a negative case with a region.
Two lines are inserted, neither of which have extraneous whitespace. This tests
that when this  function is called with a region, there's no unwanted
side-effects and nothing changes."
  (let ((testdata  "Hello, world!\nFoo bar")
		(expected  "Hello, world!\nFoo bar")
		(actual))
    (with-temp-buffer
      (insert testdata)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (cj/fixup-whitespace-line-or-region t)
      (setq actual (buffer-string))
      (should (string= actual expected)))))

(provide 'test-fixup-whitespace)
;;; test-fixup-whitespace.el ends here.
