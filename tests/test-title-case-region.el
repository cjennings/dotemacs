;;; test-title-case-region.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the title-case region function in custom-functions.el

;; Note on Title Case
;; Title case is a capitalization convention where major words are
;; capitalized,and most minor words are lowercase.  Nouns,verbs (including
;; linking verbs), adjectives, adverbs,pronouns,and all words of four letters or
;; more are considered major words. Short (i.e., three letters or fewer)
;; conjunctions, short prepositions,and all articles are considered minor
;; words."

;; positive case (single line, all lowercase, no skip words)
;; positive case (six lines, mixed case, skip words)
;; negative case (single line, all skip-words)
;; negative case (a long empty string)


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




(provide 'test-title-case-region)
;;; test-title-case-region.el ends here.
