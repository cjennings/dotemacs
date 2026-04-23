;;; test-coverage-core--format-report.el --- Tests for cj/--coverage-format-report -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--coverage-format-report', the pure helper that
;; renders intersect records into the text shown in the report buffer.
;;
;; Input: list of plists from `cj/--coverage-intersect':
;;   (:path PATH
;;    :changed-lines LIST
;;    :covered-lines LIST
;;    :uncovered-lines LIST
;;    :tracked BOOL)
;;
;; Output: a string suitable for insertion into a compilation-mode
;; buffer.  Uncovered-line entries use the format "<path>:<line>:
;; uncovered" so `compilation-error-regexp-alist' picks them up for
;; `next-error' navigation.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

(defun test-coverage-format-report--record (path changed covered uncovered tracked)
  "Build an intersect record plist for use in tests."
  (list :path path
		:changed-lines changed
		:covered-lines covered
		:uncovered-lines uncovered
		:tracked tracked))

;;; Normal cases

(ert-deftest test-coverage-format-report-partial-coverage ()
  "Normal: one file with 2 of 3 changed lines covered."
  (let* ((records (list (test-coverage-format-report--record
						 "modules/foo.el" '(10 11 12) '(10 11) '(12) t)))
		 (output (cj/--coverage-format-report records "Staged")))
	(should (string-match-p "Staged" output))
	;; Summary line shows the counts
	(should (string-match-p "2 of 3" output))
	(should (string-match-p "66" output))  ; 66.7%
	;; Uncovered line uses compilation-friendly format
	(should (string-match-p "modules/foo\\.el:12: uncovered" output))
	;; Covered lines aren't listed individually
	(should-not (string-match-p "modules/foo\\.el:10: uncovered" output))
	(should-not (string-match-p "modules/foo\\.el:11: uncovered" output))))

(ert-deftest test-coverage-format-report-fully-covered ()
  "Normal: a fully-covered file lists in the \"Fully covered\" section."
  (let* ((records (list (test-coverage-format-report--record
						 "modules/baz.el" '(1 2 3) '(1 2 3) nil t)))
		 (output (cj/--coverage-format-report records "Working tree")))
	(should (string-match-p "Fully covered" output))
	(should (string-match-p "modules/baz\\.el" output))
	(should (string-match-p "3/3" output))
	;; No uncovered entries for this file
	(should-not (string-match-p ":[0-9]+: uncovered" output))))

(ert-deftest test-coverage-format-report-mixed-records ()
  "Normal: a mix of covered, partial, and not-tracked files."
  (let* ((records (list
				   (test-coverage-format-report--record
					"modules/foo.el" '(10 11) '(10) '(11) t)
				   (test-coverage-format-report--record
					"README.md" '(5 6 7) nil nil nil)
				   (test-coverage-format-report--record
					"modules/baz.el" '(1 2) '(1 2) nil t)))
		 (output (cj/--coverage-format-report records "Branch vs main")))
	(should (string-match-p "Uncovered lines" output))
	(should (string-match-p "Not tracked" output))
	(should (string-match-p "Fully covered" output))
	(should (string-match-p "modules/foo\\.el:11: uncovered" output))
	(should (string-match-p "README\\.md" output))
	(should (string-match-p "modules/baz\\.el" output))))

;;; Boundary cases

(ert-deftest test-coverage-format-report-empty ()
  "Boundary: no records produces a clear \"nothing to report\" message."
  (let ((output (cj/--coverage-format-report nil "Staged")))
	(should (string-match-p "No changes" output))
	(should (string-match-p "Staged" output))))

(ert-deftest test-coverage-format-report-100-percent ()
  "Boundary: 100% coverage shows a success line."
  (let* ((records (list (test-coverage-format-report--record
						 "modules/foo.el" '(1 2 3) '(1 2 3) nil t)))
		 (output (cj/--coverage-format-report records "Working tree")))
	(should (string-match-p "3 of 3" output))
	(should (string-match-p "100" output))
	;; No "Uncovered lines" section when there are none
	(should-not (string-match-p "Uncovered lines" output))))

(ert-deftest test-coverage-format-report-only-not-tracked ()
  "Boundary: every changed file is not-tracked (README-only edits)."
  (let* ((records (list
				   (test-coverage-format-report--record
					"README.md" '(1 2) nil nil nil)
				   (test-coverage-format-report--record
					"CHANGELOG.md" '(5) nil nil nil)))
		 (output (cj/--coverage-format-report records "Staged")))
	(should (string-match-p "Not tracked" output))
	;; Summary denominator excludes not-tracked lines
	(should (string-match-p "0 of 0" output))
	(should-not (string-match-p "Uncovered lines" output))))

(ert-deftest test-coverage-format-report-deletion-only-skipped ()
  "Boundary: deletion-only records (empty changed-lines) are excluded from display."
  (let* ((records (list
				   (test-coverage-format-report--record
					"modules/foo.el" '(10 11) '(10 11) nil t)
				   (test-coverage-format-report--record
					"gone.el" nil nil nil t)))
		 (output (cj/--coverage-format-report records "Working tree")))
	;; The deletion-only file shouldn't appear anywhere.
	(should-not (string-match-p "gone\\.el" output))
	;; Summary still shows the normal-case counts.
	(should (string-match-p "2 of 2" output))))

(provide 'test-coverage-core--format-report)
;;; test-coverage-core--format-report.el ends here
