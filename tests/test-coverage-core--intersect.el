;;; test-coverage-core--intersect.el --- Tests for cj/--coverage-intersect -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--coverage-intersect', the pure helper that
;; combines covered-line data (from LCOV) with changed-line data
;; (from git diff) into per-file records ready for the report buffer.
;;
;; Return shape: a list of plists, one per changed file, sorted by path.
;;   (:path "modules/foo.el"
;;    :changed-lines (10 11 12)
;;    :covered-lines (10 11)   ; nil when the file isn't tracked
;;    :uncovered-lines (12)    ; nil when the file isn't tracked
;;    :tracked t)              ; nil when the file isn't in the LCOV data

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

(defun test-coverage-intersect--hash-of-lines (pairs)
  "Build a file → line-set hash table from PAIRS.
Each pair is (FILE . (LINES...)); LINES becomes a hash-table of line → t."
  (let ((result (make-hash-table :test 'equal)))
	(dolist (pair pairs)
	  (let ((lines (make-hash-table :test 'eql)))
		(dolist (line (cdr pair))
		  (puthash line t lines))
		(puthash (car pair) lines result)))
	result))

;;; Normal cases

(ert-deftest test-coverage-intersect-single-file-all-covered ()
  "Normal: one changed file, all lines covered."
  (let* ((covered (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 10 11 12))))
		 (changed (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 10 11 12))))
		 (result (cj/--coverage-intersect covered changed))
		 (record (car result)))
	(should (= 1 (length result)))
	(should (equal "foo.el" (plist-get record :path)))
	(should (equal '(10 11 12) (plist-get record :covered-lines)))
	(should (equal nil (plist-get record :uncovered-lines)))
	(should (equal '(10 11 12) (plist-get record :changed-lines)))
	(should (eq t (plist-get record :tracked)))))

(ert-deftest test-coverage-intersect-single-file-partial ()
  "Normal: partial coverage — 10 and 12 covered, 11 not."
  (let* ((covered (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 10 12 50))))
		 (changed (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 10 11 12))))
		 (result (cj/--coverage-intersect covered changed))
		 (record (car result)))
	(should (equal '(10 12) (plist-get record :covered-lines)))
	(should (equal '(11) (plist-get record :uncovered-lines)))
	(should (eq t (plist-get record :tracked)))))

(ert-deftest test-coverage-intersect-multiple-files-sorted ()
  "Normal: multiple files sorted by path."
  (let* ((covered (test-coverage-intersect--hash-of-lines
				   '(("a.el" 1) ("b.el" 5) ("c.el" 10))))
		 (changed (test-coverage-intersect--hash-of-lines
				   '(("c.el" 10) ("a.el" 1) ("b.el" 5))))
		 (result (cj/--coverage-intersect covered changed))
		 (paths (mapcar (lambda (r) (plist-get r :path)) result)))
	(should (equal '("a.el" "b.el" "c.el") paths))))

;;; Boundary cases

(ert-deftest test-coverage-intersect-file-not-tracked ()
  "Boundary: file has changed lines but isn't in the covered set at all."
  (let* ((covered (test-coverage-intersect--hash-of-lines
				   '(("tracked.el" 1 2 3))))
		 (changed (test-coverage-intersect--hash-of-lines
				   '(("README.md" 5 6 7))))
		 (result (cj/--coverage-intersect covered changed))
		 (record (car result)))
	(should (= 1 (length result)))
	(should (equal "README.md" (plist-get record :path)))
	(should (equal '(5 6 7) (plist-get record :changed-lines)))
	(should (equal nil (plist-get record :covered-lines)))
	(should (equal nil (plist-get record :uncovered-lines)))
	(should (eq nil (plist-get record :tracked)))))

(ert-deftest test-coverage-intersect-tracked-file-none-covered ()
  "Boundary: file is in LCOV but none of the changed lines are covered."
  (let* ((covered (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 1 2 3))))
		 (changed (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 10 11 12))))
		 (result (cj/--coverage-intersect covered changed))
		 (record (car result)))
	(should (eq t (plist-get record :tracked)))
	(should (equal nil (plist-get record :covered-lines)))
	(should (equal '(10 11 12) (plist-get record :uncovered-lines)))))

(ert-deftest test-coverage-intersect-empty-changed-lines ()
  "Boundary: file with empty changed-lines (deletion-only) appears with all lists empty."
  (let* ((covered (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 1 2))))
		 (changed (test-coverage-intersect--hash-of-lines
				   '(("gone.el"))))
		 (result (cj/--coverage-intersect covered changed))
		 (record (car result)))
	(should (= 1 (length result)))
	(should (equal "gone.el" (plist-get record :path)))
	(should (equal nil (plist-get record :changed-lines)))
	(should (equal nil (plist-get record :covered-lines)))
	(should (equal nil (plist-get record :uncovered-lines)))))

(ert-deftest test-coverage-intersect-empty-changed-returns-empty ()
  "Boundary: empty CHANGED hash table returns an empty list, not nil-as-error."
  (let* ((covered (test-coverage-intersect--hash-of-lines
				   '(("foo.el" 1 2 3))))
		 (changed (make-hash-table :test 'equal))
		 (result (cj/--coverage-intersect covered changed)))
	(should (listp result))
	(should (= 0 (length result)))))

(ert-deftest test-coverage-intersect-empty-covered-all-not-tracked ()
  "Boundary: empty COVERED means every changed file is not-tracked."
  (let* ((covered (make-hash-table :test 'equal))
		 (changed (test-coverage-intersect--hash-of-lines
				   '(("a.el" 1) ("b.el" 5))))
		 (result (cj/--coverage-intersect covered changed)))
	(should (= 2 (length result)))
	(dolist (record result)
	  (should (eq nil (plist-get record :tracked))))))

;;; Error cases

(ert-deftest test-coverage-intersect-nil-inputs-return-empty ()
  "Error: nil CHANGED returns an empty list rather than erroring."
  (should (equal nil (cj/--coverage-intersect nil nil)))
  (should (equal nil (cj/--coverage-intersect
					  (test-coverage-intersect--hash-of-lines '(("x" 1)))
					  nil))))

(provide 'test-coverage-core--intersect)
;;; test-coverage-core--intersect.el ends here
