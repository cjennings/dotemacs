;;; test-coverage-core--whole-project.el --- Tests for whole-project coverage scope -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the helpers that power the "Whole project" scope in
;; `cj/coverage-report':
;;
;;   `cj/--coverage-simplecov-executable-lines' — returns all executable
;;   lines per file from a simplecov JSON report, including lines with
;;   zero hits (symmetric with `cj/--coverage-parse-simplecov', which
;;   returns only hit lines).
;;
;;   `cj/--coverage-format-summary' — renders intersect records as a
;;   per-file percentage summary, sorted by coverage ascending (worst
;;   first, most useful when scanning for what to test next).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

(defun test-coverage-whole--write-json (content)
  "Write CONTENT to a temp file; return its path."
  (let ((file (make-temp-file "test-whole-" nil ".json")))
	(with-temp-file file (insert content))
	file))

(defun test-coverage-whole--record (path changed covered uncovered tracked)
  "Build an intersect record."
  (list :path path
		:changed-lines changed
		:covered-lines covered
		:uncovered-lines uncovered
		:tracked tracked))

;;; cj/--coverage-simplecov-executable-lines

(ert-deftest test-coverage-simplecov-executable-lines-basic ()
  "Normal: executable lines include both hit (>0) and 0-hit entries."
  (let* ((content "{\"run\":{\"timestamp\":1,\"coverage\":{\"/foo.el\":[null,1,0,2,null,0]}}}")
		 (file (test-coverage-whole--write-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-simplecov-executable-lines file))
			   (lines (gethash "/foo.el" result)))
		  (should (= 4 (hash-table-count lines)))
		  (should-not (gethash 1 lines))        ; null
		  (should (gethash 2 lines))            ; 1 hit
		  (should (gethash 3 lines))            ; 0 hits (still executable)
		  (should (gethash 4 lines))            ; 2 hits
		  (should-not (gethash 5 lines))        ; null
		  (should (gethash 6 lines)))           ; 0 hits
	  (delete-file file))))

(ert-deftest test-coverage-simplecov-executable-lines-all-null ()
  "Boundary: all-null array returns empty set (not nil)."
  (let* ((content "{\"run\":{\"coverage\":{\"/foo.el\":[null,null,null]}}}")
		 (file (test-coverage-whole--write-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-simplecov-executable-lines file))
			   (lines (gethash "/foo.el" result)))
		  (should (hash-table-p lines))
		  (should (= 0 (hash-table-count lines))))
	  (delete-file file))))

(ert-deftest test-coverage-simplecov-executable-lines-multiple-runs-unioned ()
  "Boundary: multiple test-name keys are unioned (matches parse-simplecov semantics)."
  (let* ((content (concat "{\"run1\":{\"coverage\":{\"/foo.el\":[1,null,null]}},"
						  "\"run2\":{\"coverage\":{\"/foo.el\":[null,null,0]}}}"))
		 (file (test-coverage-whole--write-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-simplecov-executable-lines file))
			   (lines (gethash "/foo.el" result)))
		  (should (= 2 (hash-table-count lines)))
		  (should (gethash 1 lines))
		  (should (gethash 3 lines)))
	  (delete-file file))))

(ert-deftest test-coverage-simplecov-executable-lines-missing-file ()
  "Error: nonexistent file signals user-error."
  (should-error (cj/--coverage-simplecov-executable-lines
				 "/nonexistent/path/xyz.json")
				:type 'user-error))

;;; cj/--coverage-format-summary

(ert-deftest test-coverage-format-summary-multiple-files-sorted ()
  "Normal: per-file summary is sorted by coverage percentage ascending."
  (let* ((records (list
				   (test-coverage-whole--record
					"high.el" '(1 2 3 4) '(1 2 3 4) nil t)     ; 100%
				   (test-coverage-whole--record
					"low.el" '(1 2 3 4 5 6 7 8 9 10)
					'(1) '(2 3 4 5 6 7 8 9 10) t)              ; 10%
				   (test-coverage-whole--record
					"mid.el" '(1 2) '(1) '(2) t)))             ; 50%
		 (output (cj/--coverage-format-summary records "Whole project"))
		 ;; Position of each filename in the output string
		 (pos-low (string-match "low\\.el" output))
		 (pos-mid (string-match "mid\\.el" output))
		 (pos-high (string-match "high\\.el" output)))
	(should pos-low)
	(should pos-mid)
	(should pos-high)
	;; Lower coverage files appear first
	(should (< pos-low pos-mid))
	(should (< pos-mid pos-high))
	;; Summary totals appear
	(should (string-match-p "6 of 16" output))
	(should (string-match-p "Whole project" output))))

(ert-deftest test-coverage-format-summary-shows-percentages ()
  "Normal: each file shows its own hit/total and percentage."
  (let* ((records (list (test-coverage-whole--record
						 "foo.el" '(1 2 3 4) '(1 2 3) '(4) t)))   ; 75%
		 (output (cj/--coverage-format-summary records "Whole project")))
	(should (string-match-p "3/4" output))
	(should (string-match-p "75" output))))

(ert-deftest test-coverage-format-summary-empty ()
  "Boundary: empty records produces a clear \"nothing to report\" message."
  (let ((output (cj/--coverage-format-summary nil "Whole project")))
	(should (string-match-p "No coverage data" output))))

(ert-deftest test-coverage-format-summary-not-tracked-files-excluded ()
  "Boundary: files with :tracked nil don't appear in the summary."
  (let* ((records (list
				   (test-coverage-whole--record
					"modules/foo.el" '(1) '(1) nil t)
				   (test-coverage-whole--record
					"README.md" '(5 6) nil nil nil)))
		 (output (cj/--coverage-format-summary records "Whole project")))
	(should (string-match-p "modules/foo\\.el" output))
	(should-not (string-match-p "README\\.md" output))))

(provide 'test-coverage-core--whole-project)
;;; test-coverage-core--whole-project.el ends here
