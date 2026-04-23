;;; test-coverage-core--parse-simplecov.el --- Tests for cj/--coverage-parse-simplecov -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--coverage-parse-simplecov', the pure helper
;; that reads a simplecov JSON report and returns a hash table of
;; file → set of covered line numbers.
;;
;; Simplecov JSON structure:
;;   { <test-name>: { "coverage": { <path>: [null | 0 | int, ...] } } }
;;
;; Array index i (0-based) corresponds to line (i+1).
;;   nil          — line is not executable (blank, comment)
;;   0            — line is executable but not hit
;;   positive int — hit count
;;
;; When the JSON has multiple top-level test-name keys, coverage is
;; unioned across them.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

(defun test-coverage-parse-simplecov--write-temp-json (content)
  "Write CONTENT (a JSON string) to a temp file; return its path."
  (let ((file (make-temp-file "test-simplecov-" nil ".json")))
	(with-temp-file file
	  (insert content))
	file))

;;; Normal cases

(ert-deftest test-coverage-parse-simplecov-single-file-all-hit ()
  "Normal: one file with every executable line hit."
  (let* ((content "{\"run\":{\"timestamp\":1,\"coverage\":{\"/foo.el\":[null,1,2,3]}}}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-parse-simplecov file))
			   (lines (gethash "/foo.el" result)))
		  (should (hash-table-p result))
		  (should (= 3 (hash-table-count lines)))
		  (should (gethash 2 lines))
		  (should (gethash 3 lines))
		  (should (gethash 4 lines))
		  (should-not (gethash 1 lines)))
	  (delete-file file))))

(ert-deftest test-coverage-parse-simplecov-multiple-files ()
  "Normal: multiple files under one test-name are both parsed."
  (let* ((content "{\"run\":{\"timestamp\":1,\"coverage\":{\"/a.el\":[1,1],\"/b.el\":[null,5]}}}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let ((result (cj/--coverage-parse-simplecov file)))
		  (should (= 2 (hash-table-count result)))
		  (should (gethash "/a.el" result))
		  (should (gethash "/b.el" result))
		  (should (= 2 (hash-table-count (gethash "/a.el" result))))
		  (should (= 1 (hash-table-count (gethash "/b.el" result)))))
	  (delete-file file))))

(ert-deftest test-coverage-parse-simplecov-mixed-hits ()
  "Normal: null (not executable) and 0 (not hit) are excluded."
  (let* ((content "{\"run\":{\"timestamp\":1,\"coverage\":{\"/foo.el\":[null,1,0,5,null,0,2]}}}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-parse-simplecov file))
			   (lines (gethash "/foo.el" result)))
		  (should (= 3 (hash-table-count lines)))
		  (should-not (gethash 1 lines))
		  (should (gethash 2 lines))
		  (should-not (gethash 3 lines))
		  (should (gethash 4 lines))
		  (should-not (gethash 5 lines))
		  (should-not (gethash 6 lines))
		  (should (gethash 7 lines)))
	  (delete-file file))))

;;; Boundary cases

(ert-deftest test-coverage-parse-simplecov-empty-json ()
  "Boundary: a JSON object with no test-name keys returns empty hash."
  (let* ((content "{}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let ((result (cj/--coverage-parse-simplecov file)))
		  (should (hash-table-p result))
		  (should (= 0 (hash-table-count result))))
	  (delete-file file))))

(ert-deftest test-coverage-parse-simplecov-file-with-spaces-in-path ()
  "Boundary: filename with spaces is parsed as one key."
  (let* ((content "{\"run\":{\"timestamp\":1,\"coverage\":{\"/my path/spaces.el\":[1]}}}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let ((result (cj/--coverage-parse-simplecov file)))
		  (should (gethash "/my path/spaces.el" result)))
	  (delete-file file))))

(ert-deftest test-coverage-parse-simplecov-all-zero-hits ()
  "Boundary: file with every executable line at 0 returns empty set."
  (let* ((content "{\"run\":{\"timestamp\":1,\"coverage\":{\"/foo.el\":[0,0,null,0]}}}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-parse-simplecov file))
			   (lines (gethash "/foo.el" result)))
		  (should (hash-table-p lines))
		  (should (= 0 (hash-table-count lines))))
	  (delete-file file))))

(ert-deftest test-coverage-parse-simplecov-all-null-entries ()
  "Boundary: all-null coverage array (no executable lines) returns empty set."
  (let* ((content "{\"run\":{\"timestamp\":1,\"coverage\":{\"/foo.el\":[null,null,null]}}}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-parse-simplecov file))
			   (lines (gethash "/foo.el" result)))
		  (should (hash-table-p lines))
		  (should (= 0 (hash-table-count lines))))
	  (delete-file file))))

(ert-deftest test-coverage-parse-simplecov-multiple-test-names-unioned ()
  "Boundary: multiple top-level test-name keys are unioned for the same file."
  (let* ((content "{\"run1\":{\"timestamp\":1,\"coverage\":{\"/foo.el\":[1,1,0,0]}},\"run2\":{\"timestamp\":2,\"coverage\":{\"/foo.el\":[0,0,1,1]}}}")
		 (file (test-coverage-parse-simplecov--write-temp-json content)))
	(unwind-protect
		(let* ((result (cj/--coverage-parse-simplecov file))
			   (lines (gethash "/foo.el" result)))
		  (should (= 4 (hash-table-count lines)))
		  (should (gethash 1 lines))
		  (should (gethash 2 lines))
		  (should (gethash 3 lines))
		  (should (gethash 4 lines)))
	  (delete-file file))))

;;; Error cases

(ert-deftest test-coverage-parse-simplecov-missing-file-errors ()
  "Error: nonexistent file signals user-error."
  (should-error (cj/--coverage-parse-simplecov "/nonexistent/path/xyz.json")
				:type 'user-error))

(ert-deftest test-coverage-parse-simplecov-malformed-json-errors ()
  "Error: malformed JSON input signals user-error naming the file."
  (let ((file (test-coverage-parse-simplecov--write-temp-json "{not valid json")))
	(unwind-protect
		(should-error (cj/--coverage-parse-simplecov file)
					  :type 'user-error)
	  (delete-file file))))

(provide 'test-coverage-core--parse-simplecov)
;;; test-coverage-core--parse-simplecov.el ends here
