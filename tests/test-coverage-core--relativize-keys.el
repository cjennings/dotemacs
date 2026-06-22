;;; test-coverage-core--relativize-keys.el --- Tests for path-key normalization -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit + integration tests for `cj/--coverage-relativize-keys', the helper
;; that normalizes a file-path-keyed coverage table to repo-relative paths.
;;
;; The bug it fixes: `cj/--coverage-parse-simplecov' returns ABSOLUTE path
;; keys (simplecov/undercover emit absolute source paths), while
;; `cj/--coverage-parse-diff-output' returns repo-RELATIVE keys (git's
;; "+++ b/<path>").  `cj/--coverage-intersect' joins the two by exact string
;; key, so for the diff-aware scopes every changed file was classified
;; ":tracked nil" — zero matches ever.  Normalizing both tables to
;; repo-relative before the intersect makes the join work.
;;
;; The integration test drives the real parsers (a simplecov JSON fixture
;; with an absolute key + a git-diff string with the relative key) through
;; relativize + intersect, and asserts the file is tracked with the right
;; covered/uncovered split — the end-to-end reproduction of the bug.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

(defun test-coverage-relativize--hash-of-lines (pairs)
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

(ert-deftest test-coverage-relativize-absolute-key-made-relative ()
  "Normal: an absolute key is relativized against ROOT."
  (let* ((table (test-coverage-relativize--hash-of-lines
                 '(("/home/u/.emacs.d/modules/foo.el" 10 11))))
         (out (cj/--coverage-relativize-keys table "/home/u/.emacs.d")))
    (should (gethash "modules/foo.el" out))
    (should (null (gethash "/home/u/.emacs.d/modules/foo.el" out)))))

(ert-deftest test-coverage-relativize-preserves-line-set ()
  "Normal: the line-set value travels unchanged to the new key."
  (let* ((table (test-coverage-relativize--hash-of-lines
                 '(("/r/modules/foo.el" 4 8 15))))
         (out (cj/--coverage-relativize-keys table "/r"))
         (lines (gethash "modules/foo.el" out)))
    (should (hash-table-p lines))
    (should (gethash 4 lines))
    (should (gethash 8 lines))
    (should (gethash 15 lines))))

;;; Boundary cases

(ert-deftest test-coverage-relativize-already-relative-unchanged ()
  "Boundary: an already-relative key is left as-is, not re-relativized."
  (let* ((table (test-coverage-relativize--hash-of-lines
                 '(("modules/foo.el" 1 2))))
         (out (cj/--coverage-relativize-keys table "/home/u/.emacs.d")))
    (should (gethash "modules/foo.el" out))
    (should (= 1 (hash-table-count out)))))

(ert-deftest test-coverage-relativize-empty-table ()
  "Boundary: an empty table yields an empty table."
  (let ((out (cj/--coverage-relativize-keys (make-hash-table :test 'equal) "/r")))
    (should (hash-table-p out))
    (should (= 0 (hash-table-count out)))))

;;; Error cases

(ert-deftest test-coverage-relativize-nil-table-returns-empty ()
  "Error: a nil table returns an empty table rather than erroring."
  (let ((out (cj/--coverage-relativize-keys nil "/r")))
    (should (hash-table-p out))
    (should (= 0 (hash-table-count out)))))

;;; Integration — the real bug reproduction

(ert-deftest test-coverage-integration-absolute-report-relative-diff-tracks ()
  "Integration: a simplecov report (absolute keys) and a git diff (relative
keys) for the same file intersect as TRACKED once both are relativized.
This is the diff-aware-scope bug: without normalization the file reads
\":tracked nil\"."
  (let* ((root "/tmp/cov-root")
         (abs-path (concat root "/modules/foo.el"))
         (report (make-temp-file "cov-report-" nil ".json"))
         (diff (concat
                "diff --git a/modules/foo.el b/modules/foo.el\n"
                "index 1111111..2222222 100644\n"
                "--- a/modules/foo.el\n"
                "+++ b/modules/foo.el\n"
                "@@ -2,0 +2,3 @@\n"
                "+line two\n"
                "+line three\n"
                "+line four\n")))
    (unwind-protect
        (progn
          ;; simplecov array: index1=null, 2=hit, 3=0-hits, 4=hit
          ;; → covered lines {2, 4}
          (with-temp-file report
            (insert (format "{\"t\":{\"coverage\":{%S:[null,1,0,2]}}}" abs-path)))
          (let* ((covered (cj/--coverage-relativize-keys
                           (cj/--coverage-parse-simplecov report) root))
                 (changed (cj/--coverage-relativize-keys
                           (cj/--coverage-parse-diff-output diff) root))
                 (records (cj/--coverage-intersect covered changed))
                 (record (car records)))
            (should (= 1 (length records)))
            (should (equal "modules/foo.el" (plist-get record :path)))
            (should (eq t (plist-get record :tracked)))
            (should (equal '(2 3 4) (plist-get record :changed-lines)))
            (should (equal '(2 4) (plist-get record :covered-lines)))
            (should (equal '(3) (plist-get record :uncovered-lines)))))
      (delete-file report))))

(provide 'test-coverage-core--relativize-keys)
;;; test-coverage-core--relativize-keys.el ends here
