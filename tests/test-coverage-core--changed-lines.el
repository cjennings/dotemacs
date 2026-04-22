;;; test-coverage-core--changed-lines.el --- Tests for cj/--coverage-changed-lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for:
;;   `cj/--coverage-parse-diff-output' (pure parser over git-diff text)
;;   `cj/--coverage-changed-lines' (scope → hash table, shells to git)
;;
;; The parser takes the output of `git diff --unified=0' and returns
;; a hash table of file → set of changed (added) line numbers in the
;; new version.  Hunk headers have the form:
;;   @@ -<old_start>[,<old_count>] +<new_start>[,<new_count>] @@
;; Changed lines are new_start through new_start + new_count - 1.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

;;; Fixtures

(defconst test-coverage-diff--simple-single-file
  "diff --git a/foo.el b/foo.el
index abc..def 100644
--- a/foo.el
+++ b/foo.el
@@ -10,1 +10,3 @@
-old line
+new line 1
+new line 2
+new line 3
"
  "Single-file diff with one hunk adding three lines at line 10.")

(defconst test-coverage-diff--multiple-files
  "diff --git a/a.el b/a.el
--- a/a.el
+++ b/a.el
@@ -1,0 +1,2 @@
+line 1
+line 2
diff --git a/b.el b/b.el
--- a/b.el
+++ b/b.el
@@ -5,0 +6,1 @@
+new line
"
  "Two-file diff.")

(defconst test-coverage-diff--new-file
  "diff --git a/new.el b/new.el
new file mode 100644
index 0000000..abc
--- /dev/null
+++ b/new.el
@@ -0,0 +1,3 @@
+line 1
+line 2
+line 3
"
  "New file: @@ -0,0 +1,3 @@ — three lines added to a brand-new file.")

(defconst test-coverage-diff--deletion-only
  "diff --git a/gone.el b/gone.el
--- a/gone.el
+++ b/gone.el
@@ -1,3 +1,0 @@
-removed 1
-removed 2
-removed 3
"
  "Deletion-only hunk: no new lines, file should map to an empty set.")

(defconst test-coverage-diff--binary-marker
  "diff --git a/image.png b/image.png
Binary files a/image.png and b/image.png differ
"
  "Binary file marker: no parseable hunks.")

(defconst test-coverage-diff--single-line-no-count
  "diff --git a/foo.el b/foo.el
--- a/foo.el
+++ b/foo.el
@@ -5 +5 @@
-old
+new
"
  "Hunk without a count means a single line (count defaults to 1).")

;;; Normal cases — parser

(ert-deftest test-coverage-parse-diff-single-hunk-three-lines ()
  "Normal: one hunk adding three lines gives {10, 11, 12}."
  (let* ((result (cj/--coverage-parse-diff-output
                  test-coverage-diff--simple-single-file))
         (lines (gethash "foo.el" result)))
    (should (= 1 (hash-table-count result)))
    (should (= 3 (hash-table-count lines)))
    (should (gethash 10 lines))
    (should (gethash 11 lines))
    (should (gethash 12 lines))))

(ert-deftest test-coverage-parse-diff-multiple-files ()
  "Normal: two files parsed separately with their own line sets."
  (let* ((result (cj/--coverage-parse-diff-output
                  test-coverage-diff--multiple-files))
         (a-lines (gethash "a.el" result))
         (b-lines (gethash "b.el" result)))
    (should (= 2 (hash-table-count result)))
    (should (= 2 (hash-table-count a-lines)))
    (should (gethash 1 a-lines))
    (should (gethash 2 a-lines))
    (should (= 1 (hash-table-count b-lines)))
    (should (gethash 6 b-lines))))

;;; Boundary cases — parser

(ert-deftest test-coverage-parse-diff-new-file ()
  "Boundary: new file hunk @@ -0,0 +1,3 @@ yields lines 1-3."
  (let* ((result (cj/--coverage-parse-diff-output
                  test-coverage-diff--new-file))
         (lines (gethash "new.el" result)))
    (should (= 3 (hash-table-count lines)))
    (should (gethash 1 lines))
    (should (gethash 2 lines))
    (should (gethash 3 lines))))

(ert-deftest test-coverage-parse-diff-deletion-only ()
  "Boundary: deletion-only hunk (+1,0) records the file with an empty line set."
  (let* ((result (cj/--coverage-parse-diff-output
                  test-coverage-diff--deletion-only))
         (lines (gethash "gone.el" result)))
    (should (hash-table-p lines))
    (should (= 0 (hash-table-count lines)))))

(ert-deftest test-coverage-parse-diff-binary-file-ignored ()
  "Boundary: binary files have no hunks; parser doesn't crash."
  (let ((result (cj/--coverage-parse-diff-output
                 test-coverage-diff--binary-marker)))
    (should (hash-table-p result))
    (should (= 0 (hash-table-count result)))))

(ert-deftest test-coverage-parse-diff-single-line-no-count ()
  "Boundary: @@ -5 +5 @@ means one line at line 5 (count defaults to 1)."
  (let* ((result (cj/--coverage-parse-diff-output
                  test-coverage-diff--single-line-no-count))
         (lines (gethash "foo.el" result)))
    (should (= 1 (hash-table-count lines)))
    (should (gethash 5 lines))))

(ert-deftest test-coverage-parse-diff-empty-input ()
  "Boundary: empty string input returns an empty hash table, not nil."
  (let ((result (cj/--coverage-parse-diff-output "")))
    (should (hash-table-p result))
    (should (= 0 (hash-table-count result)))))

;;; Error cases — parser

(ert-deftest test-coverage-parse-diff-malformed-hunk-header-skipped ()
  "Error: a malformed @@ line is skipped; surrounding valid hunks still parse."
  (let* ((input (concat "diff --git a/foo.el b/foo.el\n"
                        "--- a/foo.el\n"
                        "+++ b/foo.el\n"
                        "@@ this is not a valid hunk header @@\n"
                        "@@ -1,0 +10,2 @@\n"
                        "+ok1\n"
                        "+ok2\n"))
         (result (cj/--coverage-parse-diff-output input))
         (lines (gethash "foo.el" result)))
    (should (= 2 (hash-table-count lines)))
    (should (gethash 10 lines))
    (should (gethash 11 lines))))

;;; Smoke test — changed-lines (stubbed git invocation)

(ert-deftest test-coverage-changed-lines-working-tree-stubbed ()
  "Smoke: scope dispatches, shell is stubbed, parser is applied to the result."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) test-coverage-diff--simple-single-file)))
    (let* ((result (cj/--coverage-changed-lines 'working-tree))
           (lines (gethash "foo.el" result)))
      (should (= 1 (hash-table-count result)))
      (should (= 3 (hash-table-count lines))))))

(ert-deftest test-coverage-changed-lines-unknown-scope-errors ()
  "Error: an unknown scope symbol signals user-error."
  (should-error (cj/--coverage-changed-lines 'bogus-scope)
                :type 'user-error))

(provide 'test-coverage-core--changed-lines)
;;; test-coverage-core--changed-lines.el ends here
