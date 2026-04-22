;;; test-coverage-core--parse-lcov.el --- Tests for cj/--coverage-parse-lcov -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/--coverage-parse-lcov', the pure helper that
;; reads an LCOV file and returns a hash table of file → set of
;; covered line numbers.
;;
;; LCOV format (the subset we care about):
;;   SF:<source file>
;;   DA:<line>,<hit count>
;;   end_of_record
;;
;; A line counts as "covered" when its hit count is greater than zero.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

(defun test-coverage-parse-lcov--write-temp-lcov (content)
  "Write CONTENT to a temp file and return its path.
Caller is responsible for deleting the file."
  (let ((file (make-temp-file "test-lcov-" nil ".info")))
    (with-temp-file file
      (insert content))
    file))

;;; Normal cases

(ert-deftest test-coverage-parse-lcov-single-file-all-covered ()
  "Normal: one file with every line hit > 0 returns all lines in the set."
  (let* ((content "SF:/path/to/foo.el\nDA:10,1\nDA:11,2\nDA:12,3\nend_of_record\n")
         (file (test-coverage-parse-lcov--write-temp-lcov content)))
    (unwind-protect
        (let* ((result (cj/--coverage-parse-lcov file))
               (lines (gethash "/path/to/foo.el" result)))
          (should (hash-table-p result))
          (should (= 3 (hash-table-count lines)))
          (should (gethash 10 lines))
          (should (gethash 11 lines))
          (should (gethash 12 lines)))
      (delete-file file))))

(ert-deftest test-coverage-parse-lcov-multiple-files ()
  "Normal: multiple file records in one LCOV file are both parsed."
  (let* ((content (concat "SF:/a.el\nDA:1,1\nDA:2,1\nend_of_record\n"
                          "SF:/b.el\nDA:5,1\nend_of_record\n"))
         (file (test-coverage-parse-lcov--write-temp-lcov content)))
    (unwind-protect
        (let ((result (cj/--coverage-parse-lcov file)))
          (should (= 2 (hash-table-count result)))
          (should (gethash "/a.el" result))
          (should (gethash "/b.el" result)))
      (delete-file file))))

(ert-deftest test-coverage-parse-lcov-mixed-hits ()
  "Normal: lines with hit count 0 are excluded; positive counts included."
  (let* ((content "SF:/foo.el\nDA:1,0\nDA:2,1\nDA:3,0\nDA:4,5\nend_of_record\n")
         (file (test-coverage-parse-lcov--write-temp-lcov content)))
    (unwind-protect
        (let* ((result (cj/--coverage-parse-lcov file))
               (lines (gethash "/foo.el" result)))
          (should (= 2 (hash-table-count lines)))
          (should-not (gethash 1 lines))
          (should (gethash 2 lines))
          (should-not (gethash 3 lines))
          (should (gethash 4 lines)))
      (delete-file file))))

;;; Boundary cases

(ert-deftest test-coverage-parse-lcov-empty-file ()
  "Boundary: empty LCOV file returns an empty hash table, not nil."
  (let ((file (test-coverage-parse-lcov--write-temp-lcov "")))
    (unwind-protect
        (let ((result (cj/--coverage-parse-lcov file)))
          (should (hash-table-p result))
          (should (= 0 (hash-table-count result))))
      (delete-file file))))

(ert-deftest test-coverage-parse-lcov-file-with-spaces-in-path ()
  "Boundary: filename with spaces is parsed as one key."
  (let* ((content "SF:/my path/with spaces.el\nDA:1,1\nend_of_record\n")
         (file (test-coverage-parse-lcov--write-temp-lcov content)))
    (unwind-protect
        (let ((result (cj/--coverage-parse-lcov file)))
          (should (gethash "/my path/with spaces.el" result)))
      (delete-file file))))

(ert-deftest test-coverage-parse-lcov-extra-lcov-fields-ignored ()
  "Boundary: LF/LH/BRDA/FN fields don't break parsing; only DA matters."
  (let* ((content (concat "SF:/foo.el\n"
                          "FN:10,some-function\n"
                          "FNDA:1,some-function\n"
                          "DA:10,1\n"
                          "DA:11,1\n"
                          "LF:2\n"
                          "LH:2\n"
                          "BRDA:10,0,0,1\n"
                          "end_of_record\n"))
         (file (test-coverage-parse-lcov--write-temp-lcov content)))
    (unwind-protect
        (let* ((result (cj/--coverage-parse-lcov file))
               (lines (gethash "/foo.el" result)))
          (should (= 2 (hash-table-count lines)))
          (should (gethash 10 lines))
          (should (gethash 11 lines)))
      (delete-file file))))

(ert-deftest test-coverage-parse-lcov-all-zero-hits ()
  "Boundary: file with no covered lines returns an empty set for that file."
  (let* ((content "SF:/foo.el\nDA:1,0\nDA:2,0\nend_of_record\n")
         (file (test-coverage-parse-lcov--write-temp-lcov content)))
    (unwind-protect
        (let* ((result (cj/--coverage-parse-lcov file))
               (lines (gethash "/foo.el" result)))
          (should (hash-table-p lines))
          (should (= 0 (hash-table-count lines))))
      (delete-file file))))

;;; Error cases

(ert-deftest test-coverage-parse-lcov-missing-file-errors ()
  "Error: nonexistent file signals a user-error naming the path."
  (should-error (cj/--coverage-parse-lcov "/nonexistent/path/xyz.info")
                :type 'user-error))

(ert-deftest test-coverage-parse-lcov-malformed-da-line-skipped ()
  "Error: malformed DA lines are skipped; parsing continues on the rest."
  (let* ((content (concat "SF:/foo.el\n"
                          "DA:not-a-number,1\n"
                          "DA:10,1\n"
                          "DA:\n"
                          "DA:11,notanumber\n"
                          "DA:12,2\n"
                          "end_of_record\n"))
         (file (test-coverage-parse-lcov--write-temp-lcov content)))
    (unwind-protect
        (let* ((result (cj/--coverage-parse-lcov file))
               (lines (gethash "/foo.el" result)))
          (should (= 2 (hash-table-count lines)))
          (should (gethash 10 lines))
          (should (gethash 12 lines)))
      (delete-file file))))

(provide 'test-coverage-core--parse-lcov)
;;; test-coverage-core--parse-lcov.el ends here
