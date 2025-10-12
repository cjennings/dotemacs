;;; test-tool-library-fs-list-directory-recursive-extra.el --- Additional ERT tests for cj/fs-list-directory-recursive -*- lexical-binding: t; -*-

;; Author: gptel-tool-writer and cjennings
;; Keywords: tests, filesystem, tools

;;; Commentary:
;; Additional tests to verify combined filters, boundary cases,
;; symlink protection, and permission issue handling in
;; cj/fs-list-directory-recursive.

;;; Code:

(require 'ert)
(require 'f)
(require 'tool-filesystem-library)

(defvar cj/fs-extra-test--temp-dir nil "Temporary temp directory for extra fs-list-directory-recursive tests.")

(defun cj/fs-extra-test--setup ()
  "Set up temp directory for extra fs-list-directory-recursive tests."
  (setq cj/fs-extra-test--temp-dir (make-temp-file "fs-lib-test" t))
  ;; Create directory structure
  (make-directory (f-join cj/fs-extra-test--temp-dir "subdir") t)
  (make-directory (f-join cj/fs-extra-test--temp-dir "subdir2") t)
  ;; Files at root level
  (with-temp-buffer (insert "Root org file") (write-file (f-join cj/fs-extra-test--temp-dir "file1.org")))
  (with-temp-buffer (insert "Root txt file") (write-file (f-join cj/fs-extra-test--temp-dir "file2.txt")))
  ;; Files in subdirectories
  (with-temp-buffer (insert "Subdir txt file") (write-file (f-join cj/fs-extra-test--temp-dir "subdir" "file3.txt")))
  (with-temp-buffer (insert "Subdir2 org file") (write-file (f-join cj/fs-extra-test--temp-dir "subdir2" "file4.org")))
  ;; Symlink to subdir2 inside subdir (potential for loops)
  (let ((target (f-join cj/fs-extra-test--temp-dir "subdir2"))
		(link (f-join cj/fs-extra-test--temp-dir "subdir" "link-to-subdir2")))
	(ignore-errors (delete-file link))
	(make-symbolic-link target link))

  ;; Create protected directory inside subdir to test permission issues
  (let ((protected-dir (f-join cj/fs-extra-test--temp-dir "subdir" "protected-dir")))
	(make-directory protected-dir t)
	;; Remove read & execute permissions
	(set-file-modes protected-dir #o000)))

(defun cj/fs-extra-test--teardown ()
  "Clean up temp directory for extra tests."
  (when (and cj/fs-extra-test--temp-dir (file-directory-p cj/fs-extra-test--temp-dir))
	;; Reset permissions to allow deletion
	(let ((protected-dir (f-join cj/fs-extra-test--temp-dir "subdir" "protected-dir")))
	  (when (file-exists-p protected-dir)
		(set-file-modes protected-dir #o755)))
	(delete-directory cj/fs-extra-test--temp-dir t))
  (setq cj/fs-extra-test--temp-dir nil))

(ert-deftest test-cj/fs-list-directory-recursive-normal-combined-filter-maxdepth ()
  "Normal: recursive listing combining extension filter and max depth."
  (cj/fs-extra-test--setup)
  (unwind-protect
	  (let* ((filter-fn (lambda (fi)
						  (string-suffix-p ".org" (f-filename (plist-get fi :path)))))
			 ;; max-depth 1 means root directory only, no recursion into subdirs
			 (files (cj/fs-list-directory-recursive cj/fs-extra-test--temp-dir filter-fn 1)))
		;; Should find only root level org files, not ones nested
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.org")) files))
		(should-not (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file4.org")) files)))
	(cj/fs-extra-test--teardown)))

(ert-deftest test-cj/fs-list-directory-recursive-boundary-max-depth-zero ()
  "Boundary: max depth zero lists no files (no recursion)."
  (cj/fs-extra-test--setup)
  (unwind-protect
	  (let ((files (cj/fs-list-directory-recursive cj/fs-extra-test--temp-dir nil 0)))
		;; Should be empty as depth 0 means no entries processed
		(should (equal files nil)))
	(cj/fs-extra-test--teardown)))

(ert-deftest test-cj/fs-list-directory-recursive-error-negative-max-depth ()
  "Error: negative max depth results in error."
  (cj/fs-extra-test--setup)
  (unwind-protect
	  (should-error (cj/fs-list-directory-recursive cj/fs-extra-test--temp-dir nil -1))
	(cj/fs-extra-test--teardown)))

(ert-deftest test-cj/fs-list-directory-recursive-boundary-symlink-no-infinite-loop ()
  "Boundary: symlinked directories do not cause infinite recursion."
  (cj/fs-extra-test--setup)
  (unwind-protect
	  (let ((files (cj/fs-list-directory-recursive cj/fs-extra-test--temp-dir nil 5)))
		;; There should be files from subdirs, but no infinite loop crashes
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file4.org")) files))
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.org")) files)))
	(cj/fs-extra-test--teardown)))

(ert-deftest test-cj/fs-list-directory-recursive-normal-permission-issue-handling ()
  "Normal: files in directories with permission issues are handled gracefully."
  (cj/fs-extra-test--setup)
  (unwind-protect
	  (let ((caught-warning nil))
		(cl-letf (((symbol-function 'message)
				   (lambda (&rest args)
					 (when (string-match "Warning:" (apply #'format args))
					   (setq caught-warning t)))))
		  (cj/fs-list-directory-recursive cj/fs-extra-test--temp-dir nil 5)
		  (should caught-warning)))
	(cj/fs-extra-test--teardown)))

(provide 'test-tool-library-fs-list-directory-recursive-extra)
;;; test-tool-library-fs-list-directory-recursive-extra.el ends here
