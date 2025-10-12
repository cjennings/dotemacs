;;; test-tool-library-fs-list-directory-recursive.el --- ERT tests for cj/fs-list-directory-recursive -*- lexical-binding: t; -*-

;; Author: gptel-tool-writer and cjennings
;; Keywords: tests, filesystem, tools

;;; Commentary:
;; ERT tests for the cj/fs-list-directory-recursive function from tool-filesystem-library.el.
;; Place this file in ~/.emacs.d/tests/ and load it to run tests.

;;; Code:

(require 'ert)
(require 'f)
(require 'tool-filesystem-library)

(defvar cj/fs-test--temp-dir nil "Temporary temp directory for fs-list-directory-recursive tests.")

(defun cj/fs-test--setup ()
  "Set up temp directory for fs-list-directory-recursive tests."
  (setq cj/fs-test--temp-dir (make-temp-file "fs-lib-test" t))
  ;; Create test directory structure
  (make-directory (f-join cj/fs-test--temp-dir "subdir") t)
  (make-directory (f-join cj/fs-test--temp-dir "subdir2") t)
  (with-temp-buffer (insert "Test file 1") (write-file (f-join cj/fs-test--temp-dir "file1.org")))
  (with-temp-buffer (insert "Test file 2") (write-file (f-join cj/fs-test--temp-dir "subdir" "file2.txt")))
  (with-temp-buffer (insert "Test file 3") (write-file (f-join cj/fs-test--temp-dir "subdir2" "file3.org")))
  (make-directory (f-join cj/fs-test--temp-dir ".hiddendir") t)
  (with-temp-buffer (insert "Secret") (write-file (f-join cj/fs-test--temp-dir ".hiddendir" "secret.txt"))))

(defun cj/fs-test--teardown ()
  "Clean up temp directory for fs-list-directory-recursive tests."
  (when (and cj/fs-test--temp-dir (file-directory-p cj/fs-test--temp-dir))
	(delete-directory cj/fs-test--temp-dir t))
  (setq cj/fs-test--temp-dir nil))

(ert-deftest test-cj/fs-list-directory-recursive-normal-recursive-filter ()
  "Normal: recursive listing with filter."
  (cj/fs-test--setup)
  (unwind-protect
	  (let* ((filter-fn (lambda (fi) (string-suffix-p ".org" (f-filename (plist-get fi :path)))))
			 (files (cj/fs-list-directory-recursive cj/fs-test--temp-dir filter-fn)))
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.org")) files))
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file3.org")) files))
		(should-not (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file2.txt")) files)))
	(cj/fs-test--teardown)))

(ert-deftest test-cj/fs-list-directory-recursive-normal-max-depth ()
  "Normal: recursive listing with max depth limit."
  (cj/fs-test--setup)
  (unwind-protect
	  (let* ((filter-fn (lambda (_) t))
			 (files (cj/fs-list-directory-recursive cj/fs-test--temp-dir filter-fn 1)))
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.org")) files))
		(should-not (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file3.org")) files)))
	(cj/fs-test--teardown)))

(ert-deftest test-cj/fs-list-directory-recursive-error-non-directory ()
  "Error: non-directory input."
  (should-error (cj/fs-list-directory-recursive "/etc/hosts")))

(ert-deftest test-cj/fs-list-directory-recursive-boundary-empty-dir ()
  "Boundary: recursive listing in empty directory."
  (make-temp-file "empty-dir" t)
  (let ((empty (make-temp-file "empty-dir" t)))
	(unwind-protect
		(progn
		  (should (equal (cj/fs-list-directory-recursive empty) nil))
		  (delete-directory empty)))))

(provide 'test-tool-library-fs-list-directory-recursive)
;;; test-tool-library-fs-list-directory-recursive.el ends here
