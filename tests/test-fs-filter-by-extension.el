;;; test-tool-library-fs-filter-by-extension.el --- ERT tests for cj/fs-filter-by-extension -*- lexical-binding: t; -*-

;; Author: gptel-tool-writer and cjennings
;; Keywords: tests, filesystem, tools

;;; Commentary:
;; ERT tests for the cj/fs-filter-by-extension function from tool-filesystem-library.el.
;; Place this file in ~/.emacs.d/tests/ and load it to run tests.

;;; Code:

(require 'ert)
(require 'f)
(require 'tool-filesystem-library)

(defvar cj/fs-test--temp-dir nil "Temporary test directory for fs-filter-by-extension tests.")

(defun cj/fs-test--setup ()
  "Set up temp directory for fs-filter-by-extension tests."
  (setq cj/fs-test--temp-dir (make-temp-file "fs-lib-test" t))
  ;; Create files
  (with-temp-buffer (insert "Org file") (write-file (f-join cj/fs-test--temp-dir "file1.org")))
  (with-temp-buffer (insert "Txt file") (write-file (f-join cj/fs-test--temp-dir "file2.txt")))
  (make-directory (f-join cj/fs-test--temp-dir "subdir") t))

(defun cj/fs-test--teardown ()
  "Clean up temp directory for fs-filter-by-extension tests."
  (when (and cj/fs-test--temp-dir (file-directory-p cj/fs-test--temp-dir))
    (delete-directory cj/fs-test--temp-dir t))
  (setq cj/fs-test--temp-dir nil))

(ert-deftest test-cj/fs-filter-by-extension-normal-match ()
  "Normal: match single extension in list."
  (cj/fs-test--setup)
  (unwind-protect
	  (let* ((infos (mapcar #'cj/fs-get-file-info (cj/fs-directory-entries cj/fs-test--temp-dir)))
			 (filtered (cj/fs-filter-by-extension infos "org")))
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.org")) filtered))
		(should-not (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file2.txt")) filtered)))
	(cj/fs-test--teardown)))

(ert-deftest test-cj/fs-filter-by-extension-normal-no-filter ()
  "Normal: no extension filter returns full list."
  (cj/fs-test--setup)
  (unwind-protect
	  (let* ((infos (mapcar #'cj/fs-get-file-info (cj/fs-directory-entries cj/fs-test--temp-dir)))
			 (filtered (cj/fs-filter-by-extension infos nil)))
		(should (= (length filtered) (length infos))))
	(cj/fs-test--teardown)))

(ert-deftest test-cj/fs-filter-by-extension-error-empty-list ()
  "Error: empty file info list handled."
  (should (equal (cj/fs-filter-by-extension nil "org") nil)))

(ert-deftest test-cj/fs-filter-by-extension-boundary-mixed-files ()
  "Boundary: mixed extensions and directories handled."
  (cj/fs-test--setup)
  (unwind-protect
	  (let* ((entries (cj/fs-directory-entries cj/fs-test--temp-dir))
			 (infos (mapcar #'cj/fs-get-file-info entries))
			 (filtered (cj/fs-filter-by-extension infos "org")))
		(should (cl-some (lambda (fi) (plist-get fi :directory)) filtered))
		(should (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file1.org")) filtered))
		(should-not (cl-some (lambda (fi) (string= (f-filename (plist-get fi :path)) "file2.txt")) filtered)))
	(cj/fs-test--teardown)))

(provide 'test-tool-library-fs-filter-by-extension)
;;; test-tool-library-fs-filter-by-extension.el ends here
