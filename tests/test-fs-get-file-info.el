;;; test-tool-library-fs-get-file-info.el --- ERT tests for cj/fs-get-file-info -*- lexical-binding: t; -*-

;; Author: gptel-tool-writer and cjennings
;; Keywords: tests, filesystem, tools

;;; Commentary:
;; ERT tests for the cj/fs-get-file-info function from tool-filesystem-library.el.
;; Place this file in ~/.emacs.d/tests/ and load it to run tests.

;;; Code:

(require 'ert)
(require 'f)
(require 'tool-filesystem-library)

(defvar cj/fs-test--temp-dir nil "Temporary test directory for fs-get-file-info tests.")

(defun cj/fs-test--setup ()
  "Setup temporary directory for fs-get-file-info tests."
  (setq cj/fs-test--temp-dir (make-temp-file "fs-lib-test" t))
  ;; Create test files and directories
  (make-directory (f-join cj/fs-test--temp-dir "subdir") t)
  (with-temp-buffer (insert "Test content") (write-file (f-join cj/fs-test--temp-dir "test-file.txt")))
  (make-directory (f-join cj/fs-test--temp-dir "subdir") t)
  (with-temp-buffer (insert "Nested test") (write-file (f-join cj/fs-test--temp-dir "subdir/nested-file.txt"))))

(defun cj/fs-test--teardown ()
  "Clean up temporary directory for fs-get-file-info tests."
  (when (and cj/fs-test--temp-dir (file-directory-p cj/fs-test--temp-dir))
	(delete-directory cj/fs-test--temp-dir t))
  (setq cj/fs-test--temp-dir nil))

(ert-deftest test-cj/fs-get-file-info-normal-regular-file ()
  "Normal: info for regular file."
  (cj/fs-test--setup)
  (unwind-protect
	  (let ((info (cj/fs-get-file-info (f-join cj/fs-test--temp-dir "test-file.txt"))))
		(should (plist-get info :success))
		(should (string-suffix-p "test-file.txt" (plist-get info :path)))
		(should (not (plist-get info :directory))))
	(cj/fs-test--teardown)))

(ert-deftest test-cj/fs-get-file-info-normal-directory ()
  "Normal: info for directory."
  (cj/fs-test--setup)
  (unwind-protect
	  (let ((info (cj/fs-get-file-info (f-join cj/fs-test--temp-dir "subdir"))))
		(should (plist-get info :success))
		(should (string-suffix-p "subdir" (plist-get info :path)))
		(should (plist-get info :directory)))
	(cj/fs-test--teardown)))

(ert-deftest test-cj/fs-get-file-info-error-nonexistent ()
  "Error: non-existent file returns :success nil plist."
  (let ((info (cj/fs-get-file-info "/tmp/nonexistent-file-1234567890")))
	(should (not (plist-get info :success)))
	(should (stringp (plist-get info :error)))))

(ert-deftest test-cj/fs-get-file-info-error-permission-denied ()
  "Error: permission denied file returns :success nil plist."
  (cj/fs-test--setup)
  (let ((file (f-join cj/fs-test--temp-dir "protected-file")))
	(unwind-protect
		(progn
		  (with-temp-buffer (insert "secret") (write-file file))
		  (set-file-modes file #o000)
		  (let ((info (cj/fs-get-file-info file)))
			(should (not (plist-get info :success)))
			(should (stringp (plist-get info :error)))))
	  (set-file-modes file #o644)
	  (delete-file file)
	  (cj/fs-test--teardown))))

(provide 'test-tool-library-fs-get-file-info)
;;; test-tool-library-fs-get-file-info.el ends here
