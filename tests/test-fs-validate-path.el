;;; test-tool-library-cj/fs-validate-path.el --- ERT tests for cj/fs-validate-path -*- lexical-binding: t; -*-

;; Author: gptel-tool-writer and cjennings
;; Keywords: tests, filesystem, tools

;;; Commentary:
;; ERT tests for the cj/fs-validate-path function from tool-filesystem-library.el.
;; Place this file in ~/.emacs.d/tests/ and load it to run tests.

;;; Code:

(require 'ert)
(require 'f)
(require 'tool-filesystem-library)

(ert-deftest test-cj/fs-validate-path-normal-home ()
  "Normal: validate home directory path."
  (should (string-prefix-p (expand-file-name "~")
						   (cj/fs-validate-path "~"))))

(ert-deftest test-cj/fs-validate-path-normal-temp ()
  "Normal: validate temp directory path."
  (let ((temp (expand-file-name temporary-file-directory)))
	(should (string-prefix-p temp (cj/fs-validate-path temp)))))

(ert-deftest test-cj/fs-validate-path-error-outside ()
  "Error: path outside allowed directories."
  (should-error (cj/fs-validate-path "/etc/passwd")))

(ert-deftest test-cj/fs-validate-path-error-nonexistent ()
  "Error: non-existent path."
  (should-error (cj/fs-validate-path (format "/tmp/nonexistent-%d" (random 100000)))))

(ert-deftest test-cj/fs-validate-path-error-unreadable ()
  "Error: unreadable path."
  (let ((file (make-temp-file "test-unreadable")))
	(unwind-protect
		(progn
		  (set-file-modes file 0)
		  (should-error (cj/fs-validate-path file)))
	  (set-file-modes file #o644)
	  (delete-file file))))

(provide 'test-tool-library-cj/fs-validate-path)
;;; test-tool-library-cj/fs-validate-path.el ends here
