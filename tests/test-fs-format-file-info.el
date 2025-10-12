;;; test-tool-library-fs-format-file-info.el --- ERT tests for cj/fs-format-file-info -*- lexical-binding: t; -*-

;; Author: gptel-tool-writer and cjennings
;; Keywords: tests, filesystem, tools

;;; Commentary:
;; ERT tests for the cj/fs-format-file-info function from tool-filesystem-library.el.
;; Place this file in ~/.emacs.d/tests/ and load it to run tests.

;;; Code:

(require 'ert)
(require 'f)
(require 'tool-filesystem-library)

(ert-deftest test-cj/fs-format-file-info-normal-typical ()
  "Normal: format typical file info plist."
  (let ((info (list :permissions "-rw-r--r--"
                    :executable nil
                    :size 1024
                    :last-modified (current-time)
                    :path "~/test-file.txt")))
	(should (string-match-p "test-file.txt" (cj/fs-format-file-info info "~")))))

(ert-deftest test-cj/fs-format-file-info-error-missing-keys ()
  "Error: format with missing keys handled."
  (let ((info (list)))
	(should (cj/fs-format-file-info info "~"))))

(ert-deftest test-cj/fs-format-file-info-boundary-zero-size ()
  "Boundary: format with zero size."
  (let ((info (list :permissions "-rw-r--r--"
					:executable nil
					:size 0
					:last-modified (current-time)
					:path "~/empty-file.txt")))
	(should (string-match-p "empty-file.txt" (cj/fs-format-file-info info "~")))))

(provide 'test-tool-library-fs-format-file-info)
;;; test-tool-library-fs-format-file-info.el ends here
