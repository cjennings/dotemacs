;;; test-tool-library-fs--mode-to-permissions.el --- ERT tests for cj/fs--mode-to-permissions -*- lexical-binding: t; -*-

;; Author: gptel-tool-writer and cjennings
;; Keywords: tests, filesystem, tools

;;; Commentary:
;; ERT tests for the cj/fs--mode-to-permissions function from tool-filesystem-library.el.
;; Place this file in ~/.emacs.d/tests/ and load it to run tests.

;;; Code:

(require 'ert)
(require 'tool-filesystem-library)

(ert-deftest test-cj/fs--mode-to-permissions-normal-directory ()
  "Normal: directory permissions string."
  (should (string-prefix-p "d"
                           (cj/fs--mode-to-permissions #o40755))))

(ert-deftest test-cj/fs--mode-to-permissions-normal-regular-file ()
  "Normal: regular file permissions string."
  (should (string-prefix-p "-"
                           (cj/fs--mode-to-permissions #o100644))))

(ert-deftest test-cj/fs--mode-to-permissions-boundary-zero ()
  "Boundary: no permissions."
  (should (string= "----------"
				   (cj/fs--mode-to-permissions 0))))

(ert-deftest test-cj/fs--mode-to-permissions-boundary-full ()
  "Boundary: full permissions string."
  (should (string= "-rwxrwxrwx"
				   (cj/fs--mode-to-permissions #o777))))

(provide 'test-tool-library-fs--mode-to-permissions)
;;; test-tool-library-fs--mode-to-permissions.el ends here
