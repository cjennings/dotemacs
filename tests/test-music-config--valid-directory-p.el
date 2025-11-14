;;; test-music-config--valid-directory-p.el --- Tests for directory validation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--valid-directory-p function.
;; Tests the pure helper that validates non-hidden directories.
;;
;; Test organization:
;; - Normal Cases: Valid visible directories
;; - Boundary Cases: Trailing slashes, dots in names, hidden directories
;; - Error Cases: Files (not dirs), nonexistent paths, nil input
;;
;;; Code:

(require 'ert)
(require 'testutil-general)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Load production code
(require 'music-config)

;;; Setup & Teardown

(defun test-music-config--valid-directory-p-setup ()
  "Setup test environment."
  (cj/create-test-base-dir))

(defun test-music-config--valid-directory-p-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--valid-directory-p-normal-visible-directory-returns-true ()
  "Validate visible directory returns non-nil."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory "testdir")))
        (should (cj/music--valid-directory-p test-dir)))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-normal-nested-directory-returns-true ()
  "Validate nested visible directory returns non-nil."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory "testdir/subdir/nested")))
        (should (cj/music--valid-directory-p test-dir)))
    (test-music-config--valid-directory-p-teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--valid-directory-p-boundary-trailing-slash-returns-true ()
  "Validate directory with trailing slash returns non-nil."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory "testdir")))
        (should (cj/music--valid-directory-p (file-name-as-directory test-dir))))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-boundary-no-trailing-slash-returns-true ()
  "Validate directory without trailing slash returns non-nil."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory "testdir")))
        (should (cj/music--valid-directory-p (directory-file-name test-dir))))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-boundary-dot-in-middle-returns-true ()
  "Validate directory with dot in middle of name returns non-nil."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory "my.music.dir")))
        (should (cj/music--valid-directory-p test-dir)))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-boundary-hidden-directory-returns-nil ()
  "Validate hidden directory (starting with dot) returns nil."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory ".hidden")))
        (should-not (cj/music--valid-directory-p test-dir)))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-boundary-current-dir-dot-returns-nil ()
  "Validate current directory '.' returns nil (hidden)."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory "testdir")))
        ;; Change to test dir and check "."
        (let ((default-directory test-dir))
          (should-not (cj/music--valid-directory-p "."))))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-boundary-parent-dir-dotdot-returns-nil ()
  "Validate parent directory '..' returns nil (hidden)."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-dir (cj/create-test-subdirectory "testdir/subdir")))
        ;; Change to subdir and check ".."
        (let ((default-directory test-dir))
          (should-not (cj/music--valid-directory-p ".."))))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-boundary-hidden-subdir-basename-check ()
  "Validate hidden subdirectory returns nil based on basename."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((hidden-dir (cj/create-test-subdirectory "visible/.hidden")))
        (should-not (cj/music--valid-directory-p hidden-dir)))
    (test-music-config--valid-directory-p-teardown)))

;;; Error Cases

(ert-deftest test-music-config--valid-directory-p-error-regular-file-returns-nil ()
  "Validate regular file (not directory) returns nil."
  (test-music-config--valid-directory-p-setup)
  (unwind-protect
      (let ((test-file (cj/create-temp-test-file "testfile-")))
        (should-not (cj/music--valid-directory-p test-file)))
    (test-music-config--valid-directory-p-teardown)))

(ert-deftest test-music-config--valid-directory-p-error-nonexistent-path-returns-nil ()
  "Validate nonexistent path returns nil."
  (should-not (cj/music--valid-directory-p "/nonexistent/path/to/directory")))

(ert-deftest test-music-config--valid-directory-p-error-nil-input-returns-nil ()
  "Validate nil input returns nil gracefully."
  (should-not (cj/music--valid-directory-p nil)))

(ert-deftest test-music-config--valid-directory-p-error-empty-string-returns-nil ()
  "Validate empty string returns nil."
  (should-not (cj/music--valid-directory-p "")))

(provide 'test-music-config--valid-directory-p)
;;; test-music-config--valid-directory-p.el ends here
