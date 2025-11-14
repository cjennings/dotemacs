;;; test-music-config--get-m3u-basenames.el --- Tests for M3U basename extraction -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--get-m3u-basenames function.
;; Tests the helper that extracts M3U basenames (without .m3u extension).
;;
;; Test organization:
;; - Normal Cases: Multiple files, single file
;; - Boundary Cases: Empty directory, extension removal
;; - Error Cases: Nonexistent directory
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

(defun test-music-config--get-m3u-basenames-setup ()
  "Setup test environment."
  (cj/create-test-base-dir))

(defun test-music-config--get-m3u-basenames-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--get-m3u-basenames-normal-multiple-files-returns-basenames ()
  "Extract basenames from multiple M3U files without .m3u extension."
  (test-music-config--get-m3u-basenames-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "playlists"))
             (file1 (cj/create-temp-test-file-with-content "" "rock.m3u"))
             (file2 (cj/create-temp-test-file-with-content "" "jazz.m3u"))
             (file3 (cj/create-temp-test-file-with-content "" "classical.m3u")))
        (rename-file file1 (expand-file-name "rock.m3u" test-dir))
        (rename-file file2 (expand-file-name "jazz.m3u" test-dir))
        (rename-file file3 (expand-file-name "classical.m3u" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-basenames)))
            (should (= (length result) 3))
            ;; Sort for consistent comparison
            (let ((sorted-result (sort result #'string<)))
              (should (equal sorted-result '("classical" "jazz" "rock")))))))
    (test-music-config--get-m3u-basenames-teardown)))

(ert-deftest test-music-config--get-m3u-basenames-normal-single-file-returns-basename ()
  "Extract basename from single M3U file without .m3u extension."
  (test-music-config--get-m3u-basenames-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "playlists"))
             (file1 (cj/create-temp-test-file-with-content "" "favorites.m3u")))
        (rename-file file1 (expand-file-name "favorites.m3u" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-basenames)))
            (should (= (length result) 1))
            (should (equal (car result) "favorites")))))
    (test-music-config--get-m3u-basenames-teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--get-m3u-basenames-boundary-empty-directory-returns-empty ()
  "Extract basenames from empty directory returns empty list."
  (test-music-config--get-m3u-basenames-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "empty-playlists")))
        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-basenames)))
            (should (null result)))))
    (test-music-config--get-m3u-basenames-teardown)))

(ert-deftest test-music-config--get-m3u-basenames-boundary-extension-removed ()
  "Basenames have .m3u extension removed."
  (test-music-config--get-m3u-basenames-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "playlists"))
             (file1 (cj/create-temp-test-file-with-content "" "test.m3u")))
        (rename-file file1 (expand-file-name "playlist.m3u" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-basenames)))
            (should (equal result '("playlist")))
            ;; Verify no .m3u extension present
            (should-not (string-match-p "\\.m3u" (car result))))))
    (test-music-config--get-m3u-basenames-teardown)))

(ert-deftest test-music-config--get-m3u-basenames-boundary-spaces-in-filename-preserved ()
  "Basenames with spaces preserve the spaces."
  (test-music-config--get-m3u-basenames-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "playlists"))
             (file1 (cj/create-temp-test-file-with-content "" "test.m3u")))
        (rename-file file1 (expand-file-name "My Favorite Songs.m3u" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-basenames)))
            (should (equal result '("My Favorite Songs"))))))
    (test-music-config--get-m3u-basenames-teardown)))

;;; Error Cases

(ert-deftest test-music-config--get-m3u-basenames-error-nonexistent-directory-signals-error ()
  "Nonexistent directory signals error."
  (let ((cj/music-m3u-root "/nonexistent/directory/path"))
    (should-error (cj/music--get-m3u-basenames)
                  :type 'file-error)))

(provide 'test-music-config--get-m3u-basenames)
;;; test-music-config--get-m3u-basenames.el ends here
