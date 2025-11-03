;;; test-music-config--get-m3u-files.el --- Tests for M3U file discovery -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--get-m3u-files function.
;; Tests the helper that discovers M3U files in the music directory.
;;
;; Test organization:
;; - Normal Cases: Multiple M3U files, single file
;; - Boundary Cases: Empty directory, non-M3U files, various filenames
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

(defun test-music-config--get-m3u-files-setup ()
  "Setup test environment."
  (cj/create-test-base-dir))

(defun test-music-config--get-m3u-files-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--get-m3u-files-normal-multiple-files-returns-list ()
  "Discover multiple M3U files returns list of (basename . fullpath) conses."
  (test-music-config--get-m3u-files-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "playlists"))
             (file1 (cj/create-temp-test-file-with-content "" "playlist1.m3u"))
             (file2 (cj/create-temp-test-file-with-content "" "playlist2.m3u"))
             (file3 (cj/create-temp-test-file-with-content "" "playlist3.m3u")))
        ;; Move files to test-dir
        (rename-file file1 (expand-file-name "playlist1.m3u" test-dir))
        (rename-file file2 (expand-file-name "playlist2.m3u" test-dir))
        (rename-file file3 (expand-file-name "playlist3.m3u" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-files)))
            (should (= (length result) 3))
            ;; Check structure: list of (basename . fullpath) conses
            ;; Sort for consistent comparison (directory-files order is filesystem-dependent)
            (let ((basenames (sort (mapcar #'car result) #'string<))
                  (fullpaths (sort (mapcar #'cdr result) #'string<)))
              (should (equal basenames '("playlist1.m3u" "playlist2.m3u" "playlist3.m3u")))
              (should (equal fullpaths
                            (list (expand-file-name "playlist1.m3u" test-dir)
                                  (expand-file-name "playlist2.m3u" test-dir)
                                  (expand-file-name "playlist3.m3u" test-dir))))))))
    (test-music-config--get-m3u-files-teardown)))

(ert-deftest test-music-config--get-m3u-files-normal-single-file-returns-list ()
  "Discover single M3U file returns single-item list."
  (test-music-config--get-m3u-files-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "playlists"))
             (file1 (cj/create-temp-test-file-with-content "" "myplaylist.m3u")))
        (rename-file file1 (expand-file-name "myplaylist.m3u" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-files)))
            (should (= (length result) 1))
            (should (equal (caar result) "myplaylist.m3u"))
            (should (equal (cdar result) (expand-file-name "myplaylist.m3u" test-dir))))))
    (test-music-config--get-m3u-files-teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--get-m3u-files-boundary-empty-directory-returns-empty ()
  "Discover M3U files in empty directory returns empty list."
  (test-music-config--get-m3u-files-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "empty-playlists")))
        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-files)))
            (should (null result)))))
    (test-music-config--get-m3u-files-teardown)))

(ert-deftest test-music-config--get-m3u-files-boundary-non-m3u-files-ignored ()
  "Directory with non-M3U files returns empty list."
  (test-music-config--get-m3u-files-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "mixed-files"))
             (txt-file (cj/create-temp-test-file-with-content "" "readme.txt"))
             (mp3-file (cj/create-temp-test-file-with-content "" "song.mp3"))
             (json-file (cj/create-temp-test-file-with-content "" "data.json")))
        (rename-file txt-file (expand-file-name "readme.txt" test-dir))
        (rename-file mp3-file (expand-file-name "song.mp3" test-dir))
        (rename-file json-file (expand-file-name "data.json" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-files)))
            (should (null result)))))
    (test-music-config--get-m3u-files-teardown)))

(ert-deftest test-music-config--get-m3u-files-boundary-m3u-with-spaces-included ()
  "M3U files with spaces in name are discovered."
  (test-music-config--get-m3u-files-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "playlists"))
             (file1 (cj/create-temp-test-file-with-content "" "my-playlist.m3u")))
        (rename-file file1 (expand-file-name "My Favorite Songs.m3u" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-files)))
            (should (= (length result) 1))
            (should (equal (caar result) "My Favorite Songs.m3u")))))
    (test-music-config--get-m3u-files-teardown)))

(ert-deftest test-music-config--get-m3u-files-boundary-mixed-m3u-and-other-files ()
  "Directory with both M3U and non-M3U files returns only M3U files."
  (test-music-config--get-m3u-files-setup)
  (unwind-protect
      (let* ((test-dir (cj/create-test-subdirectory "mixed"))
             (m3u-file (cj/create-temp-test-file-with-content "" "playlist.m3u"))
             (txt-file (cj/create-temp-test-file-with-content "" "readme.txt"))
             (mp3-file (cj/create-temp-test-file-with-content "" "song.mp3")))
        (rename-file m3u-file (expand-file-name "playlist.m3u" test-dir))
        (rename-file txt-file (expand-file-name "readme.txt" test-dir))
        (rename-file mp3-file (expand-file-name "song.mp3" test-dir))

        (let ((cj/music-m3u-root test-dir))
          (let ((result (cj/music--get-m3u-files)))
            (should (= (length result) 1))
            (should (equal (caar result) "playlist.m3u")))))
    (test-music-config--get-m3u-files-teardown)))

;;; Error Cases

(ert-deftest test-music-config--get-m3u-files-error-nonexistent-directory-signals-error ()
  "Nonexistent directory signals error."
  (let ((cj/music-m3u-root "/nonexistent/directory/path"))
    (should-error (cj/music--get-m3u-files)
                  :type 'file-error)))

(provide 'test-music-config--get-m3u-files)
;;; test-music-config--get-m3u-files.el ends here
