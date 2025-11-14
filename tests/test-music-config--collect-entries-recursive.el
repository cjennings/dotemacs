;;; test-music-config--collect-entries-recursive.el --- Tests for recursive music collection -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--collect-entries-recursive function.
;; Tests the recursive helper that collects music files and directories.
;;
;; Test organization:
;; - Normal Cases: Single level, nested directories, mixed files
;; - Boundary Cases: Hidden files/dirs, non-music files, empty dirs, sorting
;; - Error Cases: Empty root, nonexistent root
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

(defun test-music-config--collect-entries-recursive-setup ()
  "Setup test environment."
  (cj/create-test-base-dir))

(defun test-music-config--collect-entries-recursive-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--collect-entries-recursive-normal-single-level-files-and-dirs ()
  "Collect music files and subdirectories at single level."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Create files at root
        (cj/create-directory-or-file-ensuring-parents "music/song1.mp3" "")
        (cj/create-directory-or-file-ensuring-parents "music/song2.flac" "")
        ;; Create subdirectories
        (cj/create-directory-or-file-ensuring-parents "music/artist1/" "")
        (cj/create-directory-or-file-ensuring-parents "music/artist2/" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (member "artist1/" result))
          (should (member "artist2/" result))
          (should (member "song1.mp3" result))
          (should (member "song2.flac" result))
          (should (= (length result) 4))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-normal-nested-directories ()
  "Collect nested directories multiple levels deep."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Create nested structure
        (cj/create-directory-or-file-ensuring-parents "music/artist/" "")
        (cj/create-directory-or-file-ensuring-parents "music/artist/album/" "")
        (cj/create-directory-or-file-ensuring-parents "music/artist/album/disc1/" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (member "artist/" result))
          (should (member "artist/album/" result))
          (should (member "artist/album/disc1/" result))
          (should (= (length result) 3))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-normal-mixed-files-at-multiple-levels ()
  "Collect music files at root, subdirs, and nested subdirs."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Root level file
        (cj/create-directory-or-file-ensuring-parents "music/root-track.mp3" "")
        ;; Subdir with file
        (cj/create-directory-or-file-ensuring-parents "music/artist/" "")
        (cj/create-directory-or-file-ensuring-parents "music/artist/track1.mp3" "")
        ;; Nested subdir with file
        (cj/create-directory-or-file-ensuring-parents "music/artist/album/" "")
        (cj/create-directory-or-file-ensuring-parents "music/artist/album/track2.mp3" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (member "root-track.mp3" result))
          (should (member "artist/" result))
          (should (member "artist/track1.mp3" result))
          (should (member "artist/album/" result))
          (should (member "artist/album/track2.mp3" result))
          (should (= (length result) 5))))
    (test-music-config--collect-entries-recursive-teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--collect-entries-recursive-boundary-hidden-directories-skipped ()
  "Hidden directories and their contents are excluded."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Visible file
        (cj/create-directory-or-file-ensuring-parents "music/visible.mp3" "")
        ;; Hidden directory with music file
        (cj/create-directory-or-file-ensuring-parents "music/.hidden/" "")
        (cj/create-directory-or-file-ensuring-parents "music/.hidden/secret.mp3" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (member "visible.mp3" result))
          (should-not (member ".hidden/" result))
          (should-not (member ".hidden/secret.mp3" result))
          (should (= (length result) 1))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-boundary-hidden-files-skipped ()
  "Hidden files at root are excluded."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Visible file
        (cj/create-directory-or-file-ensuring-parents "music/visible.mp3" "")
        ;; Hidden file (note: directory-files regex "^[^.].*" should skip it)
        (cj/create-directory-or-file-ensuring-parents "music/.hidden-track.mp3" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (member "visible.mp3" result))
          (should-not (member ".hidden-track.mp3" result))
          (should (= (length result) 1))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-boundary-non-music-files-excluded ()
  "Non-music files are excluded."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Music file
        (cj/create-directory-or-file-ensuring-parents "music/song.mp3" "")
        ;; Non-music files
        (cj/create-directory-or-file-ensuring-parents "music/readme.txt" "")
        (cj/create-directory-or-file-ensuring-parents "music/cover.jpg" "")
        (cj/create-directory-or-file-ensuring-parents "music/info.pdf" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (member "song.mp3" result))
          (should-not (member "readme.txt" result))
          (should-not (member "cover.jpg" result))
          (should-not (member "info.pdf" result))
          (should (= (length result) 1))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-boundary-empty-directories-included ()
  "Empty subdirectories are still listed with trailing slash."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Empty subdirectories
        (cj/create-directory-or-file-ensuring-parents "music/empty-artist/" "")
        (cj/create-directory-or-file-ensuring-parents "music/another-empty/" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (member "empty-artist/" result))
          (should (member "another-empty/" result))
          (should (= (length result) 2))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-boundary-sorted-output ()
  "Output is sorted alphabetically (case-insensitive)."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Create files in non-alphabetical order
        (cj/create-directory-or-file-ensuring-parents "music/zebra.mp3" "")
        (cj/create-directory-or-file-ensuring-parents "music/Alpha.mp3" "")
        (cj/create-directory-or-file-ensuring-parents "music/beta.mp3" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          ;; Should be sorted alphabetically (case-insensitive)
          (should (equal result '("Alpha.mp3" "beta.mp3" "zebra.mp3")))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-boundary-directories-have-trailing-slash ()
  "Directories have trailing slash, files don't."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        (cj/create-directory-or-file-ensuring-parents "music/artist/" "")
        (cj/create-directory-or-file-ensuring-parents "music/song.mp3" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          ;; Directory has trailing slash
          (should (cl-some (lambda (entry) (string-suffix-p "/" entry)) result))
          ;; File doesn't have trailing slash
          (should (cl-some (lambda (entry) (not (string-suffix-p "/" entry))) result))
          ;; Specifically check
          (should (member "artist/" result))
          (should (member "song.mp3" result))
          (should-not (member "song.mp3/" result))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-boundary-all-music-extensions ()
  "All configured music extensions are collected."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "music")))
        ;; Create file for each extension: aac, flac, m4a, mp3, ogg, opus, wav
        (cj/create-directory-or-file-ensuring-parents "music/track.aac" "")
        (cj/create-directory-or-file-ensuring-parents "music/track.flac" "")
        (cj/create-directory-or-file-ensuring-parents "music/track.m4a" "")
        (cj/create-directory-or-file-ensuring-parents "music/track.mp3" "")
        (cj/create-directory-or-file-ensuring-parents "music/track.ogg" "")
        (cj/create-directory-or-file-ensuring-parents "music/track.opus" "")
        (cj/create-directory-or-file-ensuring-parents "music/track.wav" "")

        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (= (length result) 7))
          (should (member "track.aac" result))
          (should (member "track.flac" result))
          (should (member "track.m4a" result))
          (should (member "track.mp3" result))
          (should (member "track.ogg" result))
          (should (member "track.opus" result))
          (should (member "track.wav" result))))
    (test-music-config--collect-entries-recursive-teardown)))

;;; Error Cases

(ert-deftest test-music-config--collect-entries-recursive-error-empty-root-returns-empty ()
  "Empty root directory returns empty list."
  (test-music-config--collect-entries-recursive-setup)
  (unwind-protect
      (let* ((root-dir (cj/create-test-subdirectory "empty-music")))
        (let ((result (cj/music--collect-entries-recursive root-dir)))
          (should (null result))))
    (test-music-config--collect-entries-recursive-teardown)))

(ert-deftest test-music-config--collect-entries-recursive-error-nonexistent-root-returns-empty ()
  "Nonexistent directory returns empty list."
  (let ((result (cj/music--collect-entries-recursive "/nonexistent/path/to/music")))
    (should (null result))))

(provide 'test-music-config--collect-entries-recursive)
;;; test-music-config--collect-entries-recursive.el ends here
