;;; test-music-config--valid-file-p.el --- Tests for music file validation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--valid-file-p function.
;; Tests the pure, deterministic helper that validates music file extensions.
;;
;; Test organization:
;; - Normal Cases: Valid music extensions (case-insensitive)
;; - Boundary Cases: Edge conditions (no extension, dots in path, empty strings)
;; - Error Cases: Invalid extensions, nil input
;;
;;; Code:

(require 'ert)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Load production code
(require 'music-config)

;;; Normal Cases

(ert-deftest test-music-config--valid-file-p-normal-mp3-extension-returns-true ()
  "Validate mp3 file extension returns non-nil."
  (should (cj/music--valid-file-p "/path/to/song.mp3")))

(ert-deftest test-music-config--valid-file-p-normal-flac-extension-returns-true ()
  "Validate flac file extension returns non-nil."
  (should (cj/music--valid-file-p "/path/to/song.flac")))

(ert-deftest test-music-config--valid-file-p-normal-all-extensions-return-true ()
  "Validate all configured music extensions return non-nil."
  ;; Test each extension from cj/music-file-extensions
  (dolist (ext '("aac" "flac" "m4a" "mp3" "ogg" "opus" "wav"))
    (should (cj/music--valid-file-p (format "/path/to/song.%s" ext)))))

(ert-deftest test-music-config--valid-file-p-normal-uppercase-extension-returns-true ()
  "Validate uppercase extension returns non-nil (case-insensitive)."
  (should (cj/music--valid-file-p "/path/to/song.MP3")))

(ert-deftest test-music-config--valid-file-p-normal-mixed-case-extension-returns-true ()
  "Validate mixed-case extension returns non-nil (case-insensitive)."
  (should (cj/music--valid-file-p "/path/to/song.Mp3"))
  (should (cj/music--valid-file-p "/path/to/song.FLaC")))

;;; Boundary Cases

(ert-deftest test-music-config--valid-file-p-boundary-dots-in-path-returns-true ()
  "Validate file with dots in directory path uses only last extension."
  (should (cj/music--valid-file-p "/path/with.dots/in.directory/song.mp3")))

(ert-deftest test-music-config--valid-file-p-boundary-multiple-extensions-uses-last ()
  "Validate file with multiple extensions uses rightmost extension."
  (should (cj/music--valid-file-p "/path/to/song.backup.mp3"))
  (should (cj/music--valid-file-p "/path/to/song.old.flac")))

(ert-deftest test-music-config--valid-file-p-boundary-just-filename-with-extension-returns-true ()
  "Validate bare filename without path returns non-nil."
  (should (cj/music--valid-file-p "song.mp3")))

(ert-deftest test-music-config--valid-file-p-boundary-no-extension-returns-nil ()
  "Validate file without extension returns nil."
  (should-not (cj/music--valid-file-p "/path/to/song")))

(ert-deftest test-music-config--valid-file-p-boundary-dot-at-end-returns-nil ()
  "Validate file ending with dot (empty extension) returns nil."
  (should-not (cj/music--valid-file-p "/path/to/song.")))

(ert-deftest test-music-config--valid-file-p-boundary-empty-string-returns-nil ()
  "Validate empty string returns nil."
  (should-not (cj/music--valid-file-p "")))

;;; Error Cases

(ert-deftest test-music-config--valid-file-p-error-nil-input-returns-nil ()
  "Validate nil input returns nil gracefully."
  (should-not (cj/music--valid-file-p nil)))

(ert-deftest test-music-config--valid-file-p-error-non-music-extension-returns-nil ()
  "Validate non-music file extension returns nil."
  (should-not (cj/music--valid-file-p "/path/to/document.txt"))
  (should-not (cj/music--valid-file-p "/path/to/readme.md")))

(ert-deftest test-music-config--valid-file-p-error-image-extension-returns-nil ()
  "Validate image file extension returns nil."
  (should-not (cj/music--valid-file-p "/path/to/cover.jpg"))
  (should-not (cj/music--valid-file-p "/path/to/artwork.png")))

(ert-deftest test-music-config--valid-file-p-error-video-extension-returns-nil ()
  "Validate video file extension returns nil (mp4 not in list, only m4a)."
  (should-not (cj/music--valid-file-p "/path/to/video.mp4"))
  (should-not (cj/music--valid-file-p "/path/to/clip.mkv")))

(provide 'test-music-config--valid-file-p)
;;; test-music-config--valid-file-p.el ends here
