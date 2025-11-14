;;; test-music-config--safe-filename.el --- Tests for filename sanitization -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--safe-filename function.
;; Tests the pure helper that sanitizes filenames by replacing invalid chars.
;;
;; Test organization:
;; - Normal Cases: Valid filenames unchanged, spaces replaced
;; - Boundary Cases: Special chars, unicode, slashes, consecutive invalid chars
;; - Error Cases: Nil input
;;
;;; Code:

(require 'ert)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Load production code
(require 'music-config)

;;; Normal Cases

(ert-deftest test-music-config--safe-filename-normal-alphanumeric-unchanged ()
  "Validate alphanumeric filename remains unchanged."
  (should (string= (cj/music--safe-filename "MyPlaylist123")
                   "MyPlaylist123")))

(ert-deftest test-music-config--safe-filename-normal-with-hyphens-unchanged ()
  "Validate filename with hyphens remains unchanged."
  (should (string= (cj/music--safe-filename "my-playlist-name")
                   "my-playlist-name")))

(ert-deftest test-music-config--safe-filename-normal-with-underscores-unchanged ()
  "Validate filename with underscores remains unchanged."
  (should (string= (cj/music--safe-filename "my_playlist_name")
                   "my_playlist_name")))

(ert-deftest test-music-config--safe-filename-normal-spaces-replaced ()
  "Validate spaces are replaced with underscores."
  (should (string= (cj/music--safe-filename "My Favorite Songs")
                   "My_Favorite_Songs")))

;;; Boundary Cases

(ert-deftest test-music-config--safe-filename-boundary-special-chars-replaced ()
  "Validate special characters are replaced with underscores."
  (should (string= (cj/music--safe-filename "playlist@#$%^&*()")
                   "playlist_________")))

(ert-deftest test-music-config--safe-filename-boundary-unicode-replaced ()
  "Validate unicode characters are replaced with underscores."
  (should (string= (cj/music--safe-filename "中文歌曲")
                   "____")))

(ert-deftest test-music-config--safe-filename-boundary-mixed-valid-invalid ()
  "Validate mixed valid and invalid characters."
  (should (string= (cj/music--safe-filename "Rock & Roll")
                   "Rock___Roll")))

(ert-deftest test-music-config--safe-filename-boundary-dots-replaced ()
  "Validate dots are replaced with underscores."
  (should (string= (cj/music--safe-filename "my.playlist.name")
                   "my_playlist_name")))

(ert-deftest test-music-config--safe-filename-boundary-slashes-replaced ()
  "Validate slashes are replaced with underscores."
  (should (string= (cj/music--safe-filename "folder/file")
                   "folder_file")))

(ert-deftest test-music-config--safe-filename-boundary-consecutive-invalid-chars ()
  "Validate consecutive invalid characters each become underscores."
  (should (string= (cj/music--safe-filename "test!!!name")
                   "test___name")))

(ert-deftest test-music-config--safe-filename-boundary-empty-string-unchanged ()
  "Validate empty string remains unchanged."
  (should (string= (cj/music--safe-filename "")
                   "")))

(ert-deftest test-music-config--safe-filename-boundary-only-invalid-chars ()
  "Validate string with only invalid characters becomes all underscores."
  (should (string= (cj/music--safe-filename "!@#$%")
                   "_____")))

;;; Error Cases

(ert-deftest test-music-config--safe-filename-error-nil-input-signals-error ()
  "Validate nil input signals error."
  (should-error (cj/music--safe-filename nil)
                :type 'wrong-type-argument))

(provide 'test-music-config--safe-filename)
;;; test-music-config--safe-filename.el ends here
