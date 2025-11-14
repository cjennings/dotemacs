;;; test-music-config--m3u-file-tracks.el --- Tests for M3U file parsing -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--m3u-file-tracks function.
;; Tests the M3U parser that extracts track paths from playlist files.
;;
;; Test organization:
;; - Normal Cases: Absolute paths, relative paths, URLs (http/https/mms)
;; - Boundary Cases: Empty lines, whitespace, comments, order preservation
;; - Error Cases: Nonexistent files, nil input
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

(defun test-music-config--m3u-file-tracks-setup ()
  "Setup test environment."
  (cj/create-test-base-dir))

(defun test-music-config--m3u-file-tracks-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--m3u-file-tracks-normal-absolute-paths-returns-list ()
  "Parse M3U with absolute paths returns list in order."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "/home/user/music/track1.mp3\n/home/user/music/track2.mp3\n/home/user/music/track3.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("/home/user/music/track1.mp3"
                               "/home/user/music/track2.mp3"
                               "/home/user/music/track3.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-normal-relative-paths-expanded ()
  "Parse M3U with relative paths expands them relative to M3U directory."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "artist/track1.mp3\nartist/track2.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (m3u-dir (file-name-directory m3u-file))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks (list (expand-file-name "artist/track1.mp3" m3u-dir)
                                    (expand-file-name "artist/track2.mp3" m3u-dir)))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-normal-http-urls-preserved ()
  "Parse M3U with http:// URLs preserves them as-is."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "http://example.com/stream1.mp3\nhttp://example.com/stream2.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("http://example.com/stream1.mp3"
                               "http://example.com/stream2.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-normal-https-urls-preserved ()
  "Parse M3U with https:// URLs preserves them as-is."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "https://secure.example.com/stream.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("https://secure.example.com/stream.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-normal-mms-urls-preserved ()
  "Parse M3U with mms:// URLs preserves them as-is."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "mms://radio.example.com/stream\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("mms://radio.example.com/stream"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-normal-mixed-paths-and-urls ()
  "Parse M3U with mix of absolute, relative, and URLs handles all correctly."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "/home/user/music/local.mp3\nartist/relative.mp3\nhttp://example.com/stream.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (m3u-dir (file-name-directory m3u-file))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks (list "/home/user/music/local.mp3"
                                    (expand-file-name "artist/relative.mp3" m3u-dir)
                                    "http://example.com/stream.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--m3u-file-tracks-boundary-empty-lines-ignored ()
  "Parse M3U with empty lines ignores them and returns tracks."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "/home/user/music/track1.mp3\n\n/home/user/music/track2.mp3\n\n\n/home/user/music/track3.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("/home/user/music/track1.mp3"
                               "/home/user/music/track2.mp3"
                               "/home/user/music/track3.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-boundary-whitespace-only-lines-ignored ()
  "Parse M3U with whitespace-only lines ignores them."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "/home/user/music/track1.mp3\n   \n\t\t\n/home/user/music/track2.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("/home/user/music/track1.mp3"
                               "/home/user/music/track2.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-boundary-comments-ignored ()
  "Parse M3U with comment lines ignores them, returns only tracks."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "#EXTM3U\n#EXTINF:-1,Track Title\n/home/user/music/track.mp3\n#Another comment\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("/home/user/music/track.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-boundary-leading-trailing-whitespace-trimmed ()
  "Parse M3U with whitespace around paths trims it."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "  /home/user/music/track1.mp3  \n\t/home/user/music/track2.mp3\t\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("/home/user/music/track1.mp3"
                               "/home/user/music/track2.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-boundary-empty-file-returns-nil ()
  "Parse empty M3U file returns nil."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (null tracks)))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-boundary-only-comments-returns-empty ()
  "Parse M3U with only comments returns empty list."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "#EXTM3U\n#EXTINF:-1,Title\n#Another comment\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (null tracks)))
    (test-music-config--m3u-file-tracks-teardown)))

(ert-deftest test-music-config--m3u-file-tracks-boundary-preserves-order ()
  "Parse M3U preserves track order (tests nreverse)."
  (test-music-config--m3u-file-tracks-setup)
  (unwind-protect
      (let* ((content "/track1.mp3\n/track2.mp3\n/track3.mp3\n/track4.mp3\n/track5.mp3\n")
             (m3u-file (cj/create-temp-test-file-with-content content "test.m3u"))
             (tracks (cj/music--m3u-file-tracks m3u-file)))
        (should (equal tracks '("/track1.mp3" "/track2.mp3" "/track3.mp3" "/track4.mp3" "/track5.mp3"))))
    (test-music-config--m3u-file-tracks-teardown)))

;;; Error Cases

(ert-deftest test-music-config--m3u-file-tracks-error-nonexistent-file-returns-nil ()
  "Parse nonexistent file returns nil."
  (should (null (cj/music--m3u-file-tracks "/nonexistent/path/playlist.m3u"))))

(ert-deftest test-music-config--m3u-file-tracks-error-nil-input-returns-nil ()
  "Parse nil input returns nil gracefully."
  (should (null (cj/music--m3u-file-tracks nil))))

(provide 'test-music-config--m3u-file-tracks)
;;; test-music-config--m3u-file-tracks.el ends here
