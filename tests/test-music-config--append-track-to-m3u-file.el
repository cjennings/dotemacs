;;; test-music-config--append-track-to-m3u-file.el --- Tests for appending tracks to M3U files -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--append-track-to-m3u-file function.
;; Tests the pure, deterministic helper that appends track paths to M3U files.
;;
;; Test organization:
;; - Normal Cases: Standard append operations
;; - Boundary Cases: Edge conditions (unicode, long paths, special chars)
;; - Error Cases: File errors (missing, read-only, directory instead of file)
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

(defun test-music-config--append-track-to-m3u-file-setup ()
  "Setup test environment."
  (cj/create-test-base-dir))

(defun test-music-config--append-track-to-m3u-file-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--append-track-to-m3u-file-normal-empty-file-appends-track ()
  "Append to brand new empty M3U file."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file "test-playlist-"))
             (track-path (expand-file-name "artist/song.mp3" cj/music-root))
             (expected-relative "artist/song.mp3"))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string) (concat expected-relative "\n")))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-normal-existing-with-newline-appends-track ()
  "Append to file with existing content ending with newline."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((existing-content "first.mp3\n")
             (cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file-with-content existing-content "test-playlist-"))
             (track-path (expand-file-name "second.mp3" cj/music-root))
             (expected-relative "second.mp3"))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string)
                          (concat existing-content expected-relative "\n")))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-normal-existing-without-newline-appends-track ()
  "Append to file without trailing newline adds leading newline."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((existing-content "first.mp3")
             (cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file-with-content existing-content "test-playlist-"))
             (track-path (expand-file-name "second.mp3" cj/music-root))
             (expected-relative "second.mp3"))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string)
                          (concat existing-content "\n" expected-relative "\n")))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-normal-multiple-appends-all-succeed ()
  "Multiple appends to same file all succeed (allows duplicates)."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file "test-playlist-"))
             (track1 (expand-file-name "track1.mp3" cj/music-root))
             (track2 (expand-file-name "track2.mp3" cj/music-root))
             (track1-duplicate (expand-file-name "track1.mp3" cj/music-root))
             (rel1 "track1.mp3")
             (rel2 "track2.mp3"))
        (cj/music--append-track-to-m3u-file track1 m3u-file)
        (cj/music--append-track-to-m3u-file track2 m3u-file)
        (cj/music--append-track-to-m3u-file track1-duplicate m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (let ((content (buffer-string)))
            (should (string= content
                            (concat rel1 "\n" rel2 "\n" rel1 "\n"))))))
    (test-music-config--append-track-to-m3u-file-teardown)))

;;; Normal Cases: round-trip with the reader

(ert-deftest test-music-config--append-track-to-m3u-file-normal-round-trips-through-the-reader ()
  "Normal: the same-directory case round-trips through the reader.
A positive control only.  With the playlist and the music root in one
directory both candidate bases produce the same string, so this passes
against the old writer too — the discriminating cases are the two tests
below, which put the bases at different depths."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((base (cj/create-test-base-dir))
             (cj/music-root base)
             (m3u-file (cj/create-temp-test-file "test-playlist-"))
             (track-path (expand-file-name "artist/song.mp3" base)))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (should (equal (cj/music--m3u-file-tracks m3u-file)
                       (list track-path))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-normal-round-trips-outside-the-music-root ()
  "Normal/regression: a playlist living outside `cj/music-root' round-trips.
This is the case the old writer got wrong.  It based every relative path on
`cj/music-root' wherever the playlist sat, while the reader resolved against
the playlist's directory.  Inside the music root the two coincide, which is
why the defect stayed invisible until a playlist moved out of it."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      ;; The layout mirrors the real one: playlists/ and audio/ are siblings
      ;; under mpd/, and the music root is a separate tree at a different depth.
      ;; The depth difference is load-bearing -- put the music root alongside
      ;; playlists/ instead and both bases yield the same relative path, so the
      ;; test passes against the broken writer and proves nothing.
      (let* ((base (cj/create-test-base-dir))
             (playlists (expand-file-name "mpd/playlists/" base))
             (audio (expand-file-name "mpd/audio/" base))
             (cj/music-root (expand-file-name "music/" base))
             (m3u-file (expand-file-name "ambience.m3u" playlists))
             (track-path (expand-file-name "rain-loop.mp3" audio)))
        (make-directory playlists t)
        (make-directory audio t)
        (make-directory cj/music-root t)
        (with-temp-buffer (write-file m3u-file))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (should (equal (cj/music--m3u-file-tracks m3u-file)
                       (list track-path))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-normal-base-is-the-playlist-not-the-music-root ()
  "Normal: the written line is relative to the playlist's own directory.
Stated directly as well as via the round-trip, so a reader change could not
quietly make both sides agree on the wrong base."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      ;; Same depth asymmetry as the round-trip test above, and for the same
      ;; reason: with the music root alongside playlists/ both bases agree and
      ;; the assertion holds against the broken writer.
      (let* ((base (cj/create-test-base-dir))
             (playlists (expand-file-name "mpd/playlists/" base))
             (audio (expand-file-name "mpd/audio/" base))
             (cj/music-root (expand-file-name "music/" base))
             (m3u-file (expand-file-name "ambience.m3u" playlists))
             (track-path (expand-file-name "rain-loop.mp3" audio)))
        (make-directory playlists t)
        (make-directory audio t)
        (make-directory cj/music-root t)
        (with-temp-buffer (write-file m3u-file))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string) "../audio/rain-loop.mp3\n"))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-normal-deep-parent-chain-round-trips ()
  "Normal: a track several levels above the playlist round-trips.
The single \"../\" the tests above exercise is the shallow case.  Once
playlists and audio can live anywhere, a multi-level chain is the shape most
likely to be broken by a later \"let's normalize these paths\" edit, and
nothing else here would catch it."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((base (cj/create-test-base-dir))
             (playlists (expand-file-name "a/b/c/playlists/" base))
             (cj/music-root (expand-file-name "music/" base))
             (m3u-file (expand-file-name "deep.m3u" playlists))
             (track-path (expand-file-name "faraway/song.mp3" base)))
        (make-directory playlists t)
        (make-directory (expand-file-name "faraway/" base) t)
        (make-directory cj/music-root t)
        (with-temp-buffer (write-file m3u-file))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          ;; playlists/ -> c -> b -> a -> base is four hops up.
          (should (string= (buffer-string) "../../../../faraway/song.mp3\n")))
        (should (equal (cj/music--m3u-file-tracks m3u-file)
                       (list track-path))))
    (test-music-config--append-track-to-m3u-file-teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--append-track-to-m3u-file-boundary-very-long-path-appends-successfully ()
  "Append very long track path without truncation."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file "test-playlist-"))
             ;; Create a relative path that's ~450 chars long
             (relative-path (concat (make-string 440 ?a) "/song.mp3"))
             (track-path (expand-file-name relative-path cj/music-root)))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string) (concat relative-path "\n")))
          (should (= (length (buffer-string)) (1+ (length relative-path))))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-boundary-path-with-unicode-appends-successfully ()
  "Append path with unicode characters preserves UTF-8 encoding."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file "test-playlist-"))
             (relative-path "中文/artist-名前/song🎵.mp3")
             (track-path (expand-file-name relative-path cj/music-root)))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string) (concat relative-path "\n")))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-boundary-path-with-spaces-appends-successfully ()
  "Append path with spaces and special characters."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file "test-playlist-"))
             (relative-path "Artist Name/Album (2024)/01 - Song's Title [Remix].mp3")
             (track-path (expand-file-name relative-path cj/music-root)))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string) (concat relative-path "\n")))))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-boundary-m3u-with-comments-appends-after ()
  "Append to M3U file containing comments and metadata."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((existing-content "#EXTM3U\n#EXTINF:-1,Radio Station\nhttp://stream.url/radio\n")
             (cj/music-root (cj/create-test-base-dir))
             (m3u-file (cj/create-temp-test-file-with-content existing-content "test-playlist-"))
             (relative-path "local-track.mp3")
             (track-path (expand-file-name relative-path cj/music-root)))
        (cj/music--append-track-to-m3u-file track-path m3u-file)
        (with-temp-buffer
          (insert-file-contents m3u-file)
          (should (string= (buffer-string)
                          (concat existing-content relative-path "\n")))))
    (test-music-config--append-track-to-m3u-file-teardown)))

;;; Error Cases

(ert-deftest test-music-config--append-track-to-m3u-file-error-nonexistent-file-signals-error ()
  "Signal error when M3U file doesn't exist."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((m3u-file "/nonexistent/path/to/playlist.m3u")
             (track-path "/home/user/music/song.mp3"))
        (should-error (cj/music--append-track-to-m3u-file track-path m3u-file)
                     :type 'error))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-error-readonly-file-signals-error ()
  "Signal error when M3U file is read-only."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      ;; No `cj/music-root' rebinding here: the writable-p guard signals before
      ;; any path computation runs, so binding it would imply a dependency the
      ;; read-only path does not have.
      (let* ((m3u-file (cj/create-temp-test-file "test-playlist-"))
             (track-path "/home/user/music/song.mp3"))
        ;; Make file read-only
        (set-file-modes m3u-file #o444)
        (should-error (cj/music--append-track-to-m3u-file track-path m3u-file)
                     :type 'error))
    (test-music-config--append-track-to-m3u-file-teardown)))

(ert-deftest test-music-config--append-track-to-m3u-file-error-directory-not-file-signals-error ()
  "Signal error when path points to directory instead of file."
  (test-music-config--append-track-to-m3u-file-setup)
  (unwind-protect
      (let* ((m3u-dir (cj/create-test-subdirectory "test-playlist-dir"))
             (track-path "/home/user/music/song.mp3"))
        (should-error (cj/music--append-track-to-m3u-file track-path m3u-dir)
                     :type 'error))
    (test-music-config--append-track-to-m3u-file-teardown)))

(provide 'test-music-config--append-track-to-m3u-file)
;;; test-music-config--append-track-to-m3u-file.el ends here
