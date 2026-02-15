;;; test-music-config--track-description.el --- Tests for track description -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--track-description function.
;; Tests the custom track description that replaces EMMS's default file-path display
;; with human-readable formats based on track type and available metadata.
;;
;; Track construction: EMMS tracks are alists created with `emms-track' and
;; populated with `emms-track-set'. No playlist buffer or player state needed.
;;
;; Test organization:
;; - Normal Cases: Tagged tracks (artist+title+duration), partial metadata, file fallback, URL
;; - Boundary Cases: Empty strings, missing fields, special characters, long names
;; - Error Cases: Unknown track type fallback
;;
;;; Code:

(require 'ert)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Add EMMS elpa directory to load path for batch testing
(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir
    (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'emms-playlist-mode)
(require 'music-config)

;;; Test helpers

(defun test-track-description--make-file-track (path &optional title artist duration)
  "Create a file TRACK with PATH and optional metadata TITLE, ARTIST, DURATION."
  (let ((track (emms-track 'file path)))
    (when title (emms-track-set track 'info-title title))
    (when artist (emms-track-set track 'info-artist artist))
    (when duration (emms-track-set track 'info-playing-time duration))
    track))

(defun test-track-description--make-url-track (url &optional title artist duration)
  "Create a URL TRACK with URL and optional metadata TITLE, ARTIST, DURATION."
  (let ((track (emms-track 'url url)))
    (when title (emms-track-set track 'info-title title))
    (when artist (emms-track-set track 'info-artist artist))
    (when duration (emms-track-set track 'info-playing-time duration))
    track))

;;; Normal Cases — Tagged tracks (artist + title + duration)

(ert-deftest test-music-config--track-description-normal-full-metadata ()
  "Validate track with artist, title, and duration shows all three."
  (let ((track (test-track-description--make-file-track
                "/music/Kind of Blue/01 - So What.flac"
                "So What" "Miles Davis" 562)))
    (should (string= (cj/music--track-description track)
                     "Miles Davis - So What  [9:22]"))))

(ert-deftest test-music-config--track-description-normal-title-and-artist-no-duration ()
  "Validate track with artist and title but no duration omits bracket."
  (let ((track (test-track-description--make-file-track
                "/test/uncached-nodur.mp3" "Blue in Green" "Miles Davis")))
    (should (string= (cj/music--track-description track)
                     "Miles Davis - Blue in Green"))))

(ert-deftest test-music-config--track-description-normal-title-only ()
  "Validate track with title but no artist shows title alone."
  (let ((track (test-track-description--make-file-track
                "/test/uncached-noartist.mp3" "Flamenco Sketches" nil 566)))
    (should (string= (cj/music--track-description track)
                     "Flamenco Sketches  [9:26]"))))

(ert-deftest test-music-config--track-description-normal-title-only-no-duration ()
  "Validate track with only title shows just the title."
  (let ((track (test-track-description--make-file-track
                "/test/uncached-titleonly.mp3" "All Blues")))
    (should (string= (cj/music--track-description track)
                     "All Blues"))))

;;; Normal Cases — File tracks without tags

(ert-deftest test-music-config--track-description-normal-file-no-tags ()
  "Validate untagged file shows filename without path or extension."
  (let ((track (test-track-description--make-file-track
                "/music/Kind of Blue/02 - Freddie Freeloader.flac")))
    (should (string= (cj/music--track-description track)
                     "02 - Freddie Freeloader"))))

(ert-deftest test-music-config--track-description-normal-file-nested-path ()
  "Validate deeply nested path still shows only the filename."
  (let ((track (test-track-description--make-file-track
                "/music/Jazz/Miles Davis/Kind of Blue/01 - So What.mp3")))
    (should (string= (cj/music--track-description track)
                     "01 - So What"))))

;;; Normal Cases — URL tracks

(ert-deftest test-music-config--track-description-normal-url-plain ()
  "Validate plain URL is shown as-is."
  (let ((track (test-track-description--make-url-track
                "https://radio.example.com/stream")))
    (should (string= (cj/music--track-description track)
                     "https://radio.example.com/stream"))))

(ert-deftest test-music-config--track-description-normal-url-percent-encoded ()
  "Validate percent-encoded URL characters are decoded."
  (let ((track (test-track-description--make-url-track
                "https://radio.example.com/my%20station%21")))
    (should (string= (cj/music--track-description track)
                     "https://radio.example.com/my station!"))))

(ert-deftest test-music-config--track-description-normal-url-with-tags ()
  "Validate URL track with tags uses tag display, not URL."
  (let ((track (test-track-description--make-url-track
                "https://radio.example.com/stream"
                "Jazz FM" "Radio Station" 0)))
    ;; Duration 0 → nil from format-duration, so no bracket
    (should (string= (cj/music--track-description track)
                     "Radio Station - Jazz FM"))))

;;; Boundary Cases

(ert-deftest test-music-config--track-description-boundary-empty-title-string ()
  "Validate empty title string is still truthy, shows empty result."
  (let ((track (test-track-description--make-file-track
                "/music/track.mp3" "" "Artist")))
    ;; Empty string is non-nil, so title branch is taken
    (should (string= (cj/music--track-description track)
                     "Artist - "))))

(ert-deftest test-music-config--track-description-boundary-file-no-extension ()
  "Validate file without extension shows full filename."
  (let ((track (test-track-description--make-file-track "/music/README")))
    (should (string= (cj/music--track-description track)
                     "README"))))

(ert-deftest test-music-config--track-description-boundary-file-multiple-dots ()
  "Validate file with multiple dots strips only the final extension."
  (let ((track (test-track-description--make-file-track
                "/music/disc.1.track.03.flac")))
    (should (string= (cj/music--track-description track)
                     "disc.1.track.03"))))

(ert-deftest test-music-config--track-description-boundary-unicode-title ()
  "Validate unicode characters in metadata are preserved."
  (let ((track (test-track-description--make-file-track
                "/music/track.mp3" "夜に駆ける" "YOASOBI" 258)))
    (should (string= (cj/music--track-description track)
                     "YOASOBI - 夜に駆ける  [4:18]"))))

(ert-deftest test-music-config--track-description-boundary-url-utf8-percent-encoded ()
  "Validate percent-encoded UTF-8 in URL is decoded correctly."
  (let ((track (test-track-description--make-url-track
                "https://example.com/caf%C3%A9")))
    (should (string= (cj/music--track-description track)
                     "https://example.com/café"))))

(ert-deftest test-music-config--track-description-boundary-short-duration ()
  "Validate 1-second track formats correctly in bracket."
  (let ((track (test-track-description--make-file-track
                "/music/t.mp3" "Beep" nil 1)))
    (should (string= (cj/music--track-description track)
                     "Beep  [0:01]"))))

;;; Error Cases

(ert-deftest test-music-config--track-description-error-unknown-type-fallback ()
  "Validate unknown track type uses emms-track-simple-description fallback."
  (let ((track (emms-track 'streamlist "https://example.com/playlist.m3u")))
    ;; Should not error; falls through to simple-description
    (let ((result (cj/music--track-description track)))
      (should (stringp result))
      (should (string-match-p "example\\.com" result)))))

(provide 'test-music-config--track-description)
;;; test-music-config--track-description.el ends here
