;;; test-music-config--display-name.el --- Tests for track display-name -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music--display-name' and `cj/music--format-meta'.
;;
;; display-name is the pure, name-only resolver shared by the header's Current
;; line and the playlist row renderer: for a tagged track it returns
;; "Artist - Title" (no duration -- duration is right-aligned meta, not name);
;; for an untagged file the clean filename; for a url track the #EXTINF label
;; from a passed name-map, else a tidied host; unknown types fall back to
;; emms-track-simple-description.
;;
;; format-meta returns the right-aligned meta string for a row: a file's
;; duration as "[M:SS]", empty otherwise.
;;
;; Track independence: `emms-track' returns a track cached by (type . name)
;; once EMMS is loaded, so two tests sharing a name would share a mutated
;; object.  Every test below uses a UNIQUE track name to stay independent.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'emms-playlist-mode)
(require 'music-config)

;;; Helpers

(defun test-display-name--file (path &optional title artist duration)
  "Create a file TRACK with PATH and optional TITLE ARTIST DURATION."
  (let ((track (emms-track 'file path)))
    (when title (emms-track-set track 'info-title title))
    (when artist (emms-track-set track 'info-artist artist))
    (when duration (emms-track-set track 'info-playing-time duration))
    track))

(defun test-display-name--url (url &optional title artist)
  "Create a url TRACK with URL and optional TITLE ARTIST."
  (let ((track (emms-track 'url url)))
    (when title (emms-track-set track 'info-title title))
    (when artist (emms-track-set track 'info-artist artist))
    track))

;;; Normal -- tagged tracks (no duration in the name)

(ert-deftest test-music-config--display-name-normal-artist-title ()
  "Normal: tagged track shows Artist - Title, no duration bracket."
  (let ((track (test-display-name--file
                "/dn/artist-title.flac" "So What" "Miles Davis" 562)))
    (should (string= (cj/music--display-name track) "Miles Davis - So What"))))

(ert-deftest test-music-config--display-name-normal-title-only ()
  "Normal: title without artist shows the title alone."
  (let ((track (test-display-name--file "/dn/title-only.mp3" "Flamenco Sketches" nil 566)))
    (should (string= (cj/music--display-name track) "Flamenco Sketches"))))

;;; Normal -- untagged file

(ert-deftest test-music-config--display-name-normal-file-no-tags ()
  "Normal: untagged file shows filename without path or extension."
  (let ((track (test-display-name--file "/dn/Kind of Blue/02 - Freddie.flac")))
    (should (string= (cj/music--display-name track) "02 - Freddie"))))

;;; Normal -- url resolves to #EXTINF label from the name-map

(ert-deftest test-music-config--display-name-normal-url-label-from-map ()
  "Normal: a url track resolves to its #EXTINF label via the name-map."
  (let ((track (test-display-name--url "https://ice6.somafm.com/groovesalad-256-mp3"))
        (map '(("https://ice6.somafm.com/groovesalad-256-mp3" . "SomaFM Groove Salad"))))
    (should (string= (cj/music--display-name track map) "SomaFM Groove Salad"))))

;;; Normal -- url with tags formats like a tagged track

(ert-deftest test-music-config--display-name-normal-url-with-tags ()
  "Normal: a url track carrying tags uses Artist - Title, not the URL."
  (let ((track (test-display-name--url "https://tagged.example.com/stream"
                                       "Jazz FM" "Radio Station")))
    (should (string= (cj/music--display-name track) "Radio Station - Jazz FM"))))

;;; Boundary -- url with no label falls back to a tidied host

(ert-deftest test-music-config--display-name-boundary-url-host-fallback ()
  "Boundary: a url with no label and no map falls back to the tidied host."
  (let ((track (test-display-name--url "https://ice6.hostonly.somafm.com/gs")))
    (should (string= (cj/music--display-name track) "somafm.com"))))

(ert-deftest test-music-config--display-name-boundary-url-not-in-map ()
  "Boundary: a url absent from a non-empty map still falls to the host."
  (let ((track (test-display-name--url "https://stream.other.net/live"))
        (map '(("https://ice6.somafm.com/x" . "Groove Salad"))))
    (should (string= (cj/music--display-name track map) "other.net"))))

(ert-deftest test-music-config--display-name-boundary-unicode-title ()
  "Boundary: unicode in the title is preserved."
  (let ((track (test-display-name--file "/dn/unicode.mp3" "夜に駆ける" "YOASOBI" 258)))
    (should (string= (cj/music--display-name track) "YOASOBI - 夜に駆ける"))))

(ert-deftest test-music-config--display-name-boundary-file-multiple-dots ()
  "Boundary: only the final extension is stripped."
  (let ((track (test-display-name--file "/dn/disc.1.track.03.flac")))
    (should (string= (cj/music--display-name track) "disc.1.track.03"))))

;;; Error -- unknown type falls back without erroring

(ert-deftest test-music-config--display-name-error-unknown-type ()
  "Error: an unknown track type falls back to a string, no error."
  (let* ((track (emms-track 'streamlist "https://unknown.example.com/playlist.m3u"))
         (result (cj/music--display-name track)))
    (should (stringp result))
    (should (string-match-p "example\\.com" result))))

;;; format-meta

(ert-deftest test-music-config--format-meta-normal-file-duration ()
  "Normal: a file with a duration yields the bracketed M:SS meta."
  (let ((track (test-display-name--file "/dn/meta-dur.flac" "So What" "Miles" 562)))
    (should (string= (cj/music--format-meta track) "[9:22]"))))

(ert-deftest test-music-config--format-meta-boundary-no-duration ()
  "Boundary: a track with no duration yields an empty meta string."
  (let ((track (test-display-name--file "/dn/meta-nodur.flac" "So What" "Miles")))
    (should (string= (cj/music--format-meta track) ""))))

(ert-deftest test-music-config--format-meta-boundary-url-no-meta ()
  "Boundary: a url stream with no duration yields empty meta."
  (let ((track (test-display-name--url "https://meta.example.com/stream")))
    (should (string= (cj/music--format-meta track) ""))))

(provide 'test-music-config--display-name)
;;; test-music-config--display-name.el ends here
