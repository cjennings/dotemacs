;;; test-music-config--art-favicon-url.el --- Tests for stream favicon URL -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music-art--favicon-url': the direct favicon image URL for
;; a url track, taken from the #RADIOBROWSERFAVICON captured at station creation.
;; A station with only a UUID resolves its favicon via a separate byuuid lookup
;; (done in the impure orchestrator), so this pure helper returns nil there.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(ert-deftest test-music-config--art-favicon-url-normal-captured ()
  "Normal: a captured #RADIOBROWSERFAVICON is returned directly."
  (let ((track (emms-track 'url "https://fav.somafm.com/gs"))
        (entries '(("https://fav.somafm.com/gs"
                    :name "GS" :uuid "u1" :favicon "https://cdn.example/gs.png"))))
    (should (string= (cj/music-art--favicon-url track entries)
                     "https://cdn.example/gs.png"))))

(ert-deftest test-music-config--art-favicon-url-boundary-uuid-only ()
  "Boundary: a station with a UUID but no captured favicon returns nil
\(the byuuid lookup is the orchestrator's job)."
  (let ((track (emms-track 'url "https://fav2.example.net/live"))
        (entries '(("https://fav2.example.net/live"
                    :name "X" :uuid "u2" :favicon nil))))
    (should (null (cj/music-art--favicon-url track entries)))))

(ert-deftest test-music-config--art-favicon-url-boundary-empty-favicon ()
  "Boundary: an empty-string favicon is treated as absent."
  (let ((track (emms-track 'url "https://fav3.example.net/live"))
        (entries '(("https://fav3.example.net/live" :name "X" :uuid "u3" :favicon ""))))
    (should (null (cj/music-art--favicon-url track entries)))))

(ert-deftest test-music-config--art-favicon-url-error-file-track ()
  "Error: a file track has no stream favicon URL."
  (let ((track (emms-track 'file "/music/x.flac")))
    (should (null (cj/music-art--favicon-url track nil)))))

(ert-deftest test-music-config--art-favicon-url-normal-track-property ()
  "Normal: a queued lookup station carries its favicon as a track property."
  (let ((track (emms-track 'url "https://fp.example.net/live")))
    (emms-track-set track 'radio-favicon "https://fp.example.net/icon.png")
    (should (string= (cj/music-art--favicon-url track nil)
                     "https://fp.example.net/icon.png"))))

(ert-deftest test-music-config--art-favicon-url-normal-property-beats-entries ()
  "Normal: the track property wins over a conflicting entries favicon."
  (let* ((url "https://fp2.example.net/live")
         (track (emms-track 'url url))
         (entries (list (list url :name "X" :uuid nil
                              :favicon "https://entries.example/e.png"))))
    (emms-track-set track 'radio-favicon "https://prop.example/p.png")
    (should (string= (cj/music-art--favicon-url track entries)
                     "https://prop.example/p.png"))))

(provide 'test-music-config--art-favicon-url)
;;; test-music-config--art-favicon-url.el ends here
