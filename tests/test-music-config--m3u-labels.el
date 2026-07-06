;;; test-music-config--m3u-labels.el --- Tests for #EXTINF label extraction -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music--m3u-labels': parse .m3u text into an alist of
;; (stream-url . #EXTINF-label) pairs.  This is the pure core that lets a url
;; track resolve to its station name (the label written at creation) instead
;; of the raw stream URL.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(ert-deftest test-music-config--m3u-labels-normal-single ()
  "Normal: one #EXTINF + url pair yields one (url . label) cons."
  (let ((text "#EXTM3U\n#EXTINF:1,SomaFM Groove Salad\nhttps://ice6.somafm.com/gs\n"))
    (should (equal (cj/music--m3u-labels text)
                   '(("https://ice6.somafm.com/gs" . "SomaFM Groove Salad"))))))

(ert-deftest test-music-config--m3u-labels-normal-uuid-line-between ()
  "Normal: a #RADIOBROWSERUUID line between #EXTINF and the url is skipped."
  (let ((text (concat "#EXTM3U\n#EXTINF:1,Jazz24\n"
                      "#RADIOBROWSERUUID:abc-123\nhttps://jazz.example/live\n")))
    (should (equal (cj/music--m3u-labels text)
                   '(("https://jazz.example/live" . "Jazz24"))))))

(ert-deftest test-music-config--m3u-labels-normal-multiple ()
  "Normal: multiple stations each yield their own pair."
  (let ((text (concat "#EXTM3U\n"
                      "#EXTINF:1,Station A\nhttps://a.example/1\n"
                      "#EXTINF:-1,Station B\nhttps://b.example/2\n")))
    (should (equal (cj/music--m3u-labels text)
                   '(("https://a.example/1" . "Station A")
                     ("https://b.example/2" . "Station B"))))))

(ert-deftest test-music-config--m3u-labels-boundary-url-without-extinf ()
  "Boundary: a bare url with no preceding #EXTINF produces no pair."
  (let ((text "#EXTM3U\nhttps://plain.example/stream\n"))
    (should (null (cj/music--m3u-labels text)))))

(ert-deftest test-music-config--m3u-labels-boundary-empty ()
  "Boundary: empty text yields nil."
  (should (null (cj/music--m3u-labels ""))))

(ert-deftest test-music-config--m3u-labels-boundary-comma-in-name ()
  "Boundary: a comma inside the label is preserved (split on the first only)."
  (let ((text "#EXTINF:1,Radio, the Good Kind\nhttps://x.example/s\n"))
    (should (equal (cj/music--m3u-labels text)
                   '(("https://x.example/s" . "Radio, the Good Kind"))))))

(provide 'test-music-config--m3u-labels)
;;; test-music-config--m3u-labels.el ends here
