;;; test-music-config--tidy-host.el --- Tests for stream-URL host tidying -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music--tidy-host': reduce a stream URL to a readable host
;; label (scheme dropped, a leading "www." removed), used as the last-resort
;; display name for a url track with no #EXTINF label.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(ert-deftest test-music-config--tidy-host-normal ()
  "Normal: scheme and path are dropped, host kept."
  (should (string= (cj/music--tidy-host "https://ice6.somafm.com/groovesalad-256-mp3")
                   "somafm.com")))

(ert-deftest test-music-config--tidy-host-normal-http ()
  "Normal: plain http host with a port keeps the host, drops the port."
  (should (string= (cj/music--tidy-host "http://stream.example.org:8000/live")
                   "example.org")))

(ert-deftest test-music-config--tidy-host-boundary-strip-www ()
  "Boundary: a leading www. is stripped."
  (should (string= (cj/music--tidy-host "https://www.radioparadise.com/m3u/mp3-128.m3u")
                   "radioparadise.com")))

(ert-deftest test-music-config--tidy-host-boundary-bare-domain ()
  "Boundary: a two-label domain is returned unchanged."
  (should (string= (cj/music--tidy-host "http://somafm.com/") "somafm.com")))

(ert-deftest test-music-config--tidy-host-error-not-a-url ()
  "Error: a non-URL string is returned as-is rather than erroring."
  (should (string= (cj/music--tidy-host "not a url") "not a url")))

(provide 'test-music-config--tidy-host)
;;; test-music-config--tidy-host.el ends here
