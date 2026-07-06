;;; test-music-config--art-cache-key.el --- Tests for cover-art cache key -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music-art--cache-key': the stable cache-file basename for
;; a track.  A url with a known #RADIOBROWSERUUID keys on the uuid (so the same
;; station shares one cached logo); any other url keys on a hash of its address;
;; a file keys on a hash of its path.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(ert-deftest test-music-config--art-cache-key-normal-uuid ()
  "Normal: a url with a UUID in the entries keys on the UUID."
  (let ((track (emms-track 'url "https://ck.somafm.com/gs"))
        (entries '(("https://ck.somafm.com/gs" :name "GS" :uuid "uuid-42" :favicon nil))))
    (should (string= (cj/music-art--cache-key track entries) "uuid-42"))))

(ert-deftest test-music-config--art-cache-key-boundary-url-no-uuid ()
  "Boundary: a url with no UUID keys on a stable url- hash, not the raw URL."
  (let* ((url "https://ck2.example.net/live")
         (track (emms-track 'url url))
         (key (cj/music-art--cache-key track nil)))
    (should (string-prefix-p "url-" key))
    (should (string= key (concat "url-" (sha1 url))))))

(ert-deftest test-music-config--art-cache-key-normal-file ()
  "Normal: a file keys on a file- hash of its path."
  (let* ((path "/music/Kind of Blue/01.flac")
         (track (emms-track 'file path)))
    (should (string= (cj/music-art--cache-key track nil)
                     (concat "file-" (sha1 path))))))

(ert-deftest test-music-config--art-cache-key-boundary-empty-uuid-falls-to-hash ()
  "Boundary: an empty-string UUID is treated as absent, so the url hashes."
  (let* ((url "https://ck3.example.net/x")
         (track (emms-track 'url url))
         (entries (list (list url :name "X" :uuid "" :favicon nil))))
    (should (string= (cj/music-art--cache-key track entries)
                     (concat "url-" (sha1 url))))))

(provide 'test-music-config--art-cache-key)
;;; test-music-config--art-cache-key.el ends here
