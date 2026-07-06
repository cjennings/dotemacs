;;; test-music-config--m3u-entries.el --- Tests for #EXTINF/UUID/favicon parse -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music--m3u-entries': parse .m3u text into an alist of
;; (stream-url . plist), each plist carrying :name (the #EXTINF label), :uuid
;; (#RADIOBROWSERUUID), and :favicon (#RADIOBROWSERFAVICON).  This is the one
;; pure parser both the name resolution (Phase 1) and the cover-art layer read.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(ert-deftest test-music-config--m3u-entries-normal-name-only ()
  "Normal: an #EXTINF + url pair yields :name with nil :uuid and :favicon."
  (let* ((text "#EXTM3U\n#EXTINF:1,SomaFM Groove Salad\nhttps://ice6.somafm.com/gs\n")
         (e (cdr (assoc "https://ice6.somafm.com/gs" (cj/music--m3u-entries text)))))
    (should (equal (plist-get e :name) "SomaFM Groove Salad"))
    (should (null (plist-get e :uuid)))
    (should (null (plist-get e :favicon)))))

(ert-deftest test-music-config--m3u-entries-normal-uuid-and-favicon ()
  "Normal: UUID and favicon comment lines are captured onto the entry."
  (let* ((text (concat "#EXTM3U\n#EXTINF:1,Jazz24\n"
                       "#RADIOBROWSERUUID:abc-123\n"
                       "#RADIOBROWSERFAVICON:https://cdn.example/jazz.png\n"
                       "https://jazz.example/live\n"))
         (e (cdr (assoc "https://jazz.example/live" (cj/music--m3u-entries text)))))
    (should (equal (plist-get e :name) "Jazz24"))
    (should (equal (plist-get e :uuid) "abc-123"))
    (should (equal (plist-get e :favicon) "https://cdn.example/jazz.png"))))

(ert-deftest test-music-config--m3u-entries-normal-multiple-reset ()
  "Normal: fields reset between stations (station B has no UUID leak from A)."
  (let* ((text (concat "#EXTINF:1,A\n#RADIOBROWSERUUID:aaa\nhttps://a.example/1\n"
                       "#EXTINF:1,B\nhttps://b.example/2\n"))
         (entries (cj/music--m3u-entries text))
         (b (cdr (assoc "https://b.example/2" entries))))
    (should (equal (plist-get b :name) "B"))
    (should (null (plist-get b :uuid)))))

(ert-deftest test-music-config--m3u-entries-boundary-url-without-extinf ()
  "Boundary: a bare url with no #EXTINF is skipped."
  (should (null (cj/music--m3u-entries "#EXTM3U\nhttps://plain.example/stream\n"))))

(ert-deftest test-music-config--m3u-entries-boundary-empty ()
  "Boundary: empty text yields nil."
  (should (null (cj/music--m3u-entries ""))))

(ert-deftest test-music-config--m3u-entries-boundary-comma-in-name ()
  "Boundary: a comma inside the #EXTINF label is preserved."
  (let* ((text "#EXTINF:1,Radio, the Good Kind\nhttps://x.example/s\n")
         (e (cdr (assoc "https://x.example/s" (cj/music--m3u-entries text)))))
    (should (equal (plist-get e :name) "Radio, the Good Kind"))))

(provide 'test-music-config--m3u-entries)
;;; test-music-config--m3u-entries.el ends here
