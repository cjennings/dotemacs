;;; test-music-config--m3u-text.el --- playlist .m3u emitter tests -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The custom .m3u emitter behind playlist save.  The stock EMMS m3u writer
;; emits bare URLs, which would throw away a station's name/uuid/favicon on
;; save; this emitter writes the same #EXTINF / #RADIOBROWSERUUID /
;; #RADIOBROWSERFAVICON lines the parser (`cj/music--m3u-entries') reads, so
;; save -> load round-trips a station's display name and cover-art metadata.
;; Metadata comes from track properties first, the m3u-scan entries as
;; fallback (a loaded legacy playlist has entries but no properties).

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module.
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(declare-function cj/music--m3u-text "music-config" (tracks entries))
(declare-function cj/music-radio--station-track "music-config" (st))

(ert-deftest test-music-m3u-text-normal-file-track-bare-path ()
  "Normal: a file track is a bare absolute path under the #EXTM3U header."
  (let ((text (cj/music--m3u-text (list (emms-track 'file "/music/a.flac")) nil)))
    (should (string-prefix-p "#EXTM3U\n" text))
    (should (string-match-p "^/music/a\\.flac$" text))))

(ert-deftest test-music-m3u-text-normal-url-track-from-properties ()
  "Normal: a url track with properties emits uuid, favicon, EXTINF, and url."
  (let* ((track (cj/music-radio--station-track
                 '(:name "Groove Salad" :url "https://ck.somafm.com/gs"
                   :stationuuid "uuid-1" :favicon "https://somafm.com/i.png")))
         (text (cj/music--m3u-text (list track) nil)))
    (should (string-match-p "^#RADIOBROWSERUUID:uuid-1$" text))
    (should (string-match-p "^#RADIOBROWSERFAVICON:https://somafm\\.com/i\\.png$" text))
    (should (string-match-p "^#EXTINF:-1,Groove Salad$" text))
    (should (string-match-p "^https://ck\\.somafm\\.com/gs$" text))))

(ert-deftest test-music-m3u-text-normal-url-track-from-entries-fallback ()
  "Normal: a propertyless url track (a loaded legacy playlist) resolves its
metadata from the ENTRIES alist."
  (let* ((url "https://legacy.example/stream")
         (track (emms-track 'url url))
         (entries (list (list url :name "Legacy FM" :uuid "uuid-9"
                              :favicon "https://legacy.example/f.ico")))
         (text (cj/music--m3u-text (list track) entries)))
    (should (string-match-p "^#RADIOBROWSERUUID:uuid-9$" text))
    (should (string-match-p "^#EXTINF:-1,Legacy FM$" text))))

(ert-deftest test-music-m3u-text-boundary-url-track-no-metadata ()
  "Boundary: a url track with neither properties nor entries still gets an
EXTINF label (the tidied host) and no uuid/favicon lines."
  (let ((text (cj/music--m3u-text
               (list (emms-track 'url "https://ice6.somafm.com/live")) nil)))
    (should (string-match-p "^#EXTINF:-1,somafm\\.com$" text))
    (should-not (string-match-p "RADIOBROWSERUUID" text))
    (should-not (string-match-p "RADIOBROWSERFAVICON" text))))

(ert-deftest test-music-m3u-text-normal-mixed-order-preserved ()
  "Normal: a mixed queue keeps its track order in the file."
  (let* ((f (emms-track 'file "/music/b.mp3"))
         (u (cj/music-radio--station-track '(:name "S" :url "https://s.example/x")))
         (text (cj/music--m3u-text (list f u) nil)))
    (should (< (string-match "^/music/b\\.mp3$" text)
               (string-match "^https://s\\.example/x$" text)))))

(ert-deftest test-music-m3u-text-round-trip-through-parser ()
  "Normal: the parser recovers name, uuid, and favicon from emitted text."
  (let* ((track (cj/music-radio--station-track
                 '(:name "Round Trip" :url "https://rt.example/s"
                   :stationuuid "uuid-rt" :favicon "https://rt.example/f.png")))
         (entries (cj/music--m3u-entries (cj/music--m3u-text (list track) nil)))
         (meta (cdr (assoc "https://rt.example/s" entries))))
    (should (equal (plist-get meta :name) "Round Trip"))
    (should (equal (plist-get meta :uuid) "uuid-rt"))
    (should (equal (plist-get meta :favicon) "https://rt.example/f.png"))))

(ert-deftest test-music-m3u-text-boundary-empty-playlist-header-only ()
  "Boundary: an empty track list is just the #EXTM3U header."
  (should (equal (cj/music--m3u-text nil nil) "#EXTM3U\n")))

(provide 'test-music-config--m3u-text)
;;; test-music-config--m3u-text.el ends here
