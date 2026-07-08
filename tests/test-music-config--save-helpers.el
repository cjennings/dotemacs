;;; test-music-config--save-helpers.el --- save default-name + directory tests -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The pure helpers behind the queue-first save flow: which name the save
;; prompt pre-fills (`cj/music--save-default-name') and which directory the
;; file lands in (`cj/music--save-directory').  An all-stream queue saves into
;; the radio playlist home (`cj/music-radio-save-dir'); anything else saves
;; into `cj/music-m3u-root'.

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

(declare-function cj/music--save-default-name "music-config" (tracks file entries))
(declare-function cj/music--save-directory "music-config" (tracks))
(declare-function cj/music-radio--station-track "music-config" (st))

;;; --------------------------- save-default-name -------------------------------

(ert-deftest test-music-save-default-name-normal-associated-file-wins ()
  "Normal: an associated playlist file names the save, station or not."
  (let ((tr (cj/music-radio--station-track '(:name "S" :url "https://s.example/x"))))
    (should (equal (cj/music--save-default-name (list tr) "/pl/jazz.m3u" nil)
                   "jazz"))))

(ert-deftest test-music-save-default-name-normal-station-title ()
  "Normal: with no file, the first url track's station name pre-fills."
  (let ((tr (cj/music-radio--station-track
             '(:name "Groove Salad" :url "https://gs.example/x"))))
    (should (equal (cj/music--save-default-name (list tr) nil nil)
                   "Groove Salad"))))

(ert-deftest test-music-save-default-name-normal-first-url-track-wins ()
  "Normal: a file track ahead of the station doesn't block the station name;
the first URL track with a name wins."
  (let ((f (emms-track 'file "/music/a.flac"))
        (tr (cj/music-radio--station-track
             '(:name "Second Pick" :url "https://sp.example/x"))))
    (should (equal (cj/music--save-default-name (list f tr) nil nil)
                   "Second Pick"))))

(ert-deftest test-music-save-default-name-normal-entries-fallback ()
  "Normal: a propertyless url track resolves its name from ENTRIES."
  (let* ((url "https://legacy.example/s")
         (tr (emms-track 'url url))
         (entries (list (list url :name "Legacy FM" :uuid nil :favicon nil))))
    (should (equal (cj/music--save-default-name (list tr) nil entries)
                   "Legacy FM"))))

(ert-deftest test-music-save-default-name-boundary-no-candidates-nil ()
  "Boundary: no file, no url tracks -> nil (caller falls back to a timestamp)."
  (should-not (cj/music--save-default-name
               (list (emms-track 'file "/music/a.flac")) nil nil))
  (should-not (cj/music--save-default-name nil nil nil)))

;;; ---------------------------- save-directory ---------------------------------

(ert-deftest test-music-save-directory-normal-all-streams-radio-dir ()
  "Normal: an all-stream queue saves into the radio playlist dir."
  (let ((u1 (emms-track 'url "https://a.example/1"))
        (u2 (emms-track 'url "https://b.example/2")))
    (should (equal (cj/music--save-directory (list u1 u2))
                   cj/music-radio-save-dir))))

(ert-deftest test-music-save-directory-normal-mixed-goes-to-m3u-root ()
  "Normal: any non-stream track routes the save to the music playlist root."
  (let ((u (emms-track 'url "https://a.example/1"))
        (f (emms-track 'file "/music/a.flac")))
    (should (equal (cj/music--save-directory (list u f))
                   cj/music-m3u-root))))

(ert-deftest test-music-save-directory-boundary-empty-goes-to-m3u-root ()
  "Boundary: an empty queue defaults to the music playlist root."
  (should (equal (cj/music--save-directory nil) cj/music-m3u-root)))

(provide 'test-music-config--save-helpers)
;;; test-music-config--save-helpers.el ends here
