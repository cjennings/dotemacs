;;; test-music-config--m3u-roots.el --- multi-directory M3U sourcing tests -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The player sources .m3u playlists from a LIST of directories
;; (`cj/music-m3u-roots') so the local-library playlists (~/music) and the
;; dotfiles-tracked internet-radio playlists (MPD's playlist_directory) surface
;; together for selection and loading.  Two pieces are tested:
;;
;; - `cj/music--dedup-m3u-files' — pure: turns a flat list of paths into
;;   (BASENAME . PATH) conses, first occurrence of a basename winning.
;; - `cj/music--get-m3u-files' — unions the roots on disk, skips missing dirs,
;;   and applies the dedup so an earlier root shadows a same-named later one.
;;
;;; Code:

(require 'ert)

;; Stub missing dependencies before loading music-config.
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

(require 'music-config)

(declare-function cj/music--dedup-m3u-files "music-config" (paths))
(declare-function cj/music--get-m3u-files "music-config" ())
(defvar cj/music-m3u-roots)

;;; --------------------------- cj/music--dedup-m3u-files ----------------------

(ert-deftest test-music-config-dedup-m3u-distinct ()
  "Normal: distinct basenames across dirs all appear, in order."
  (should (equal (cj/music--dedup-m3u-files
                  '("/music/rhcp.m3u" "/radio/90s Sounds.m3u"))
                 '(("rhcp.m3u" . "/music/rhcp.m3u")
                   ("90s Sounds.m3u" . "/radio/90s Sounds.m3u")))))

(ert-deftest test-music-config-dedup-m3u-single ()
  "Normal: a single path yields a single cons."
  (should (equal (cj/music--dedup-m3u-files '("/music/blues.m3u"))
                 '(("blues.m3u" . "/music/blues.m3u")))))

(ert-deftest test-music-config-dedup-m3u-collision-first-wins ()
  "Boundary: a basename in two dirs keeps the first path (earlier root wins)."
  (should (equal (cj/music--dedup-m3u-files
                  '("/music/jazz.m3u" "/radio/jazz.m3u"))
                 '(("jazz.m3u" . "/music/jazz.m3u")))))

(ert-deftest test-music-config-dedup-m3u-empty ()
  "Boundary: an empty path list yields nil."
  (should-not (cj/music--dedup-m3u-files '())))

;;; ---------------------------- cj/music--get-m3u-files -----------------------

(ert-deftest test-music-config-get-m3u-unions-roots ()
  "Normal: M3Us from every existing root are unioned."
  (let ((a (make-temp-file "m3u-a-" t))
        (b (make-temp-file "m3u-b-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "local.m3u" a))
          (write-region "" nil (expand-file-name "radio.m3u" b))
          (let* ((cj/music-m3u-roots (list a b))
                 (bases (mapcar #'car (cj/music--get-m3u-files))))
            (should (member "local.m3u" bases))
            (should (member "radio.m3u" bases))))
      (delete-directory a t)
      (delete-directory b t))))

(ert-deftest test-music-config-get-m3u-skips-missing-root ()
  "Error: a non-existent directory in the list is skipped, not fatal."
  (let ((a (make-temp-file "m3u-a-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "local.m3u" a))
          (let* ((cj/music-m3u-roots (list a "/no/such/dir/here"))
                 (bases (mapcar #'car (cj/music--get-m3u-files))))
            (should (equal bases '("local.m3u")))))
      (delete-directory a t))))

(ert-deftest test-music-config-get-m3u-collision-first-root-wins ()
  "Boundary: same basename in two roots resolves to the earlier root's file."
  (let ((a (make-temp-file "m3u-a-" t))
        (b (make-temp-file "m3u-b-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "jazz.m3u" a))
          (write-region "" nil (expand-file-name "jazz.m3u" b))
          (let* ((cj/music-m3u-roots (list a b))
                 (pair (assoc "jazz.m3u" (cj/music--get-m3u-files))))
            (should (string-prefix-p a (cdr pair)))))
      (delete-directory a t)
      (delete-directory b t))))

(provide 'test-music-config--m3u-roots)
;;; test-music-config--m3u-roots.el ends here
