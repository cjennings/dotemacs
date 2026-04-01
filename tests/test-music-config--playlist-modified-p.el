;;; test-music-config--playlist-modified-p.el --- Tests for playlist modification detection -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--playlist-modified-p function.
;; Tests the logic that compares the current EMMS playlist buffer
;; against its associated M3U file to detect unsaved changes.
;;
;; Test organization:
;; - Normal Cases: Matching/differing track lists
;; - Boundary Cases: Empty playlists, nil file, ordering
;; - Error Cases: Missing M3U file, missing buffer
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

(defun test-modified--setup-playlist-buffer (track-names)
  "Create an EMMS playlist buffer with TRACK-NAMES and return it.
Each entry in TRACK-NAMES becomes a file track in the playlist."
  (let ((buf (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (name track-names)
          (emms-playlist-insert-track (emms-track 'file name)))))
    (setq emms-playlist-buffer buf)
    buf))

(defun test-modified--teardown ()
  "Clean up test playlist buffer."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (kill-buffer buf)))

;;; Normal Cases

(ert-deftest test-music-config--playlist-modified-p-normal-identical-tracks-returns-nil ()
  "Identical track lists in file and playlist returns nil (not modified)."
  (unwind-protect
      (let* ((tracks '("/music/a.mp3" "/music/b.mp3" "/music/c.mp3"))
             (cj/music-playlist-file "/fake/playlist.m3u"))
        (test-modified--setup-playlist-buffer tracks)
        (cl-letf (((symbol-function 'cj/music--m3u-file-tracks)
                   (lambda (_file) tracks)))
          (should-not (cj/music--playlist-modified-p))))
    (test-modified--teardown)))

(ert-deftest test-music-config--playlist-modified-p-normal-different-tracks-returns-non-nil ()
  "Different track lists returns non-nil (modified)."
  (unwind-protect
      (let* ((file-tracks '("/music/a.mp3" "/music/b.mp3"))
             (buf-tracks '("/music/a.mp3" "/music/c.mp3"))
             (cj/music-playlist-file "/fake/playlist.m3u"))
        (test-modified--setup-playlist-buffer buf-tracks)
        (cl-letf (((symbol-function 'cj/music--m3u-file-tracks)
                   (lambda (_file) file-tracks)))
          (should (cj/music--playlist-modified-p))))
    (test-modified--teardown)))

(ert-deftest test-music-config--playlist-modified-p-normal-extra-track-returns-non-nil ()
  "Playlist with extra track compared to file returns non-nil."
  (unwind-protect
      (let* ((file-tracks '("/music/a.mp3" "/music/b.mp3"))
             (buf-tracks '("/music/a.mp3" "/music/b.mp3" "/music/c.mp3"))
             (cj/music-playlist-file "/fake/playlist.m3u"))
        (test-modified--setup-playlist-buffer buf-tracks)
        (cl-letf (((symbol-function 'cj/music--m3u-file-tracks)
                   (lambda (_file) file-tracks)))
          (should (cj/music--playlist-modified-p))))
    (test-modified--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--playlist-modified-p-boundary-no-playlist-file-returns-nil ()
  "Nil cj/music-playlist-file returns nil without comparing tracks."
  (unwind-protect
      (let ((cj/music-playlist-file nil))
        (test-modified--setup-playlist-buffer '("/music/a.mp3"))
        (should-not (cj/music--playlist-modified-p)))
    (test-modified--teardown)))

(ert-deftest test-music-config--playlist-modified-p-boundary-empty-file-and-playlist-returns-nil ()
  "Both empty file tracks and empty playlist returns nil."
  (unwind-protect
      (let ((cj/music-playlist-file "/fake/playlist.m3u"))
        (test-modified--setup-playlist-buffer '())
        (cl-letf (((symbol-function 'cj/music--m3u-file-tracks)
                   (lambda (_file) nil)))
          (should-not (cj/music--playlist-modified-p))))
    (test-modified--teardown)))

(ert-deftest test-music-config--playlist-modified-p-boundary-order-difference-returns-non-nil ()
  "Same tracks in different order returns non-nil."
  (unwind-protect
      (let* ((file-tracks '("/music/a.mp3" "/music/b.mp3"))
             (buf-tracks '("/music/b.mp3" "/music/a.mp3"))
             (cj/music-playlist-file "/fake/playlist.m3u"))
        (test-modified--setup-playlist-buffer buf-tracks)
        (cl-letf (((symbol-function 'cj/music--m3u-file-tracks)
                   (lambda (_file) file-tracks)))
          (should (cj/music--playlist-modified-p))))
    (test-modified--teardown)))

;;; Error Cases

(ert-deftest test-music-config--playlist-modified-p-error-missing-m3u-file-returns-nil ()
  "When M3U file doesn't exist, m3u-file-tracks returns nil; empty playlist matches."
  (unwind-protect
      (let ((cj/music-playlist-file "/nonexistent/playlist.m3u"))
        (test-modified--setup-playlist-buffer '())
        ;; cj/music--m3u-file-tracks returns nil for nonexistent files
        ;; empty playlist also returns nil, so equal = not modified
        (should-not (cj/music--playlist-modified-p)))
    (test-modified--teardown)))

(ert-deftest test-music-config--playlist-modified-p-error-missing-m3u-with-tracks-returns-non-nil ()
  "When M3U file doesn't exist but playlist has tracks, returns non-nil."
  (unwind-protect
      (let ((cj/music-playlist-file "/nonexistent/playlist.m3u"))
        (test-modified--setup-playlist-buffer '("/music/a.mp3"))
        ;; m3u-file-tracks returns nil, but playlist has tracks = modified
        (should (cj/music--playlist-modified-p)))
    (test-modified--teardown)))

(provide 'test-music-config--playlist-modified-p)
;;; test-music-config--playlist-modified-p.el ends here
