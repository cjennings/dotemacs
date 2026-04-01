;;; test-music-config--playlist-tracks.el --- Tests for playlist track extraction -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--playlist-tracks function.
;; Tests reading track names from the current EMMS playlist buffer.
;;
;; Test organization:
;; - Normal Cases: Single and multiple tracks
;; - Boundary Cases: Empty playlist, order preservation
;; - Error Cases: No playlist buffer, tracks without names
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

(defun test-tracks--setup-playlist-buffer (track-names)
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

(defun test-tracks--teardown ()
  "Clean up test playlist buffer."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (kill-buffer buf)))

;;; Normal Cases

(ert-deftest test-music-config--playlist-tracks-normal-multiple-tracks-returns-list ()
  "Multiple tracks in playlist returns list of track names."
  (unwind-protect
      (let ((tracks '("/music/a.mp3" "/music/b.mp3" "/music/c.mp3")))
        (test-tracks--setup-playlist-buffer tracks)
        (should (equal (cj/music--playlist-tracks) tracks)))
    (test-tracks--teardown)))

(ert-deftest test-music-config--playlist-tracks-normal-single-track-returns-list ()
  "Single track in playlist returns single-element list."
  (unwind-protect
      (let ((tracks '("/music/only.mp3")))
        (test-tracks--setup-playlist-buffer tracks)
        (should (equal (cj/music--playlist-tracks) tracks)))
    (test-tracks--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--playlist-tracks-boundary-empty-playlist-returns-nil ()
  "Empty playlist returns nil (empty list)."
  (unwind-protect
      (progn
        (test-tracks--setup-playlist-buffer '())
        (should-not (cj/music--playlist-tracks)))
    (test-tracks--teardown)))

(ert-deftest test-music-config--playlist-tracks-boundary-preserves-insertion-order ()
  "Track order matches insertion order."
  (unwind-protect
      (let ((tracks '("/music/z.mp3" "/music/a.mp3" "/music/m.mp3")))
        (test-tracks--setup-playlist-buffer tracks)
        (should (equal (cj/music--playlist-tracks) tracks)))
    (test-tracks--teardown)))

(ert-deftest test-music-config--playlist-tracks-boundary-url-tracks-included ()
  "URL tracks are included alongside file tracks."
  (unwind-protect
      (let* ((buf (get-buffer-create cj/music-playlist-buffer-name)))
        (with-current-buffer buf
          (emms-playlist-mode)
          (setq emms-playlist-buffer-p t)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (emms-playlist-insert-track (emms-track 'file "/music/local.mp3"))
            (emms-playlist-insert-track (emms-track 'url "http://stream.example.com/radio"))))
        (setq emms-playlist-buffer buf)
        (let ((result (cj/music--playlist-tracks)))
          (should (= 2 (length result)))
          (should (equal (nth 0 result) "/music/local.mp3"))
          (should (equal (nth 1 result) "http://stream.example.com/radio"))))
    (test-tracks--teardown)))

;;; Error Cases

(ert-deftest test-music-config--playlist-tracks-error-no-buffer-creates-one ()
  "When playlist buffer doesn't exist, ensure-playlist-buffer creates it."
  (unwind-protect
      (progn
        ;; Kill any existing buffer
        (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
          (kill-buffer buf))
        ;; Should create buffer and return empty list
        (should-not (cj/music--playlist-tracks))
        ;; Buffer should now exist
        (should (get-buffer cj/music-playlist-buffer-name)))
    (test-tracks--teardown)))

(provide 'test-music-config--playlist-tracks)
;;; test-music-config--playlist-tracks.el ends here
