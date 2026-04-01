;;; test-music-config--header-text.el --- Tests for playlist header string generation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--header-text function.
;; Tests the multi-line header string builder for the playlist overlay.
;;
;; Test organization:
;; - Normal Cases: Playlist name from file, "Untitled" default, track count
;; - Boundary Cases: Stopped state, paused state, mode indicators
;; - Error Cases: Empty playlist buffer
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

(defun test-header--setup-playlist-buffer (track-names)
  "Create an EMMS playlist buffer with TRACK-NAMES and return it."
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

(defun test-header--teardown ()
  "Clean up test playlist buffer and reset EMMS state."
  (setq emms-player-playing-p nil)
  (setq emms-player-paused-p nil)
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (setq cj/music-playlist-file nil))
    (kill-buffer buf)))

(defun test-header--strip-properties (str)
  "Return STR with all text properties removed."
  (let ((s (copy-sequence str)))
    (set-text-properties 0 (length s) nil s)
    s))

;;; Normal Cases

(ert-deftest test-music-config--header-text-normal-shows-playlist-name-from-file ()
  "Header shows playlist name derived from cj/music-playlist-file."
  (unwind-protect
      (progn
        (test-header--setup-playlist-buffer '("/music/a.mp3"))
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file "/path/to/my-jazz.m3u"))
        (let* ((header (with-current-buffer cj/music-playlist-buffer-name
                         (cj/music--header-text)))
               (plain (test-header--strip-properties header)))
          (should (string-match-p "my-jazz" plain))))
    (test-header--teardown)))

(ert-deftest test-music-config--header-text-normal-shows-untitled-when-no-file ()
  "Header shows 'Untitled' when no playlist file is associated."
  (unwind-protect
      (progn
        (test-header--setup-playlist-buffer '("/music/a.mp3"))
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file nil))
        (let* ((header (with-current-buffer cj/music-playlist-buffer-name
                         (cj/music--header-text)))
               (plain (test-header--strip-properties header)))
          (should (string-match-p "Untitled" plain))))
    (test-header--teardown)))

(ert-deftest test-music-config--header-text-normal-shows-track-count ()
  "Header shows correct track count."
  (unwind-protect
      (progn
        (test-header--setup-playlist-buffer '("/music/a.mp3" "/music/b.mp3" "/music/c.mp3"))
        (let* ((header (with-current-buffer cj/music-playlist-buffer-name
                         (cj/music--header-text)))
               (plain (test-header--strip-properties header)))
          (should (string-match-p "(3)" plain))))
    (test-header--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--header-text-boundary-stopped-state ()
  "Header shows 'Stopped' when not playing."
  (unwind-protect
      (progn
        (test-header--setup-playlist-buffer '("/music/a.mp3"))
        (setq emms-player-playing-p nil)
        (let* ((header (with-current-buffer cj/music-playlist-buffer-name
                         (cj/music--header-text)))
               (plain (test-header--strip-properties header)))
          (should (string-match-p "Stopped" plain))))
    (test-header--teardown)))

(ert-deftest test-music-config--header-text-boundary-paused-state ()
  "Header shows 'Paused' when player is paused."
  (unwind-protect
      (progn
        (test-header--setup-playlist-buffer '("/music/a.mp3"))
        (setq emms-player-playing-p t)
        (setq emms-player-paused-p t)
        (let* ((header (with-current-buffer cj/music-playlist-buffer-name
                         (cj/music--header-text)))
               (plain (test-header--strip-properties header)))
          (should (string-match-p "Paused" plain))))
    (test-header--teardown)))

(ert-deftest test-music-config--header-text-boundary-contains-mode-labels ()
  "Header contains mode indicator labels for repeat, single, random, consume."
  (unwind-protect
      (progn
        (test-header--setup-playlist-buffer '("/music/a.mp3"))
        (let* ((header (with-current-buffer cj/music-playlist-buffer-name
                         (cj/music--header-text)))
               (plain (test-header--strip-properties header)))
          (should (string-match-p "repeat" plain))
          (should (string-match-p "single" plain))
          (should (string-match-p "random" plain))
          (should (string-match-p "consume" plain))))
    (test-header--teardown)))

;;; Error Cases

(ert-deftest test-music-config--header-text-error-empty-playlist-shows-zero-count ()
  "Header shows (0) for empty playlist."
  (unwind-protect
      (progn
        (test-header--setup-playlist-buffer '())
        (let* ((header (with-current-buffer cj/music-playlist-buffer-name
                         (cj/music--header-text)))
               (plain (test-header--strip-properties header)))
          (should (string-match-p "(0)" plain))))
    (test-header--teardown)))

(provide 'test-music-config--header-text)
;;; test-music-config--header-text.el ends here
