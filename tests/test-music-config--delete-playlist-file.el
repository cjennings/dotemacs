;;; test-music-config--delete-playlist-file.el --- Tests for playlist file deletion -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--delete-playlist-file function.
;; Tests the internal helper that removes a playlist's .m3u file,
;; clears the playlist buffer's file association when it pointed at
;; the deleted file, and refreshes the radio metadata cache.
;;
;; Test organization:
;; - Normal Cases: Existing file is deleted
;; - Boundary Cases: Association cleared only when it matches; cache refresh
;; - Error Cases: Nil path, nonexistent path
;;
;;; Code:

(require 'ert)
(require 'testutil-general)

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

(defun test-delete-playlist--setup ()
  "Create test base dir and ensure playlist buffer exists."
  (cj/create-test-base-dir)
  (let ((buf (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t))
    (setq emms-playlist-buffer buf)
    buf))

(defun test-delete-playlist--teardown ()
  "Clean up test playlist buffer and temp files."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (setq cj/music-playlist-file nil))
    (kill-buffer buf))
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--delete-playlist-file-normal-removes-file ()
  "Normal: an existing playlist file is deleted from disk."
  (test-delete-playlist--setup)
  (unwind-protect
      (let ((file (cj/create-temp-test-file-with-content
                   "#EXTM3U\n/tmp/song.mp3\n" "playlist.m3u")))
        (cj/music--delete-playlist-file file)
        (should-not (file-exists-p file)))
    (test-delete-playlist--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--delete-playlist-file-boundary-clears-matching-association ()
  "Boundary: deleting the associated playlist file clears the association."
  (test-delete-playlist--setup)
  (unwind-protect
      (let ((file (cj/create-temp-test-file-with-content
                   "#EXTM3U\n" "playlist.m3u")))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file file))
        (cj/music--delete-playlist-file file)
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (should-not cj/music-playlist-file)))
    (test-delete-playlist--teardown)))

(ert-deftest test-music-config--delete-playlist-file-boundary-keeps-other-association ()
  "Boundary: deleting a different file leaves the association untouched."
  (test-delete-playlist--setup)
  (unwind-protect
      (let ((doomed (cj/create-temp-test-file-with-content
                     "#EXTM3U\n" "doomed.m3u"))
            (kept (cj/create-temp-test-file-with-content
                   "#EXTM3U\n" "kept.m3u")))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file kept))
        (cj/music--delete-playlist-file doomed)
        (should (file-exists-p kept))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (should (equal cj/music-playlist-file kept))))
    (test-delete-playlist--teardown)))

(ert-deftest test-music-config--delete-playlist-file-boundary-refreshes-radio-cache ()
  "Boundary: deletion clears the cached radio metadata."
  (test-delete-playlist--setup)
  (unwind-protect
      (let ((file (cj/create-temp-test-file-with-content
                   "#EXTM3U\n" "playlist.m3u")))
        (setq cj/music--radio-metadata-cache '(("stale" . "entry")))
        (cj/music--delete-playlist-file file)
        (should-not cj/music--radio-metadata-cache))
    (test-delete-playlist--teardown)))

;;; Error Cases

(ert-deftest test-music-config--delete-playlist-file-error-nil-path ()
  "Error: nil path signals user-error."
  (test-delete-playlist--setup)
  (unwind-protect
      (should-error (cj/music--delete-playlist-file nil) :type 'user-error)
    (test-delete-playlist--teardown)))

(ert-deftest test-music-config--delete-playlist-file-error-nonexistent-path ()
  "Error: a path that does not exist signals user-error and touches nothing."
  (test-delete-playlist--setup)
  (unwind-protect
      (let ((kept (cj/create-temp-test-file-with-content
                   "#EXTM3U\n" "kept.m3u")))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file kept))
        (should-error
         (cj/music--delete-playlist-file
          (expand-file-name "no-such.m3u" cj/test-base-dir))
         :type 'user-error)
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (should (equal cj/music-playlist-file kept))))
    (test-delete-playlist--teardown)))

(provide 'test-music-config--delete-playlist-file)
;;; test-music-config--delete-playlist-file.el ends here
