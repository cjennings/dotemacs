;;; test-music-config--assert-valid-playlist-file.el --- Tests for playlist file validation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--assert-valid-playlist-file function.
;; Tests the validation guard that ensures a playlist buffer has
;; a valid, existing associated M3U file before operations proceed.
;;
;; Test organization:
;; - Normal Cases: Valid file passes without error
;; - Boundary Cases: Empty file, file exists but is empty
;; - Error Cases: Nil file, nonexistent file, deleted file
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

(defun test-assert--setup ()
  "Create test base dir and ensure playlist buffer exists."
  (cj/create-test-base-dir)
  (let ((buf (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t))
    (setq emms-playlist-buffer buf)
    buf))

(defun test-assert--teardown ()
  "Clean up test playlist buffer and temp files."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (setq cj/music-playlist-file nil))
    (kill-buffer buf))
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config--assert-valid-playlist-file-normal-existing-file-passes ()
  "Valid existing file does not signal error."
  (test-assert--setup)
  (unwind-protect
      (let ((file (cj/create-temp-test-file "playlist-")))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file file))
        ;; Should not error
        (cj/music--assert-valid-playlist-file))
    (test-assert--teardown)))

(ert-deftest test-music-config--assert-valid-playlist-file-normal-file-with-content-passes ()
  "Valid file with content does not signal error."
  (test-assert--setup)
  (unwind-protect
      (let ((file (cj/create-temp-test-file-with-content "track1.mp3\ntrack2.mp3\n" "playlist-")))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file file))
        (cj/music--assert-valid-playlist-file))
    (test-assert--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--assert-valid-playlist-file-boundary-empty-file-passes ()
  "Empty but existing file does not signal error."
  (test-assert--setup)
  (unwind-protect
      (let ((file (cj/create-temp-test-file "playlist-")))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file file))
        ;; File exists but is empty - should still pass
        (cj/music--assert-valid-playlist-file))
    (test-assert--teardown)))

;;; Error Cases

(ert-deftest test-music-config--assert-valid-playlist-file-error-nil-file-signals-user-error ()
  "Nil cj/music-playlist-file signals user-error."
  (test-assert--setup)
  (unwind-protect
      (progn
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file nil))
        (should-error (cj/music--assert-valid-playlist-file)
                      :type 'user-error))
    (test-assert--teardown)))

(ert-deftest test-music-config--assert-valid-playlist-file-error-nonexistent-file-signals-user-error ()
  "Nonexistent file path signals user-error."
  (test-assert--setup)
  (unwind-protect
      (progn
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file "/nonexistent/path/to/playlist.m3u"))
        (should-error (cj/music--assert-valid-playlist-file)
                      :type 'user-error))
    (test-assert--teardown)))

(ert-deftest test-music-config--assert-valid-playlist-file-error-deleted-file-signals-user-error ()
  "File that existed but was deleted signals user-error."
  (test-assert--setup)
  (unwind-protect
      (let ((file (cj/create-temp-test-file "playlist-")))
        (with-current-buffer (get-buffer cj/music-playlist-buffer-name)
          (setq cj/music-playlist-file file))
        ;; Verify it passes first
        (cj/music--assert-valid-playlist-file)
        ;; Now delete the file
        (delete-file file)
        ;; Should now signal error
        (should-error (cj/music--assert-valid-playlist-file)
                      :type 'user-error))
    (test-assert--teardown)))

(provide 'test-music-config--assert-valid-playlist-file)
;;; test-music-config--assert-valid-playlist-file.el ends here
