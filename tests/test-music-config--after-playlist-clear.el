;;; test-music-config--after-playlist-clear.el --- Tests for playlist clear advice -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--after-playlist-clear function.
;; Tests the advice function that resets cj/music-playlist-file
;; when the EMMS playlist is cleared.
;;
;; Test organization:
;; - Normal Cases: Clears file variable, noop when no buffer
;; - Boundary Cases: Already nil stays nil, idempotent on multiple calls
;; - Error Cases: Killed buffer doesn't crash
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

(defun test-after-clear--setup ()
  "Create playlist buffer with a playlist file set."
  (let ((buf (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (emms-playlist-mode)
      (setq emms-playlist-buffer-p t))
    (setq emms-playlist-buffer buf)
    buf))

(defun test-after-clear--teardown ()
  "Clean up test playlist buffer."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (setq cj/music-playlist-file nil))
    (kill-buffer buf)))

;;; Normal Cases

(ert-deftest test-music-config--after-playlist-clear-normal-clears-file-variable ()
  "Calling after-playlist-clear sets cj/music-playlist-file to nil."
  (unwind-protect
      (progn
        (test-after-clear--setup)
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file "/path/to/playlist.m3u"))
        (cj/music--after-playlist-clear)
        (with-current-buffer cj/music-playlist-buffer-name
          (should-not cj/music-playlist-file)))
    (test-after-clear--teardown)))

(ert-deftest test-music-config--after-playlist-clear-normal-noop-when-no-buffer ()
  "Does nothing when playlist buffer doesn't exist."
  (unwind-protect
      (progn
        ;; Ensure no buffer exists
        (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
          (kill-buffer buf))
        ;; Should not error
        (cj/music--after-playlist-clear))
    (test-after-clear--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--after-playlist-clear-boundary-already-nil-stays-nil ()
  "Already-nil playlist file remains nil after clear."
  (unwind-protect
      (progn
        (test-after-clear--setup)
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file nil))
        (cj/music--after-playlist-clear)
        (with-current-buffer cj/music-playlist-buffer-name
          (should-not cj/music-playlist-file)))
    (test-after-clear--teardown)))

(ert-deftest test-music-config--after-playlist-clear-boundary-multiple-calls-idempotent ()
  "Multiple calls produce same result as single call."
  (unwind-protect
      (progn
        (test-after-clear--setup)
        (with-current-buffer cj/music-playlist-buffer-name
          (setq cj/music-playlist-file "/path/to/playlist.m3u"))
        (cj/music--after-playlist-clear)
        (cj/music--after-playlist-clear)
        (cj/music--after-playlist-clear)
        (with-current-buffer cj/music-playlist-buffer-name
          (should-not cj/music-playlist-file)))
    (test-after-clear--teardown)))

;;; Error Cases

(ert-deftest test-music-config--after-playlist-clear-error-killed-buffer-no-crash ()
  "Killed buffer doesn't cause an error."
  (let ((buf (test-after-clear--setup)))
    (kill-buffer buf)
    ;; Should not error when buffer is gone
    (should-not (condition-case err
                    (progn (cj/music--after-playlist-clear) nil)
                  (error err)))))

(provide 'test-music-config--after-playlist-clear)
;;; test-music-config--after-playlist-clear.el ends here
