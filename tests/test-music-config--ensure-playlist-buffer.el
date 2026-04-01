;;; test-music-config--ensure-playlist-buffer.el --- Tests for playlist buffer creation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--ensure-playlist-buffer function.
;; Tests buffer creation, mode setup, and global variable assignment.
;;
;; Test organization:
;; - Normal Cases: Creates new buffer, returns existing buffer
;; - Boundary Cases: Wrong mode gets corrected, sets global variable
;; - Error Cases: Killed buffer gets recreated, returns live buffer
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

(defun test-ensure--teardown ()
  "Clean up test playlist buffer."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (kill-buffer buf)))

;;; Normal Cases

(ert-deftest test-music-config--ensure-playlist-buffer-normal-creates-buffer-when-none-exists ()
  "Creates a new playlist buffer when none exists."
  (unwind-protect
      (progn
        ;; Kill any existing buffer
        (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
          (kill-buffer buf))
        (let ((result (cj/music--ensure-playlist-buffer)))
          (should (bufferp result))
          (should (buffer-live-p result))
          (should (equal (buffer-name result) cj/music-playlist-buffer-name))))
    (test-ensure--teardown)))

(ert-deftest test-music-config--ensure-playlist-buffer-normal-returns-existing-buffer ()
  "Returns existing buffer without recreating it."
  (unwind-protect
      (let* ((buf1 (cj/music--ensure-playlist-buffer))
             (buf2 (cj/music--ensure-playlist-buffer)))
        (should (eq buf1 buf2)))
    (test-ensure--teardown)))

;;; Boundary Cases

(ert-deftest test-music-config--ensure-playlist-buffer-boundary-wrong-mode-corrected ()
  "Buffer in wrong mode gets corrected to emms-playlist-mode."
  (unwind-protect
      (progn
        ;; Create buffer in fundamental-mode
        (let ((buf (get-buffer-create cj/music-playlist-buffer-name)))
          (with-current-buffer buf
            (fundamental-mode)))
        (let ((result (cj/music--ensure-playlist-buffer)))
          (with-current-buffer result
            (should (eq major-mode 'emms-playlist-mode)))))
    (test-ensure--teardown)))

(ert-deftest test-music-config--ensure-playlist-buffer-boundary-sets-global-variable ()
  "Sets emms-playlist-buffer to the returned buffer."
  (unwind-protect
      (let ((result (cj/music--ensure-playlist-buffer)))
        (should (eq emms-playlist-buffer result)))
    (test-ensure--teardown)))

(ert-deftest test-music-config--ensure-playlist-buffer-boundary-sets-buffer-local-flag ()
  "Sets emms-playlist-buffer-p to t in the buffer."
  (unwind-protect
      (let ((result (cj/music--ensure-playlist-buffer)))
        (with-current-buffer result
          (should (eq emms-playlist-buffer-p t))))
    (test-ensure--teardown)))

;;; Error Cases

(ert-deftest test-music-config--ensure-playlist-buffer-error-killed-buffer-recreated ()
  "After buffer is killed, calling again creates a new live buffer."
  (unwind-protect
      (progn
        (let ((buf1 (cj/music--ensure-playlist-buffer)))
          (kill-buffer buf1))
        (let ((buf2 (cj/music--ensure-playlist-buffer)))
          (should (buffer-live-p buf2))
          (should (equal (buffer-name buf2) cj/music-playlist-buffer-name))))
    (test-ensure--teardown)))

(provide 'test-music-config--ensure-playlist-buffer)
;;; test-music-config--ensure-playlist-buffer.el ends here
