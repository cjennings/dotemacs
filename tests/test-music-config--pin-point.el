;;; test-music-config--pin-point.el --- Tests for the playlist gutter cursor -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; The playlist cursor lives pinned at the start of the row (the number
;; gutter).  The rows are rendered track lines, not editable text, and
;; vertical motion over thumbnails and stretch-space drifts point to
;; arbitrary visual columns (usually line end).  A buffer-local
;; post-command snap enforces the model for every motion command.  The one
;; exception is an active isearch, which owns point placement until it ends.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'music-config)

;;; Normal Cases

(ert-deftest test-music-pin-point-snaps-mid-line-to-bol ()
  "Normal: point mid-row snaps back to the beginning of the line."
  (with-temp-buffer
    (insert "track one\ntrack two\n")
    (goto-char (point-min))
    (forward-char 5)
    (cj/music--pin-point-to-bol)
    (should (bolp))
    (should (= (point) (point-min)))))

(ert-deftest test-music-pin-point-noop-at-bol ()
  "Normal: point already at the row start stays put."
  (with-temp-buffer
    (insert "track one\ntrack two\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((before (point)))
      (cj/music--pin-point-to-bol)
      (should (= (point) before)))))

;;; Boundary Cases

(ert-deftest test-music-pin-point-skips-during-isearch ()
  "Boundary: an active isearch owns point; the pin defers until it ends."
  (with-temp-buffer
    (insert "track one\ntrack two\n")
    (goto-char (point-min))
    (forward-char 5)
    (let ((isearch-mode t))
      (cj/music--pin-point-to-bol))
    (should-not (bolp))))

(ert-deftest test-music-pin-point-empty-buffer-no-error ()
  "Boundary: an empty buffer is a no-op, no error."
  (with-temp-buffer
    (should-not (cj/music--pin-point-to-bol))
    (should (bolp))))

;;; Hook wiring

(ert-deftest test-music-pin-point-ensure-wires-post-command-hook ()
  "Normal: the playlist buffer gets the pin on its buffer-local
post-command-hook."
  (let (created)
    (cl-letf (((symbol-function 'emms-playlist-mode) #'ignore))
      (unwind-protect
          (progn
            (setq created (cj/music--ensure-playlist-buffer))
            (with-current-buffer created
              (should (member #'cj/music--pin-point-to-bol post-command-hook))))
        (when (buffer-live-p created) (kill-buffer created))))))

(provide 'test-music-config--pin-point)
;;; test-music-config--pin-point.el ends here
