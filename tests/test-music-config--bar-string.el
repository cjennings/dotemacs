;;; test-music-config--bar-string.el --- Tests for the block progress bar -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music--bar-string': render a WIDTH-cell block bar from a
;; filled-cell count (from `cj/music--bar-fill'), or an "on air" marker when the
;; fill is `indeterminate' (a live stream with no duration).  Face-carrying
;; text; the tests assert the rendered length and content, not the faces.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(ert-deftest test-music-config--bar-string-normal-half ()
  "Normal: 10 of 20 cells filled renders a 20-cell bar."
  (let ((s (substring-no-properties (cj/music--bar-string 10 20))))
    (should (= (length s) 20))
    (should (= (cl-count ?█ s) 10))
    (should (= (cl-count ?░ s) 10))))

(ert-deftest test-music-config--bar-string-boundary-empty ()
  "Boundary: zero fill is all empty cells."
  (let ((s (substring-no-properties (cj/music--bar-string 0 20))))
    (should (= (cl-count ?█ s) 0))
    (should (= (cl-count ?░ s) 20))))

(ert-deftest test-music-config--bar-string-boundary-full ()
  "Boundary: full fill is all filled cells."
  (let ((s (substring-no-properties (cj/music--bar-string 20 20))))
    (should (= (cl-count ?█ s) 20))
    (should (= (cl-count ?░ s) 0))))

(ert-deftest test-music-config--bar-string-indeterminate-on-air ()
  "Error/indeterminate: a stream renders an on-air marker, not a bar."
  (let ((s (substring-no-properties (cj/music--bar-string 'indeterminate 20))))
    (should (string-match-p "on air" s))
    (should (= (cl-count ?█ s) 0))))

(provide 'test-music-config--bar-string)
;;; test-music-config--bar-string.el ends here
