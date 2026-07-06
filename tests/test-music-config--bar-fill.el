;;; test-music-config--bar-fill.el --- Tests for progress-bar fill math -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for `cj/music--bar-fill': the pure computation of how many cells
;; of a WIDTH-cell progress bar are filled given ELAPSED and TOTAL seconds.
;; A stream (no duration) is indeterminate; the live elapsed source (mpv) is
;; wired in a later phase, so this helper only does the clamp-and-round math.
;;
;;; Code:

(require 'ert)

(defvar-keymap cj/custom-keymap :doc "Stub keymap for testing")

(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

;;; Normal

(ert-deftest test-music-config--bar-fill-normal-half ()
  "Normal: halfway through fills half the cells."
  (should (= (cj/music--bar-fill 30 60 20) 10)))

(ert-deftest test-music-config--bar-fill-normal-quarter ()
  "Normal: a quarter elapsed rounds to a quarter of the cells."
  (should (= (cj/music--bar-fill 15 60 20) 5)))

;;; Boundary

(ert-deftest test-music-config--bar-fill-boundary-zero-elapsed ()
  "Boundary: nothing elapsed fills no cells."
  (should (= (cj/music--bar-fill 0 60 20) 0)))

(ert-deftest test-music-config--bar-fill-boundary-full ()
  "Boundary: elapsed equal to total fills every cell."
  (should (= (cj/music--bar-fill 60 60 20) 20)))

(ert-deftest test-music-config--bar-fill-boundary-over-clamps ()
  "Boundary: elapsed past total clamps to full, never overflows."
  (should (= (cj/music--bar-fill 90 60 20) 20)))

(ert-deftest test-music-config--bar-fill-boundary-nil-elapsed ()
  "Boundary: an unknown elapsed (nil) fills no cells."
  (should (= (cj/music--bar-fill nil 60 20) 0)))

;;; Error / indeterminate

(ert-deftest test-music-config--bar-fill-indeterminate-nil-total ()
  "Error: a stream (nil total) is indeterminate, not a cell count."
  (should (eq (cj/music--bar-fill 30 nil 20) 'indeterminate)))

(ert-deftest test-music-config--bar-fill-indeterminate-zero-total ()
  "Error: a zero total is indeterminate rather than a divide-by-zero."
  (should (eq (cj/music--bar-fill 30 0 20) 'indeterminate)))

(provide 'test-music-config--bar-fill)
;;; test-music-config--bar-fill.el ends here
