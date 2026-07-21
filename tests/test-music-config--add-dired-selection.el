;;; test-music-config--add-dired-selection.el --- Tests for dired add command -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music-add-dired-selection.
;;
;; Test organization:
;; - Normal Cases: marked files (no region) are all queued
;; - Boundary Cases: no marks falls back to the file at point
;; - Error Cases: outside dired signals a user-error
;;
;;; Code:

(require 'ert)
(require 'cl-lib)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Add EMMS elpa directory to load path for batch testing
(let ((emms-dir (car (file-expand-wildcards
                      (expand-file-name "elpa/emms-*" user-emacs-directory)))))
  (when emms-dir
    (add-to-list 'load-path emms-dir)))

(require 'emms)
(require 'music-config)

(defun test-add-dired--run (marked)
  "Run the command with MARKED as dired's marked-file answer; return added files."
  (let (added)
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
              ((symbol-function 'cj/music--ensure-playlist-buffer) (lambda () nil))
              ((symbol-function 'dired-get-marked-files)
               (lambda (&rest _) marked))
              ((symbol-function 'file-directory-p) (lambda (_f) nil))
              ((symbol-function 'cj/music--valid-file-p) (lambda (_f) t))
              ((symbol-function 'emms-add-file) (lambda (f) (push f added)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (cj/music-add-dired-selection)
      (nreverse added))))

(ert-deftest test-music-add-dired-selection-queues-all-marked-files ()
  "Normal: files marked with m (no region) are all queued, not just point.
The old gate ran dired-get-marked-files only under use-region-p, so marks
without a region fell to the single-file branch and silently dropped all
but the point file."
  (should (equal (test-add-dired--run '("/tmp/a.mp3" "/tmp/b.mp3" "/tmp/c.mp3"))
                 '("/tmp/a.mp3" "/tmp/b.mp3" "/tmp/c.mp3"))))

(ert-deftest test-music-add-dired-selection-point-file-when-no-marks ()
  "Boundary: with no marks, dired-get-marked-files returns the point file."
  (should (equal (test-add-dired--run '("/tmp/only.mp3"))
                 '("/tmp/only.mp3"))))

(ert-deftest test-music-add-dired-selection-errors-outside-dired ()
  "Error: outside a dired buffer the command signals a user-error."
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) nil)))
    (should-error (cj/music-add-dired-selection) :type 'user-error)))

(provide 'test-music-config--add-dired-selection)
;;; test-music-config--add-dired-selection.el ends here
