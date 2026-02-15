;;; test-music-config--format-duration.el --- Tests for duration formatting -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music--format-duration function.
;; Tests the pure helper that converts seconds to "M:SS" display strings.
;;
;; Test organization:
;; - Normal Cases: Typical durations, exact minutes, seconds only
;; - Boundary Cases: Zero, one second, large values, float input
;; - Error Cases: Nil, negative, non-numeric input
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

;;; Normal Cases

(ert-deftest test-music-config--format-duration-normal-typical-song ()
  "Validate typical 3:45 song duration."
  (should (string= (cj/music--format-duration 225) "3:45")))

(ert-deftest test-music-config--format-duration-normal-exact-minutes ()
  "Validate exact minute boundary produces M:00."
  (should (string= (cj/music--format-duration 180) "3:00")))

(ert-deftest test-music-config--format-duration-normal-seconds-only ()
  "Validate sub-minute duration produces 0:SS."
  (should (string= (cj/music--format-duration 45) "0:45")))

(ert-deftest test-music-config--format-duration-normal-single-digit-seconds ()
  "Validate seconds are zero-padded to two digits."
  (should (string= (cj/music--format-duration 62) "1:02")))

(ert-deftest test-music-config--format-duration-normal-long-track ()
  "Validate duration over 10 minutes."
  (should (string= (cj/music--format-duration 622) "10:22")))

;;; Boundary Cases

(ert-deftest test-music-config--format-duration-boundary-one-second ()
  "Validate minimum positive duration."
  (should (string= (cj/music--format-duration 1) "0:01")))

(ert-deftest test-music-config--format-duration-boundary-zero-returns-nil ()
  "Validate zero seconds returns nil (no useful duration)."
  (should (null (cj/music--format-duration 0))))

(ert-deftest test-music-config--format-duration-boundary-very-large ()
  "Validate very long duration (over an hour) formats correctly."
  (should (string= (cj/music--format-duration 3661) "61:01")))

(ert-deftest test-music-config--format-duration-boundary-float-input ()
  "Validate float seconds are truncated by integer division."
  (should (string= (cj/music--format-duration 125.7) "2:05")))

(ert-deftest test-music-config--format-duration-boundary-59-seconds ()
  "Validate max seconds before minute rollover."
  (should (string= (cj/music--format-duration 59) "0:59")))

;;; Error Cases

(ert-deftest test-music-config--format-duration-error-nil-returns-nil ()
  "Validate nil input returns nil."
  (should (null (cj/music--format-duration nil))))

(ert-deftest test-music-config--format-duration-error-negative-returns-nil ()
  "Validate negative input returns nil."
  (should (null (cj/music--format-duration -5))))

(ert-deftest test-music-config--format-duration-error-string-returns-nil ()
  "Validate non-numeric input returns nil."
  (should (null (cj/music--format-duration "3:45"))))

(provide 'test-music-config--format-duration)
;;; test-music-config--format-duration.el ends here
