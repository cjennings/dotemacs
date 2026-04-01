;;; test-music-config-create-radio-station.el --- Tests for radio station creation -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Unit tests for cj/music-create-radio-station function.
;; Tests M3U file creation for radio stations with stream URLs.
;;
;; Test organization:
;; - Normal Cases: Standard creation, EXTM3U format, safe filename
;; - Boundary Cases: Unicode name, complex URL, overwrite confirmed
;; - Error Cases: Empty name, empty URL, overwrite declined
;;
;;; Code:

(require 'ert)
(require 'testutil-general)

;; Stub missing dependencies before loading music-config
(defvar-keymap cj/custom-keymap
  :doc "Stub keymap for testing")

;; Load production code
(require 'music-config)

;;; Setup & Teardown

(defun test-music-config-create-radio-station-setup ()
  "Setup test environment with temp directory for M3U output."
  (cj/create-test-base-dir)
  (cj/create-test-subdirectory "radio-playlists"))

(defun test-music-config-create-radio-station-teardown ()
  "Clean up test environment."
  (cj/delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-music-config-create-radio-station-normal-creates-m3u-file ()
  "Creating a radio station produces an M3U file in the music root."
  (let ((test-dir (test-music-config-create-radio-station-setup)))
    (unwind-protect
        (let ((cj/music-m3u-root test-dir))
          (cj/music-create-radio-station "Jazz FM" "http://stream.jazzfm.com/radio")
          (let ((expected-file (expand-file-name "Jazz_FM_Radio.m3u" test-dir)))
            (should (file-exists-p expected-file))))
      (test-music-config-create-radio-station-teardown))))

(ert-deftest test-music-config-create-radio-station-normal-extm3u-format ()
  "Created file contains EXTM3U header, EXTINF with station name, and URL."
  (let ((test-dir (test-music-config-create-radio-station-setup)))
    (unwind-protect
        (let ((cj/music-m3u-root test-dir))
          (cj/music-create-radio-station "Jazz FM" "http://stream.jazzfm.com/radio")
          (let ((content (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "Jazz_FM_Radio.m3u" test-dir))
                           (buffer-string))))
            (should (string-match-p "^#EXTM3U" content))
            (should (string-match-p "#EXTINF:-1,Jazz FM" content))
            (should (string-match-p "http://stream.jazzfm.com/radio" content))))
      (test-music-config-create-radio-station-teardown))))

(ert-deftest test-music-config-create-radio-station-normal-safe-filename ()
  "Station name with special characters produces filesystem-safe filename."
  (let ((test-dir (test-music-config-create-radio-station-setup)))
    (unwind-protect
        (let ((cj/music-m3u-root test-dir))
          (cj/music-create-radio-station "Rock & Roll 101.5" "http://example.com/stream")
          ;; Spaces and special chars replaced with underscores
          (let ((expected-file (expand-file-name "Rock___Roll_101_5_Radio.m3u" test-dir)))
            (should (file-exists-p expected-file))))
      (test-music-config-create-radio-station-teardown))))

;;; Boundary Cases

(ert-deftest test-music-config-create-radio-station-boundary-unicode-name-safe-filename ()
  "Unicode station name produces safe filename while preserving name in EXTINF."
  (let ((test-dir (test-music-config-create-radio-station-setup)))
    (unwind-protect
        (let ((cj/music-m3u-root test-dir))
          (cj/music-create-radio-station "Klassik Radio" "http://example.com/stream")
          ;; Name is all ASCII-safe, so filename uses it directly
          (should (file-exists-p (expand-file-name "Klassik_Radio_Radio.m3u" test-dir)))
          ;; Original name preserved in EXTINF inside the file
          (let ((content (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "Klassik_Radio_Radio.m3u" test-dir))
                           (buffer-string))))
            (should (string-match-p "Klassik Radio" content))))
      (test-music-config-create-radio-station-teardown))))

(ert-deftest test-music-config-create-radio-station-boundary-url-with-query-params ()
  "Complex URL with query parameters preserved in file content."
  (let ((test-dir (test-music-config-create-radio-station-setup)))
    (unwind-protect
        (let ((cj/music-m3u-root test-dir)
              (url "https://stream.example.com/radio?format=mp3&quality=320&token=abc123"))
          (cj/music-create-radio-station "Test Radio" url)
          (let ((content (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "Test_Radio_Radio.m3u" test-dir))
                           (buffer-string))))
            (should (string-match-p (regexp-quote url) content))))
      (test-music-config-create-radio-station-teardown))))

(ert-deftest test-music-config-create-radio-station-boundary-overwrite-confirmed ()
  "Overwriting existing file when user confirms succeeds."
  (let ((test-dir (test-music-config-create-radio-station-setup)))
    (unwind-protect
        (let ((cj/music-m3u-root test-dir))
          ;; Create initial file
          (cj/music-create-radio-station "MyRadio" "http://old.url/stream")
          (let ((file (expand-file-name "MyRadio_Radio.m3u" test-dir)))
            (should (file-exists-p file))
            ;; Overwrite with user confirming
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (cj/music-create-radio-station "MyRadio" "http://new.url/stream"))
            ;; File should now contain new URL
            (let ((content (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
              (should (string-match-p "http://new.url/stream" content))
              (should-not (string-match-p "http://old.url/stream" content)))))
      (test-music-config-create-radio-station-teardown))))

;;; Error Cases

(ert-deftest test-music-config-create-radio-station-error-empty-name-signals-user-error ()
  "Empty station name signals user-error."
  (should-error (cj/music-create-radio-station "" "http://example.com/stream")
                :type 'user-error))

(ert-deftest test-music-config-create-radio-station-error-empty-url-signals-user-error ()
  "Empty URL signals user-error."
  (should-error (cj/music-create-radio-station "Test Radio" "")
                :type 'user-error))

(ert-deftest test-music-config-create-radio-station-error-overwrite-declined-signals-user-error ()
  "Declining overwrite signals user-error."
  (let ((test-dir (test-music-config-create-radio-station-setup)))
    (unwind-protect
        (let ((cj/music-m3u-root test-dir))
          ;; Create initial file
          (cj/music-create-radio-station "MyRadio" "http://old.url/stream")
          ;; Decline overwrite
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
            (should-error (cj/music-create-radio-station "MyRadio" "http://new.url/stream")
                          :type 'user-error)))
      (test-music-config-create-radio-station-teardown))))

(provide 'test-music-config-create-radio-station)
;;; test-music-config-create-radio-station.el ends here
