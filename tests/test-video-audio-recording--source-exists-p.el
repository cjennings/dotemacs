;;; test-video-audio-recording--source-exists-p.el --- Tests for cj/recording--source-exists-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--source-exists-p function.
;; Tests checking whether a PulseAudio source exists in pactl output.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Test Fixtures Helper

(defun test-load-fixture (filename)
  "Load fixture file FILENAME from tests/fixtures directory."
  (let ((fixture-path (expand-file-name
                       (concat "tests/fixtures/" filename)
                       user-emacs-directory)))
    (with-temp-buffer
      (insert-file-contents fixture-path)
      (buffer-string))))

;;; Normal Cases

(ert-deftest test-source-exists-p-normal-existing-device-returns-t ()
  "Test that an existing device returns non-nil."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (should (cj/recording--source-exists-p
             "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor" output))))

(ert-deftest test-source-exists-p-normal-input-device-returns-t ()
  "Test that an existing input device returns non-nil."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (should (cj/recording--source-exists-p
             "alsa_input.pci-0000_00_1f.3.analog-stereo" output))))

(ert-deftest test-source-exists-p-normal-bluetooth-device-returns-t ()
  "Test that a Bluetooth device returns non-nil."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (should (cj/recording--source-exists-p
             "bluez_input.00:1B:66:C0:91:6D" output))))

;;; Boundary Cases

(ert-deftest test-source-exists-p-boundary-nonexistent-device-returns-nil ()
  "Test that a non-existent device returns nil."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (should-not (cj/recording--source-exists-p
                 "nonexistent_device.monitor" output))))

(ert-deftest test-source-exists-p-boundary-empty-output-returns-nil ()
  "Test that empty pactl output returns nil."
  (should-not (cj/recording--source-exists-p "any-device" "")))

(ert-deftest test-source-exists-p-boundary-partial-name-no-match ()
  "Test that partial device name does not match."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (should-not (cj/recording--source-exists-p
                 "alsa_output.pci-0000_00_1f.3.analog-stereo" output))))

(provide 'test-video-audio-recording--source-exists-p)
;;; test-video-audio-recording--source-exists-p.el ends here
