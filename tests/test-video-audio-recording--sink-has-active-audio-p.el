;;; test-video-audio-recording--sink-has-active-audio-p.el --- Tests for cj/recording--sink-has-active-audio-p -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--sink-has-active-audio-p function.
;; Tests parsing of pactl sink-inputs output to detect active audio streams.

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

(ert-deftest test-sink-has-active-audio-p-normal-active-sink-returns-t ()
  "Test that active audio on our sink returns non-nil."
  (let ((output (test-load-fixture "pactl-sink-inputs-active.txt")))
    (should (cj/recording--sink-has-active-audio-p "65" output))))

(ert-deftest test-sink-has-active-audio-p-normal-different-sink-returns-nil ()
  "Test that audio on a different sink returns nil."
  (let ((output (test-load-fixture "pactl-sink-inputs-different-sink.txt")))
    (should-not (cj/recording--sink-has-active-audio-p "65" output))))

;;; Boundary Cases

(ert-deftest test-sink-has-active-audio-p-boundary-empty-output-returns-nil ()
  "Test that empty pactl output returns nil."
  (should-not (cj/recording--sink-has-active-audio-p "65" "")))

(ert-deftest test-sink-has-active-audio-p-boundary-no-sink-inputs-returns-nil ()
  "Test that output with no sink inputs returns nil."
  (let ((output (test-load-fixture "pactl-sink-inputs-empty.txt")))
    (should-not (cj/recording--sink-has-active-audio-p "65" output))))

(ert-deftest test-sink-has-active-audio-p-boundary-multiple-inputs-one-matches ()
  "Test that multiple sink inputs where one matches returns non-nil."
  (let ((output (concat "Sink Input #42\n"
                        "\tSink: 73\n"
                        "\tCorked: no\n"
                        "Sink Input #43\n"
                        "\tSink: 65\n"
                        "\tCorked: no\n")))
    (should (cj/recording--sink-has-active-audio-p "65" output))))

(ert-deftest test-sink-has-active-audio-p-boundary-index-substring-no-false-match ()
  "Test that sink index 6 does not match sink 65."
  (let ((output (test-load-fixture "pactl-sink-inputs-active.txt")))
    (should-not (cj/recording--sink-has-active-audio-p "6" output))))

;;; get-sink-index tests

(ert-deftest test-get-sink-index-normal-returns-index ()
  "Test that sink name is resolved to its index."
  (let ((output "65\talsa_output.usb-Jabra-00.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (should (equal "65" (cj/recording--get-sink-index
                         "alsa_output.usb-Jabra-00.analog-stereo" output)))))

(ert-deftest test-get-sink-index-normal-nonexistent-returns-nil ()
  "Test that non-existent sink name returns nil."
  (let ((output "65\talsa_output.usb-Jabra-00.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (should-not (cj/recording--get-sink-index "nonexistent-sink" output))))

(ert-deftest test-get-sink-index-boundary-empty-output-returns-nil ()
  "Test that empty output returns nil."
  (should-not (cj/recording--get-sink-index "any-sink" "")))

(ert-deftest test-get-sink-index-normal-multiple-sinks ()
  "Test correct index returned when multiple sinks present."
  (let ((output (concat "65\talsa_output.pci-0000.hdmi-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                        "69\talsa_output.usb-Jabra-00.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")))
    (should (equal "69" (cj/recording--get-sink-index
                         "alsa_output.usb-Jabra-00.analog-stereo" output)))))

(provide 'test-video-audio-recording--sink-has-active-audio-p)
;;; test-video-audio-recording--sink-has-active-audio-p.el ends here
