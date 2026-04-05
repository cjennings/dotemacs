;;; test-video-audio-recording--get-sink-index.el --- Tests for sink index lookup -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--get-sink-index.
;; Verifies numeric index extraction from `pactl list sinks short' output.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

(defconst test--sinks-output
  "47\talsa_output.usb-Jabra_SPEAK_510-00.analog-stereo\tmodule-alsa-card.c\ts16le 1ch 16000Hz\tSUSPENDED
82\talsa_output.pci-0000_00_1f.3.analog-stereo\tmodule-alsa-card.c\ts32le 2ch 48000Hz\tRUNNING
135\talsa_output.usb-JDS_Labs_Element_IV-00.analog-stereo\tmodule-alsa-card.c\ts32le 2ch 96000Hz\tIDLE"
  "Sample pactl list sinks short output for tests.")

;;; Normal Cases

(ert-deftest test-video-audio-recording--get-sink-index-normal-returns-index ()
  "Returns the numeric index for a matching sink name."
  (should (equal "82" (cj/recording--get-sink-index
                       "alsa_output.pci-0000_00_1f.3.analog-stereo"
                       test--sinks-output))))

(ert-deftest test-video-audio-recording--get-sink-index-normal-first-sink ()
  "Returns index for the first sink in the list."
  (should (equal "47" (cj/recording--get-sink-index
                       "alsa_output.usb-Jabra_SPEAK_510-00.analog-stereo"
                       test--sinks-output))))

(ert-deftest test-video-audio-recording--get-sink-index-normal-last-sink ()
  "Returns index for the last sink in the list."
  (should (equal "135" (cj/recording--get-sink-index
                        "alsa_output.usb-JDS_Labs_Element_IV-00.analog-stereo"
                        test--sinks-output))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--get-sink-index-boundary-not-found ()
  "Returns nil when sink name is not in the output."
  (should (null (cj/recording--get-sink-index
                 "nonexistent.sink"
                 test--sinks-output))))

(ert-deftest test-video-audio-recording--get-sink-index-boundary-empty-output ()
  "Returns nil for empty output string."
  (should (null (cj/recording--get-sink-index
                 "alsa_output.pci-0000_00_1f.3.analog-stereo"
                 ""))))

(ert-deftest test-video-audio-recording--get-sink-index-boundary-substring-no-match ()
  "Does not match when sink name is a substring of another sink."
  (should (null (cj/recording--get-sink-index
                 "alsa_output.pci-0000_00_1f.3"
                 test--sinks-output))))

;;; Error Cases

(ert-deftest test-video-audio-recording--get-sink-index-error-malformed-lines ()
  "Returns nil when output has no tab-separated fields."
  (should (null (cj/recording--get-sink-index
                 "some-sink"
                 "this is not valid pactl output\nneither is this"))))

(provide 'test-video-audio-recording--get-sink-index)
;;; test-video-audio-recording--get-sink-index.el ends here
