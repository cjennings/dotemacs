;;; test-video-audio-recording--device-status-label.el --- Tests for device status label -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--device-status-label.
;; Verifies human-readable label mapping from PulseAudio state strings.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--device-status-label-normal-running ()
  "RUNNING unmuted device returns [in use]."
  (should (equal "[in use]" (cj/recording--device-status-label "RUNNING" "no"))))

(ert-deftest test-video-audio-recording--device-status-label-normal-idle ()
  "IDLE unmuted device returns [ready]."
  (should (equal "[ready]" (cj/recording--device-status-label "IDLE" "no"))))

(ert-deftest test-video-audio-recording--device-status-label-normal-suspended ()
  "SUSPENDED unmuted device returns [available]."
  (should (equal "[available]" (cj/recording--device-status-label "SUSPENDED" "no"))))

(ert-deftest test-video-audio-recording--device-status-label-normal-muted-overrides-state ()
  "Muted device returns [muted] regardless of state."
  (should (equal "[muted]" (cj/recording--device-status-label "RUNNING" "yes")))
  (should (equal "[muted]" (cj/recording--device-status-label "IDLE" "yes")))
  (should (equal "[muted]" (cj/recording--device-status-label "SUSPENDED" "yes"))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--device-status-label-boundary-nil-state ()
  "Nil state with unmuted falls through to [available]."
  (should (equal "[available]" (cj/recording--device-status-label nil "no"))))

(ert-deftest test-video-audio-recording--device-status-label-boundary-lowercase-state ()
  "Lowercase state is handled via upcase."
  (should (equal "[in use]" (cj/recording--device-status-label "running" "no")))
  (should (equal "[ready]" (cj/recording--device-status-label "idle" "no"))))

(ert-deftest test-video-audio-recording--device-status-label-boundary-empty-state ()
  "Empty string state falls through to [available]."
  (should (equal "[available]" (cj/recording--device-status-label "" "no"))))

;;; Error Cases

(ert-deftest test-video-audio-recording--device-status-label-error-unknown-state ()
  "Unknown state string falls through to [available]."
  (should (equal "[available]" (cj/recording--device-status-label "BOGUS" "no"))))

(ert-deftest test-video-audio-recording--device-status-label-error-muted-nil-state ()
  "Muted with nil state still returns [muted]."
  (should (equal "[muted]" (cj/recording--device-status-label nil "yes"))))

(provide 'test-video-audio-recording--device-status-label)
;;; test-video-audio-recording--device-status-label.el ends here
