;;; test-video-audio-recording--get-default-sink-monitor.el --- Tests for default sink monitor -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--get-default-sink-monitor.
;; This function returns the monitor source name for the default audio
;; output, which captures "what you hear" (call audio, music, etc.).

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--get-default-sink-monitor-normal-appends-monitor ()
  "Test that .monitor is appended to the default sink name."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "alsa_output.usb-JDS_Labs-00.analog-stereo\n")))
    (should (equal "alsa_output.usb-JDS_Labs-00.analog-stereo.monitor"
                   (cj/recording--get-default-sink-monitor)))))

(ert-deftest test-video-audio-recording--get-default-sink-monitor-normal-trims-whitespace ()
  "Test that trailing whitespace/newlines are stripped from sink name."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "  alsa_output.pci-0000.analog-stereo  \n")))
    (should (equal "alsa_output.pci-0000.analog-stereo.monitor"
                   (cj/recording--get-default-sink-monitor)))))

(ert-deftest test-video-audio-recording--get-default-sink-monitor-normal-bluetooth-sink ()
  "Test with a bluetooth default sink."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "bluez_output.AA_BB_CC_DD_EE_FF.a2dp-sink\n")))
    (should (equal "bluez_output.AA_BB_CC_DD_EE_FF.a2dp-sink.monitor"
                   (cj/recording--get-default-sink-monitor)))))

;;; Error Cases

(ert-deftest test-video-audio-recording--get-default-sink-monitor-error-empty-output ()
  "Test that empty pactl output signals user-error."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should-error (cj/recording--get-default-sink-monitor) :type 'user-error)))

(ert-deftest test-video-audio-recording--get-default-sink-monitor-error-whitespace-only ()
  "Test that whitespace-only output signals user-error."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "  \n  ")))
    (should-error (cj/recording--get-default-sink-monitor) :type 'user-error)))

(provide 'test-video-audio-recording--get-default-sink-monitor)
;;; test-video-audio-recording--get-default-sink-monitor.el ends here
