;;; test-video-audio-recording--build-audio-command.el --- Tests for audio command builder -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--build-audio-command.
;; Verifies correct ffmpeg command construction for audio-only recording
;; (mic + system monitor mixed to lossless FLAC), including shell quoting
;; of device names and output paths.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--build-audio-command-normal-uses-ffmpeg-pulse ()
  "Normal: audio command uses ffmpeg with two PulseAudio inputs mixed to FLAC."
  (let ((cj/recording-mic-boost 2.0)
        (cj/recording-system-volume 1.0))
    (let ((cmd (cj/recording--build-audio-command "mic" "sys" "/tmp/out.flac")))
      (should (string-match-p "ffmpeg" cmd))
      (should (string-match-p "-f pulse -i" cmd))
      (should (string-match-p "amix=inputs=2" cmd))
      (should (string-match-p "-c:a flac" cmd))
      ;; Lossless: no lossy bitrate cap should be emitted.
      (should-not (string-match-p "-b:a" cmd))
      (should-not (string-match-p "-c:a aac" cmd)))))

(ert-deftest test-video-audio-recording--build-audio-command-normal-devices-in-command ()
  "Normal: both mic and system device names appear in the command."
  (let ((cj/recording-mic-boost 1.0)
        (cj/recording-system-volume 1.0))
    (let ((cmd (cj/recording--build-audio-command
                "alsa_input.usb-Jabra-00.mono"
                "alsa_output.usb-JDS-00.monitor"
                "/tmp/out.m4a")))
      (should (string-match-p "alsa_input.usb-Jabra-00.mono" cmd))
      (should (string-match-p "alsa_output.usb-JDS-00.monitor" cmd)))))

(ert-deftest test-video-audio-recording--build-audio-command-normal-volume-in-filter ()
  "Normal: volume settings appear in the filter_complex expression."
  (let ((cj/recording-mic-boost 1.5)
        (cj/recording-system-volume 0.7))
    (let ((cmd (cj/recording--build-audio-command "mic" "sys" "/tmp/out.m4a")))
      (should (string-match-p "volume=1\\.5" cmd))
      (should (string-match-p "volume=0\\.7" cmd)))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--build-audio-command-boundary-device-quoted ()
  "Boundary: device names with spaces are shell-quoted."
  (let ((cj/recording-mic-boost 1.0)
        (cj/recording-system-volume 1.0))
    (let ((cmd (cj/recording--build-audio-command
                "device with spaces" "sys" "/tmp/out.m4a")))
      (should (string-match-p "device\\\\ with\\\\ spaces" cmd)))))

(ert-deftest test-video-audio-recording--build-audio-command-boundary-filename-quoted ()
  "Boundary: output filename with spaces is shell-quoted."
  (let ((cj/recording-mic-boost 1.0)
        (cj/recording-system-volume 1.0))
    (let ((cmd (cj/recording--build-audio-command
                "mic" "sys" "/tmp/my recording.m4a")))
      (should (string-match-p "my\\\\ recording\\.m4a" cmd)))))

(ert-deftest test-video-audio-recording--build-audio-command-boundary-zero-volume ()
  "Boundary: zero volume values produce 0.0 in the command."
  (let ((cj/recording-mic-boost 0.0)
        (cj/recording-system-volume 0.0))
    (let ((cmd (cj/recording--build-audio-command "mic" "sys" "/tmp/out.m4a")))
      (should (string-match-p "volume=0\\.0" cmd)))))

(provide 'test-video-audio-recording--build-audio-command)
;;; test-video-audio-recording--build-audio-command.el ends here
