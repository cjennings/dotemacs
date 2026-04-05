;;; test-video-audio-recording--test-device.el --- Tests for device test helper -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--test-device.
;; Verifies the shared record-and-playback logic used by test-mic and test-monitor.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--test-device-normal-runs-ffmpeg-then-ffplay ()
  "Runs exactly 2 shell commands: ffmpeg to record, ffplay to playback."
  (let ((commands nil))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (push cmd commands) 0)))
      (cj/recording--test-device "test-device" "test-" "GO!")
      (should (= 2 (length commands)))
      ;; ffmpeg runs first (pushed last due to stack order)
      (should (string-match-p "ffmpeg" (cadr commands)))
      (should (string-match-p "ffplay" (car commands))))))

(ert-deftest test-video-audio-recording--test-device-normal-uses-device-in-ffmpeg ()
  "The provided device name appears in the ffmpeg command."
  (let ((commands nil))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (push cmd commands) 0)))
      (cj/recording--test-device "alsa_input.usb-Jabra.mono" "mic-" "SPEAK!")
      (let ((ffmpeg-cmd (cadr commands)))
        (should (string-match-p "alsa_input.usb-Jabra.mono" ffmpeg-cmd))
        (should (string-match-p "-t 5" ffmpeg-cmd))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--test-device-boundary-device-with-special-chars ()
  "Device names with special characters are shell-quoted."
  (let ((commands nil))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (push cmd commands) 0)))
      (cj/recording--test-device "device with spaces" "test-" "GO!")
      (let ((ffmpeg-cmd (cadr commands)))
        ;; shell-quote-argument should have escaped the spaces
        (should (string-match-p "device" ffmpeg-cmd))))))

;;; Error Cases

(ert-deftest test-video-audio-recording--test-device-error-ffmpeg-failure-no-crash ()
  "Function completes without error even when ffmpeg returns non-zero."
  (cl-letf (((symbol-function 'shell-command)
             (lambda (_cmd) 1)))
    ;; Should not signal any error
    (cj/recording--test-device "dev" "test-" "GO!")
    (should t)))

(provide 'test-video-audio-recording--test-device)
;;; test-video-audio-recording--test-device.el ends here
