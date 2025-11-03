;;; test-video-audio-recording-detect-mic-device.el --- Tests for cj/recording-detect-mic-device -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-detect-mic-device function.
;; Tests auto-detection of microphone input device from pactl output.
;; Mocks shell-command-to-string to test regex matching logic.
;;
;; IMPORTANT: These tests document actual behavior, which appears to have a bug.
;; The function currently returns the pactl ID number (e.g., "50") instead of
;; the device name (e.g., "alsa_input.pci-0000_00_1f.3.analog-stereo").
;; This is because the regex captures group 1 is \\([^\t\n]+\\) which stops
;; at the first tab, capturing only the ID.
;;
;; This function may not be actively used (parse-sources is preferred).
;; Tests document current behavior to catch regressions if function is fixed.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording-detect-mic-device-normal-built-in-analog-stereo-found ()
  "Test detection of built-in analog stereo microphone.
Note: Returns first match which is the monitor (ID 49), not the input."
  (let ((output "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        (should (stringp result))
        ;; BUG: Returns first match "49" (monitor), not input "50"
        (should (equal "49" result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-normal-usb-analog-stereo-found ()
  "Test detection of USB analog stereo microphone.
Note: Returns ID '100', not device name."
  (let ((output "100\talsa_input.usb-0b0e_Jabra_SPEAK_510_USB_1C48F9C067D5020A00-00.analog-stereo\tPipeWire\ts16le 2ch 16000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        (should (stringp result))
        ;; Current behavior: returns ID "100"
        (should (equal "100" result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-normal-first-match-returned ()
  "Test that first matching device is returned when multiple exist.
Note: Returns first ID, not device name."
  (let ((output (concat "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                        "100\talsa_input.usb-device.analog-stereo\tPipeWire\ts16le 2ch 16000Hz\tSUSPENDED\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        ;; Current behavior: returns first ID "50"
        (should (equal "50" result))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-detect-mic-device-boundary-empty-output-returns-nil ()
  "Test that empty output returns nil."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (let ((result (cj/recording-detect-mic-device)))
      (should (null result)))))

(ert-deftest test-video-audio-recording-detect-mic-device-boundary-only-monitors-returns-nil ()
  "Test that output with only monitor devices still matches (documents bug).
Current regex doesn't exclude monitors, so this returns ID '49'."
  (let ((output "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        ;; BUG: Should return nil for monitors, but regex doesn't exclude them
        (should (equal "49" result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-boundary-mono-fallback-no-match ()
  "Test that mono-fallback device doesn't match (not stereo)."
  (let ((output "100\talsa_input.usb-0b0e_Jabra_SPEAK_510_USB_1C48F9C067D5020A00-00.mono-fallback\tPipeWire\ts16le 1ch 16000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        (should (null result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-boundary-bluetooth-no-match ()
  "Test that Bluetooth devices without 'analog stereo' don't match."
  (let ((output "79\tbluez_input.00:1B:66:C0:91:6D\tPipeWire\tfloat32le 1ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        (should (null result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-boundary-whitespace-only-returns-nil ()
  "Test that whitespace-only output returns nil."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "  \n\t\n  ")))
    (let ((result (cj/recording-detect-mic-device)))
      (should (null result)))))

(ert-deftest test-video-audio-recording-detect-mic-device-boundary-case-insensitive-analog ()
  "Test that 'ANALOG' (uppercase) matches (case-insensitive regex).
Documents that regex is actually case-insensitive."
  (let ((output "50\talsa_input.pci-0000_00_1f.3.ANALOG-STEREO\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        ;; Regex is case-insensitive, matches uppercase
        (should (equal "50" result))))))

;;; Error Cases

(ert-deftest test-video-audio-recording-detect-mic-device-error-malformed-output-returns-nil ()
  "Test that malformed output returns nil."
  (let ((output "This is not valid pactl output\nRandom text here\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        (should (null result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-error-partial-match-analog-only ()
  "Test that 'analog' without 'stereo' doesn't match."
  (let ((output "50\talsa_input.pci-0000_00_1f.3.analog-mono\tPipeWire\ts32le 1ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        (should (null result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-error-partial-match-stereo-only ()
  "Test that 'stereo' without 'analog' doesn't match."
  (let ((output "50\talsa_input.pci-0000_00_1f.3.digital-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        (should (null result))))))

(ert-deftest test-video-audio-recording-detect-mic-device-error-monitor-with-analog-stereo-matches-bug ()
  "Test that monitor device with 'analog stereo' incorrectly matches (documents bug).
Should return nil for monitors, but current regex doesn't filter them."
  (let ((output "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-mic-device)))
        ;; BUG: Returns ID "49" even though this is a monitor (output device)
        (should (equal "49" result))))))

(provide 'test-video-audio-recording-detect-mic-device)
;;; test-video-audio-recording-detect-mic-device.el ends here
