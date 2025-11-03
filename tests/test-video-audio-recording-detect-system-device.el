;;; test-video-audio-recording-detect-system-device.el --- Tests for cj/recording-detect-system-device -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-detect-system-device function.
;; Tests auto-detection of system audio monitor device from pactl output.
;; Mocks shell-command-to-string to test regex matching logic.
;;
;; NOTE: This function works correctly - returns the full device name ending in .monitor.
;; The regex \\([^\t\n]+\\.monitor\\) matches any non-tab/newline chars ending with .monitor,
;; which correctly captures the device name field from pactl output.
;;
;; This function may not be actively used (parse-sources is preferred).
;; Tests document current behavior to catch regressions.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording-detect-system-device-normal-built-in-monitor-found ()
  "Test detection of built-in system audio monitor.
Returns full device name."
  (let ((output "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        (should (stringp result))
        (should (equal "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor" result))))))

(ert-deftest test-video-audio-recording-detect-system-device-normal-usb-monitor-found ()
  "Test detection of USB system audio monitor."
  (let ((output "99\talsa_output.usb-0b0e_Jabra_SPEAK_510_USB_1C48F9C067D5020A00-00.analog-stereo.monitor\tPipeWire\ts16le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        (should (stringp result))
        (should (equal "alsa_output.usb-0b0e_Jabra_SPEAK_510_USB_1C48F9C067D5020A00-00.analog-stereo.monitor" result))))))

(ert-deftest test-video-audio-recording-detect-system-device-normal-bluetooth-monitor-found ()
  "Test detection of Bluetooth monitor device."
  (let ((output "81\tbluez_output.00_1B_66_C0_91_6D.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        (should (stringp result))
        (should (equal "bluez_output.00_1B_66_C0_91_6D.1.monitor" result))))))

(ert-deftest test-video-audio-recording-detect-system-device-normal-first-match-returned ()
  "Test that first matching monitor is returned when multiple exist."
  (let ((output (concat "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                        "81\tbluez_output.00_1B_66_C0_91_6D.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n"
                        "99\talsa_output.usb-device.monitor\tPipeWire\ts16le 2ch 48000Hz\tSUSPENDED\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        ;; Returns first monitor device name
        (should (equal "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor" result))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-detect-system-device-boundary-empty-output-returns-nil ()
  "Test that empty output returns nil."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (let ((result (cj/recording-detect-system-device)))
      (should (null result)))))

(ert-deftest test-video-audio-recording-detect-system-device-boundary-only-inputs-returns-nil ()
  "Test that output with only input devices (no monitors) returns nil."
  (let ((output (concat "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                        "79\tbluez_input.00:1B:66:C0:91:6D\tPipeWire\tfloat32le 1ch 48000Hz\tSUSPENDED\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        (should (null result))))))

(ert-deftest test-video-audio-recording-detect-system-device-boundary-whitespace-only-returns-nil ()
  "Test that whitespace-only output returns nil."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "  \n\t\n  ")))
    (let ((result (cj/recording-detect-system-device)))
      (should (null result)))))

(ert-deftest test-video-audio-recording-detect-system-device-boundary-monitor-different-states ()
  "Test that monitors in different states are all matched."
  (let ((output "81\tbluez_output.00_1B_66_C0_91_6D.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        ;; Should match regardless of state (RUNNING, SUSPENDED, IDLE)
        (should (equal "bluez_output.00_1B_66_C0_91_6D.1.monitor" result))))))

(ert-deftest test-video-audio-recording-detect-system-device-boundary-case-insensitive-monitor ()
  "Test that regex is case-insensitive for '.monitor' suffix.
Documents that .MONITOR (uppercase) also matches."
  (let ((output "49\talsa_output.pci-0000_00_1f.3.analog-stereo.MONITOR\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        ;; Case-insensitive: .MONITOR matches
        (should (equal "alsa_output.pci-0000_00_1f.3.analog-stereo.MONITOR" result))))))

;;; Error Cases

(ert-deftest test-video-audio-recording-detect-system-device-error-malformed-output-returns-nil ()
  "Test that malformed output returns nil."
  (let ((output "This is not valid pactl output\nRandom text here\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        (should (null result))))))

(ert-deftest test-video-audio-recording-detect-system-device-error-partial-monitor-matches ()
  "Test that device with .monitor in middle partially matches (documents quirk).
The regex matches up to first .monitor occurrence, even if not at end of device name."
  (let ((output "50\talsa_input.monitor-device.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        ;; QUIRK: Matches partial string "alsa_input.monitor"
        (should (equal "alsa_input.monitor" result))))))

(ert-deftest test-video-audio-recording-detect-system-device-error-incomplete-line ()
  "Test that incomplete lines with .monitor are still matched."
  (let ((output "49\tincomplete-line.monitor\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        ;; Should match device name ending in .monitor
        (should (equal "incomplete-line.monitor" result))))))

(ert-deftest test-video-audio-recording-detect-system-device-error-mixed-valid-invalid ()
  "Test that mix of valid and invalid lines returns first valid monitor."
  (let ((output (concat "invalid line without tabs\n"
                        "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                        "another invalid line\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-detect-system-device)))
        (should (equal "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor" result))))))

(provide 'test-video-audio-recording-detect-system-device)
;;; test-video-audio-recording-detect-system-device.el ends here
