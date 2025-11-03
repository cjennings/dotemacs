;;; test-video-audio-recording-group-devices-by-hardware.el --- Tests for cj/recording-group-devices-by-hardware -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-group-devices-by-hardware function.
;; Tests grouping of audio sources by physical hardware device.
;; Critical test: Bluetooth MAC address normalization (colons vs underscores).
;;
;; This function is used by the quick setup command to automatically pair
;; microphone and monitor devices from the same hardware.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
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

(ert-deftest test-video-audio-recording-group-devices-by-hardware-normal-all-types-grouped ()
  "Test grouping of all three device types (built-in, USB, Bluetooth).
This is the key test validating the complete grouping logic."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        (should (listp result))
        (should (= 3 (length result)))
        ;; Check that we have all three device types
        (let ((names (mapcar #'car result)))
          (should (member "Built-in Laptop Audio" names))
          (should (member "Bluetooth Headset" names))
          (should (member "Jabra SPEAK 510 USB" names)))
        ;; Verify each device has both mic and monitor
        (dolist (device result)
          (should (stringp (car device)))           ; friendly name
          (should (stringp (cadr device)))          ; mic device
          (should (stringp (cddr device)))          ; monitor device
          (should-not (string-suffix-p ".monitor" (cadr device)))  ; mic not monitor
          (should (string-suffix-p ".monitor" (cddr device))))))))  ; monitor has suffix

(ert-deftest test-video-audio-recording-group-devices-by-hardware-normal-built-in-paired ()
  "Test that built-in laptop audio devices are correctly paired."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let* ((result (cj/recording-group-devices-by-hardware))
             (built-in (assoc "Built-in Laptop Audio" result)))
        (should built-in)
        (should (string-match-p "pci-0000_00_1f" (cadr built-in)))
        (should (string-match-p "pci-0000_00_1f" (cddr built-in)))
        (should (equal "alsa_input.pci-0000_00_1f.3.analog-stereo" (cadr built-in)))
        (should (equal "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor" (cddr built-in)))))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-normal-usb-paired ()
  "Test that USB devices (Jabra) are correctly paired."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let* ((result (cj/recording-group-devices-by-hardware))
             (jabra (assoc "Jabra SPEAK 510 USB" result)))
        (should jabra)
        (should (string-match-p "Jabra" (cadr jabra)))
        (should (string-match-p "Jabra" (cddr jabra)))))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-normal-bluetooth-paired ()
  "Test that Bluetooth devices are correctly paired.
CRITICAL: Tests MAC address normalization (colons in input, underscores in output)."
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let* ((result (cj/recording-group-devices-by-hardware))
             (bluetooth (assoc "Bluetooth Headset" result)))
        (should bluetooth)
        ;; Input has colons: bluez_input.00:1B:66:C0:91:6D
        (should (equal "bluez_input.00:1B:66:C0:91:6D" (cadr bluetooth)))
        ;; Output has underscores: bluez_output.00_1B_66_C0_91_6D.1.monitor
        ;; But they should still be grouped together (MAC address normalized)
        (should (equal "bluez_output.00_1B_66_C0_91_6D.1.monitor" (cddr bluetooth)))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-group-devices-by-hardware-boundary-empty-returns-empty ()
  "Test that empty pactl output returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (let ((result (cj/recording-group-devices-by-hardware)))
      (should (listp result))
      (should (null result)))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-boundary-only-inputs-returns-empty ()
  "Test that only input devices (no monitors) returns empty list.
Devices must have BOTH mic and monitor to be included."
  (let ((output (test-load-fixture "pactl-output-inputs-only.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        (should (listp result))
        (should (null result))))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-boundary-only-monitors-returns-empty ()
  "Test that only monitor devices (no inputs) returns empty list."
  (let ((output (test-load-fixture "pactl-output-monitors-only.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        (should (listp result))
        (should (null result))))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-boundary-single-complete-device ()
  "Test that single device with both mic and monitor is returned."
  (let ((output "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        (should (= 1 (length result)))
        (should (equal "Built-in Laptop Audio" (caar result)))))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-boundary-mixed-complete-incomplete ()
  "Test that only devices with BOTH mic and monitor are included.
Incomplete devices (only mic or only monitor) are filtered out."
  (let ((output (concat
                 ;; Complete device (built-in)
                 "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                 "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                 ;; Incomplete: USB mic with no monitor
                 "100\talsa_input.usb-device.mono-fallback\tPipeWire\ts16le 1ch 16000Hz\tSUSPENDED\n"
                 ;; Incomplete: Bluetooth monitor with no mic
                 "81\tbluez_output.AA_BB_CC_DD_EE_FF.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        ;; Only the complete built-in device should be returned
        (should (= 1 (length result)))
        (should (equal "Built-in Laptop Audio" (caar result)))))))

;;; Error Cases

(ert-deftest test-video-audio-recording-group-devices-by-hardware-error-malformed-output-returns-empty ()
  "Test that malformed pactl output returns empty list."
  (let ((output (test-load-fixture "pactl-output-malformed.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        (should (listp result))
        (should (null result))))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-error-unknown-device-type ()
  "Test that unknown device types get generic 'USB Audio Device' name."
  (let ((output (concat
                 "100\talsa_input.usb-unknown_device-00.analog-stereo\tPipeWire\ts16le 2ch 16000Hz\tSUSPENDED\n"
                 "99\talsa_output.usb-unknown_device-00.analog-stereo.monitor\tPipeWire\ts16le 2ch 48000Hz\tSUSPENDED\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        (should (= 1 (length result)))
        ;; Should get generic USB name (not matching Jabra pattern)
        (should (equal "USB Audio Device" (caar result)))))))

(ert-deftest test-video-audio-recording-group-devices-by-hardware-error-bluetooth-mac-case-variations ()
  "Test that Bluetooth MAC addresses work with different formatting.
Tests the normalization logic handles various MAC address formats."
  (let ((output (concat
                 ;; Input with colons (typical)
                 "79\tbluez_input.AA:BB:CC:DD:EE:FF\tPipeWire\tfloat32le 1ch 48000Hz\tSUSPENDED\n"
                 ;; Output with underscores (typical)
                 "81\tbluez_output.AA_BB_CC_DD_EE_FF.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((result (cj/recording-group-devices-by-hardware)))
        (should (= 1 (length result)))
        (should (equal "Bluetooth Headset" (caar result)))
        ;; Verify both devices paired despite different MAC formats
        (let ((device (car result)))
          (should (string-match-p "AA:BB:CC" (cadr device)))
          (should (string-match-p "AA_BB_CC" (cddr device))))))))

(provide 'test-video-audio-recording-group-devices-by-hardware)
;;; test-video-audio-recording-group-devices-by-hardware.el ends here
