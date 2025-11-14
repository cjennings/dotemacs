;;; test-integration-recording-device-workflow.el --- Integration tests for recording device workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests covering the complete device detection and grouping workflow.
;;
;; This tests the full pipeline from raw pactl output through parsing, grouping,
;; and friendly name assignment. The workflow enables users to select audio devices
;; for recording calls/meetings.
;;
;; Components integrated:
;; - cj/recording--parse-pactl-output (parse raw pactl output into structured data)
;; - cj/recording-parse-sources (shell command wrapper)
;; - cj/recording-group-devices-by-hardware (group inputs/monitors by device)
;; - cj/recording-friendly-state (convert technical state names)
;; - Bluetooth MAC address normalization (colons → underscores)
;; - Device name pattern matching (USB, PCI, Bluetooth)
;; - Friendly name assignment (user-facing device names)
;;
;; Critical integration points:
;; - Parse output must produce data that group-devices can process
;; - Bluetooth MAC normalization must work across parse→group boundary
;; - Incomplete devices (only mic OR only monitor) must be filtered
;; - Friendly names must correctly identify device types

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

;;; Normal Cases - Complete Workflow

(ert-deftest test-integration-recording-device-workflow-parse-to-group-all-devices ()
  "Test complete workflow from pactl output to grouped devices.

When pactl output contains all three device types (built-in, USB, Bluetooth),
the workflow should parse, group, and assign friendly names to all devices.

Components integrated:
- cj/recording--parse-pactl-output (parsing)
- cj/recording-group-devices-by-hardware (grouping + MAC normalization)
- Device pattern matching (USB/PCI/Bluetooth detection)
- Friendly name assignment

Validates:
- All three device types are detected
- Bluetooth MAC addresses normalized (colons → underscores)
- Each device has both mic and monitor
- Friendly names correctly assigned
- Complete data flow: raw output → parsed list → grouped pairs"
  (let ((output (test-load-fixture "pactl-output-normal.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      ;; Test parse step
      (let ((parsed (cj/recording-parse-sources)))
        (should (= 6 (length parsed)))

        ;; Test group step (receives parsed data)
        (let ((grouped (cj/recording-group-devices-by-hardware)))
          (should (= 3 (length grouped)))

          ;; Validate built-in device
          (let ((built-in (assoc "Built-in Laptop Audio" grouped)))
            (should built-in)
            (should (string-prefix-p "alsa_input.pci" (cadr built-in)))
            (should (string-prefix-p "alsa_output.pci" (cddr built-in))))

          ;; Validate USB device
          (let ((usb (assoc "Jabra SPEAK 510 USB" grouped)))
            (should usb)
            (should (string-match-p "Jabra" (cadr usb)))
            (should (string-match-p "Jabra" (cddr usb))))

          ;; Validate Bluetooth device (CRITICAL: MAC normalization)
          (let ((bluetooth (assoc "Bluetooth Headset" grouped)))
            (should bluetooth)
            ;; Input has colons
            (should (string-match-p "00:1B:66:C0:91:6D" (cadr bluetooth)))
            ;; Output has underscores
            (should (string-match-p "00_1B_66_C0_91_6D" (cddr bluetooth)))
            ;; But they're grouped together!
            (should (equal "bluez_input.00:1B:66:C0:91:6D" (cadr bluetooth)))
            (should (equal "bluez_output.00_1B_66_C0_91_6D.1.monitor" (cddr bluetooth)))))))))

(ert-deftest test-integration-recording-device-workflow-friendly-states-in-list ()
  "Test that friendly state names appear in device list output.

When listing devices, technical state names (SUSPENDED, RUNNING) should be
converted to friendly names (Ready, Active) for better UX.

Components integrated:
- cj/recording-parse-sources (parsing with state)
- cj/recording-friendly-state (state name conversion)

Validates:
- SUSPENDED → Ready
- RUNNING → Active
- State conversion works across the parse workflow"
  (let ((output (concat
                 "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                 "81\tbluez_output.00_1B_66_C0_91_6D.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((parsed (cj/recording-parse-sources)))
        ;; Verify states are parsed correctly
        (should (equal "SUSPENDED" (nth 2 (nth 0 parsed))))
        (should (equal "RUNNING" (nth 2 (nth 1 parsed))))

        ;; Verify friendly conversion works
        (should (equal "Ready" (cj/recording-friendly-state (nth 2 (nth 0 parsed)))))
        (should (equal "Active" (cj/recording-friendly-state (nth 2 (nth 1 parsed)))))))))

;;; Boundary Cases - Incomplete Devices

(ert-deftest test-integration-recording-device-workflow-incomplete-devices-filtered ()
  "Test that devices with only mic OR only monitor are filtered out.

For call recording, we need BOTH mic and monitor from the same device.
Incomplete devices should not appear in the grouped output.

Components integrated:
- cj/recording-parse-sources (parsing all devices)
- cj/recording-group-devices-by-hardware (filtering incomplete pairs)

Validates:
- Device with only mic is filtered
- Device with only monitor is filtered
- Only complete devices (both mic and monitor) are returned
- Filtering happens at group stage, not parse stage"
  (let ((output (concat
                 ;; Complete device
                 "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                 "49\talsa_output.pci-0000_00_1f.3.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                 ;; Incomplete: USB mic with no monitor
                 "100\talsa_input.usb-device.mono-fallback\tPipeWire\ts16le 1ch 16000Hz\tSUSPENDED\n"
                 ;; Incomplete: Bluetooth monitor with no mic
                 "81\tbluez_output.AA_BB_CC_DD_EE_FF.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      ;; Parse sees all 4 devices
      (let ((parsed (cj/recording-parse-sources)))
        (should (= 4 (length parsed)))

        ;; Group returns only 1 complete device
        (let ((grouped (cj/recording-group-devices-by-hardware)))
          (should (= 1 (length grouped)))
          (should (equal "Built-in Laptop Audio" (caar grouped))))))))

;;; Edge Cases - Bluetooth MAC Normalization

(ert-deftest test-integration-recording-device-workflow-bluetooth-mac-variations ()
  "Test Bluetooth MAC normalization with different formats.

Bluetooth devices use colons in input names but underscores in output names.
The grouping must normalize these to match devices correctly.

Components integrated:
- cj/recording-parse-sources (preserves original MAC format)
- cj/recording-group-devices-by-hardware (normalizes MAC for matching)
- Base name extraction (regex patterns)
- MAC address transformation (underscores → colons)

Validates:
- Input with colons (bluez_input.AA:BB:CC:DD:EE:FF) parsed correctly
- Output with underscores (bluez_output.AA_BB_CC_DD_EE_FF) parsed correctly
- Normalization happens during grouping
- Devices paired despite format difference
- Original device names preserved (not mutated)"
  (let ((output (concat
                 "79\tbluez_input.11:22:33:44:55:66\tPipeWire\tfloat32le 1ch 48000Hz\tSUSPENDED\n"
                 "81\tbluez_output.11_22_33_44_55_66.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((parsed (cj/recording-parse-sources)))
        ;; Original formats preserved in parse
        (should (string-match-p "11:22:33" (caar parsed)))
        (should (string-match-p "11_22_33" (caadr parsed)))

        ;; But grouping matches them
        (let ((grouped (cj/recording-group-devices-by-hardware)))
          (should (= 1 (length grouped)))
          (should (equal "Bluetooth Headset" (caar grouped)))
          ;; Original names preserved
          (should (equal "bluez_input.11:22:33:44:55:66" (cadar grouped)))
          (should (equal "bluez_output.11_22_33_44_55_66.1.monitor" (cddar grouped))))))))

;;; Error Cases - Malformed Data

(ert-deftest test-integration-recording-device-workflow-malformed-output-handled ()
  "Test that malformed pactl output is handled gracefully.

When pactl output is malformed or unparseable, the workflow should not crash.
It should return empty results at appropriate stages.

Components integrated:
- cj/recording--parse-pactl-output (malformed line handling)
- cj/recording-group-devices-by-hardware (empty input handling)

Validates:
- Malformed lines are silently skipped during parse
- Empty parse results don't crash grouping
- Workflow degrades gracefully
- No exceptions thrown"
  (let ((output (test-load-fixture "pactl-output-malformed.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) output)))
      (let ((parsed (cj/recording-parse-sources)))
        ;; Malformed output produces empty parse
        (should (null parsed))

        ;; Empty parse produces empty grouping (no crash)
        (let ((grouped (cj/recording-group-devices-by-hardware)))
          (should (null grouped)))))))

(provide 'test-integration-recording-device-workflow)
;;; test-integration-recording-device-workflow.el ends here
