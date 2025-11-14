;;; test-video-audio-recording-parse-pactl-output.el --- Tests for cj/recording--parse-pactl-output -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--parse-pactl-output function.
;; Tests parsing of pactl sources output into structured data.
;; Uses fixture files with sample pactl output for reproducible testing.

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

(ert-deftest test-video-audio-recording-parse-pactl-output-normal-all-devices-returns-list ()
  "Test parsing normal pactl output with all device types."
  (let* ((output (test-load-fixture "pactl-output-normal.txt"))
         (result (cj/recording--parse-pactl-output output)))
    (should (listp result))
    (should (= 6 (length result)))
    ;; Check first device (built-in monitor)
    (should (equal '("alsa_output.pci-0000_00_1f.3.analog-stereo.monitor"
                     "PipeWire"
                     "SUSPENDED")
                   (nth 0 result)))
    ;; Check Bluetooth input
    (should (equal '("bluez_input.00:1B:66:C0:91:6D"
                     "PipeWire"
                     "SUSPENDED")
                   (nth 2 result)))
    ;; Check USB device
    (should (equal '("alsa_input.usb-0b0e_Jabra_SPEAK_510_USB_1C48F9C067D5020A00-00.mono-fallback"
                     "PipeWire"
                     "SUSPENDED")
                   (nth 5 result)))))

(ert-deftest test-video-audio-recording-parse-pactl-output-normal-single-device-returns-list ()
  "Test parsing output with single device."
  (let* ((output (test-load-fixture "pactl-output-single.txt"))
         (result (cj/recording--parse-pactl-output output)))
    (should (listp result))
    (should (= 1 (length result)))
    (should (equal '("alsa_input.pci-0000_00_1f.3.analog-stereo"
                     "PipeWire"
                     "SUSPENDED")
                   (car result)))))

(ert-deftest test-video-audio-recording-parse-pactl-output-normal-monitors-only-returns-list ()
  "Test parsing output with only monitor devices."
  (let* ((output (test-load-fixture "pactl-output-monitors-only.txt"))
         (result (cj/recording--parse-pactl-output output)))
    (should (listp result))
    (should (= 3 (length result)))
    ;; All should end with .monitor
    (dolist (device result)
      (should (string-suffix-p ".monitor" (car device))))))

(ert-deftest test-video-audio-recording-parse-pactl-output-normal-inputs-only-returns-list ()
  "Test parsing output with only input devices."
  (let* ((output (test-load-fixture "pactl-output-inputs-only.txt"))
         (result (cj/recording--parse-pactl-output output)))
    (should (listp result))
    (should (= 3 (length result)))
    ;; None should end with .monitor
    (dolist (device result)
      (should-not (string-suffix-p ".monitor" (car device))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-parse-pactl-output-boundary-empty-string-returns-empty-list ()
  "Test parsing empty string returns empty list."
  (let ((result (cj/recording--parse-pactl-output "")))
    (should (listp result))
    (should (null result))))

(ert-deftest test-video-audio-recording-parse-pactl-output-boundary-empty-file-returns-empty-list ()
  "Test parsing empty file returns empty list."
  (let* ((output (test-load-fixture "pactl-output-empty.txt"))
         (result (cj/recording--parse-pactl-output output)))
    (should (listp result))
    (should (null result))))

(ert-deftest test-video-audio-recording-parse-pactl-output-boundary-whitespace-only-returns-empty-list ()
  "Test parsing whitespace-only string returns empty list."
  (let ((result (cj/recording--parse-pactl-output "   \n\t\n   ")))
    (should (listp result))
    (should (null result))))

(ert-deftest test-video-audio-recording-parse-pactl-output-boundary-single-newline-returns-empty-list ()
  "Test parsing single newline returns empty list."
  (let ((result (cj/recording--parse-pactl-output "\n")))
    (should (listp result))
    (should (null result))))

(ert-deftest test-video-audio-recording-parse-pactl-output-boundary-device-with-running-state-parsed ()
  "Test that RUNNING state (not just SUSPENDED) is parsed correctly."
  (let* ((output "81\tbluez_output.00_1B_66_C0_91_6D.1.monitor\tPipeWire\ts24le 2ch 48000Hz\tRUNNING\n")
         (result (cj/recording--parse-pactl-output output)))
    (should (= 1 (length result)))
    (should (equal "RUNNING" (nth 2 (car result))))))

(ert-deftest test-video-audio-recording-parse-pactl-output-boundary-device-with-idle-state-parsed ()
  "Test that IDLE state is parsed correctly."
  (let* ((output "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tIDLE\n")
         (result (cj/recording--parse-pactl-output output)))
    (should (= 1 (length result)))
    (should (equal "IDLE" (nth 2 (car result))))))

;;; Error Cases

(ert-deftest test-video-audio-recording-parse-pactl-output-error-malformed-lines-ignored ()
  "Test that malformed lines are silently ignored."
  (let* ((output (test-load-fixture "pactl-output-malformed.txt"))
         (result (cj/recording--parse-pactl-output output)))
    (should (listp result))
    (should (null result))))  ; All lines malformed, so empty list

(ert-deftest test-video-audio-recording-parse-pactl-output-error-mixed-valid-invalid-returns-valid ()
  "Test that mix of valid and invalid lines returns only valid ones."
  (let* ((output (concat "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                         "This is invalid\n"
                         "79\tbluez_input.00:1B:66:C0:91:6D\tPipeWire\tfloat32le 1ch 48000Hz\tSUSPENDED\n"
                         "Also invalid\n"))
         (result (cj/recording--parse-pactl-output output)))
    (should (= 2 (length result)))
    (should (equal "alsa_input.pci-0000_00_1f.3.analog-stereo" (car (nth 0 result))))
    (should (equal "bluez_input.00:1B:66:C0:91:6D" (car (nth 1 result))))))

(ert-deftest test-video-audio-recording-parse-pactl-output-error-missing-fields-ignored ()
  "Test that lines with missing fields are ignored."
  (let* ((output "50\tincomplete-line\tPipeWire\n")  ; Missing state and format
         (result (cj/recording--parse-pactl-output output)))
    (should (null result))))

(ert-deftest test-video-audio-recording-parse-pactl-output-error-nil-input-returns-error ()
  "Test that nil input signals an error."
  (should-error (cj/recording--parse-pactl-output nil)))

(provide 'test-video-audio-recording-parse-pactl-output)
;;; test-video-audio-recording-parse-pactl-output.el ends here
