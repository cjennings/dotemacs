;;; test-integration-recording-device-workflow.el --- Integration tests for recording device workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration test covering the device detection path that recording actually
;; uses: raw pactl output through parsing and into friendly state names.
;;
;; Components integrated:
;; - cj/recording--parse-pactl-output (parse raw pactl output into structured data)
;; - cj/recording-parse-sources (shell command wrapper, MOCKED at
;;   shell-command-to-string so no pactl runs)
;; - cj/recording-friendly-state (convert technical state names)
;;
;; Critical integration points:
;; - Parse output must carry device state through to the friendly-name conversion
;;
;; This file once covered a parse-to-group pipeline as well.  That half tested
;; cj/recording-group-devices-by-hardware, a second device-grouping
;; implementation nothing ever called -- cj/recording-select-device is the live
;; selection path and reaches parse-sources directly.  The function and its
;; tests were removed rather than left as coverage that proved an unused code
;; path worked.

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

(provide 'test-integration-recording-device-workflow)
;;; test-integration-recording-device-workflow.el ends here
