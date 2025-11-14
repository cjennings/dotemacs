;;; test-video-audio-recording-parse-sources.el --- Tests for cj/recording-parse-sources -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-parse-sources function.
;; Tests the wrapper that calls pactl and delegates to internal parser.
;; Mocks shell-command-to-string to avoid system dependencies.

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

(ert-deftest test-video-audio-recording-parse-sources-normal-calls-pactl-and-parses ()
  "Test that parse-sources calls shell command and returns parsed list."
  (let ((fixture-output (test-load-fixture "pactl-output-normal.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) fixture-output)))
      (let ((result (cj/recording-parse-sources)))
        (should (listp result))
        (should (= 6 (length result)))
        ;; Verify it returns structured data
        (should (equal "alsa_output.pci-0000_00_1f.3.analog-stereo.monitor"
                       (car (nth 0 result))))
        (should (equal "PipeWire" (nth 1 (nth 0 result))))
        (should (equal "SUSPENDED" (nth 2 (nth 0 result))))))))

(ert-deftest test-video-audio-recording-parse-sources-normal-single-device-returns-list ()
  "Test parse-sources with single device."
  (let ((fixture-output (test-load-fixture "pactl-output-single.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) fixture-output)))
      (let ((result (cj/recording-parse-sources)))
        (should (listp result))
        (should (= 1 (length result)))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-parse-sources-boundary-empty-output-returns-empty-list ()
  "Test that empty pactl output returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (let ((result (cj/recording-parse-sources)))
      (should (listp result))
      (should (null result)))))

(ert-deftest test-video-audio-recording-parse-sources-boundary-whitespace-output-returns-empty-list ()
  "Test that whitespace-only output returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "  \n\t\n  ")))
    (let ((result (cj/recording-parse-sources)))
      (should (listp result))
      (should (null result)))))

;;; Error Cases

(ert-deftest test-video-audio-recording-parse-sources-error-malformed-output-returns-empty-list ()
  "Test that malformed output is handled gracefully."
  (let ((fixture-output (test-load-fixture "pactl-output-malformed.txt")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) fixture-output)))
      (let ((result (cj/recording-parse-sources)))
        (should (listp result))
        (should (null result))))))

(ert-deftest test-video-audio-recording-parse-sources-error-mixed-valid-invalid-returns-valid-only ()
  "Test that mix of valid and invalid lines returns only valid entries."
  (let ((mixed-output (concat
                       "50\talsa_input.pci-0000_00_1f.3.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                       "invalid line\n"
                       "79\tbluez_input.00:1B:66:C0:91:6D\tPipeWire\tfloat32le 1ch 48000Hz\tRUNNING\n")))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) mixed-output)))
      (let ((result (cj/recording-parse-sources)))
        (should (= 2 (length result)))
        (should (equal "alsa_input.pci-0000_00_1f.3.analog-stereo" (car (nth 0 result))))
        (should (equal "bluez_input.00:1B:66:C0:91:6D" (car (nth 1 result))))))))

(provide 'test-video-audio-recording-parse-sources)
;;; test-video-audio-recording-parse-sources.el ends here
