;;; test-video-audio-recording--parse-pactl-sinks-verbose.el --- Tests for verbose pactl sinks parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--parse-pactl-sinks-verbose.
;; Parses the verbose output of `pactl list sinks' into structured tuples
;; of (name description mute state).

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Helper

(defvar test-sinks--dir
  (file-name-directory (or load-file-name
                           (locate-library "test-video-audio-recording--parse-pactl-sinks-verbose")))
  "Directory containing this test file.")

(defun test-sinks--fixture (filename)
  "Read fixture FILENAME from tests/fixtures/ directory."
  (let ((path (expand-file-name (concat "fixtures/" filename) test-sinks--dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;;; Normal Cases

(ert-deftest test-parse-pactl-sinks-verbose-normal-multiple-sinks ()
  "Test parsing multiple sink entries from fixture."
  (let* ((output (test-sinks--fixture "pactl-sinks-verbose-normal.txt"))
         (result (cj/recording--parse-pactl-sinks-verbose output)))
    (should (= 3 (length result)))
    (should (equal "alsa_output.usb-JDS_Labs-00.analog-stereo" (nth 0 (nth 0 result))))
    (should (equal "JDS Labs Element IV Analog Stereo" (nth 1 (nth 0 result))))
    (should (equal "no" (nth 2 (nth 0 result))))
    (should (equal "RUNNING" (nth 3 (nth 0 result))))))

(ert-deftest test-parse-pactl-sinks-verbose-normal-single-sink ()
  "Test parsing a single sink entry."
  (let* ((output "Sink #65\n\tState: SUSPENDED\n\tName: alsa_output.usb-JDS-00.analog-stereo\n\tDescription: JDS Labs Element IV\n\tMute: no\n")
         (result (cj/recording--parse-pactl-sinks-verbose output)))
    (should (= 1 (length result)))
    (should (equal "alsa_output.usb-JDS-00.analog-stereo" (nth 0 (car result))))
    (should (equal "JDS Labs Element IV" (nth 1 (car result))))
    (should (equal "no" (nth 2 (car result))))
    (should (equal "SUSPENDED" (nth 3 (car result))))))

(ert-deftest test-parse-pactl-sinks-verbose-normal-muted-sink ()
  "Test that muted sinks are parsed (filtering is done by caller)."
  (let* ((output (test-sinks--fixture "pactl-sinks-verbose-muted.txt"))
         (result (cj/recording--parse-pactl-sinks-verbose output)))
    (should (= 3 (length result)))
    ;; Second sink is muted
    (should (equal "yes" (nth 2 (nth 1 result))))))

;;; Boundary Cases

(ert-deftest test-parse-pactl-sinks-verbose-boundary-empty-input ()
  "Test that empty input returns empty list."
  (should (null (cj/recording--parse-pactl-sinks-verbose ""))))

(ert-deftest test-parse-pactl-sinks-verbose-boundary-extra-fields ()
  "Test that extra fields between sinks are ignored."
  (let* ((output (concat "Sink #65\n\tState: IDLE\n\tName: sink-a\n\tDescription: Sink A\n\tMute: no\n"
                         "\tDriver: PipeWire\n\tSample Specification: s16le 2ch 48000Hz\n"
                         "Sink #66\n\tState: SUSPENDED\n\tName: sink-b\n\tDescription: Sink B\n\tMute: no\n"))
         (result (cj/recording--parse-pactl-sinks-verbose output)))
    (should (= 2 (length result)))
    (should (equal "sink-a" (nth 0 (car result))))
    (should (equal "sink-b" (nth 0 (cadr result))))))

(ert-deftest test-parse-pactl-sinks-verbose-boundary-description-with-parens ()
  "Test descriptions containing parentheses are captured fully."
  (let* ((output "Sink #91\n\tState: SUSPENDED\n\tName: hdmi-output\n\tDescription: Radeon (HDMI 2) Output\n\tMute: no\n")
         (result (cj/recording--parse-pactl-sinks-verbose output)))
    (should (equal "Radeon (HDMI 2) Output" (nth 1 (car result))))))

;;; Error Cases

(ert-deftest test-parse-pactl-sinks-verbose-error-malformed-no-name ()
  "Test that sink entries without Name field are skipped."
  (let* ((output "Sink #65\n\tState: SUSPENDED\n\tDescription: Orphan\n\tMute: no\n")
         (result (cj/recording--parse-pactl-sinks-verbose output)))
    (should (null result))))

(provide 'test-video-audio-recording--parse-pactl-sinks-verbose)
;;; test-video-audio-recording--parse-pactl-sinks-verbose.el ends here
