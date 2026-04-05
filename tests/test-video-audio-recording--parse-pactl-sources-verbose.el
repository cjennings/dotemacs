;;; test-video-audio-recording--parse-pactl-sources-verbose.el --- Tests for verbose pactl parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--parse-pactl-verbose.
;; Parses the verbose output of `pactl list sources' into structured tuples
;; of (name description mute state).

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--parse-pactl-sources-verbose-normal-single-source ()
  "Test parsing a single source entry."
  (let* ((output "Source #65\n\tState: SUSPENDED\n\tName: alsa_input.usb-Jabra-00.mono\n\tDescription: Jabra SPEAK 510 Mono\n\tMute: no\n")
         (result (cj/recording--parse-pactl-verbose output "Source")))
    (should (= 1 (length result)))
    (should (equal "alsa_input.usb-Jabra-00.mono" (nth 0 (car result))))
    (should (equal "Jabra SPEAK 510 Mono" (nth 1 (car result))))
    (should (equal "no" (nth 2 (car result))))
    (should (equal "SUSPENDED" (nth 3 (car result))))))

(ert-deftest test-video-audio-recording--parse-pactl-sources-verbose-normal-multiple-sources ()
  "Test parsing multiple source entries."
  (let* ((output (concat "Source #65\n\tState: SUSPENDED\n\tName: device-a\n\tDescription: Device A\n\tMute: no\n"
                         "Source #66\n\tState: RUNNING\n\tName: device-b\n\tDescription: Device B\n\tMute: yes\n"))
         (result (cj/recording--parse-pactl-verbose output "Source")))
    (should (= 2 (length result)))
    (should (equal "device-a" (nth 0 (car result))))
    (should (equal "Device B" (nth 1 (cadr result))))
    (should (equal "yes" (nth 2 (cadr result))))))

(ert-deftest test-video-audio-recording--parse-pactl-sources-verbose-normal-monitors-included ()
  "Test that monitor sources are parsed (filtering is done by caller)."
  (let* ((output "Source #67\n\tState: SUSPENDED\n\tName: alsa_output.jds.monitor\n\tDescription: Monitor of JDS Labs\n\tMute: no\n")
         (result (cj/recording--parse-pactl-verbose output "Source")))
    (should (= 1 (length result)))
    (should (string-match-p "\\.monitor$" (nth 0 (car result))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--parse-pactl-sources-verbose-boundary-empty-input ()
  "Test that empty input returns empty list."
  (should (null (cj/recording--parse-pactl-verbose "" "Source"))))

(ert-deftest test-video-audio-recording--parse-pactl-sources-verbose-boundary-extra-fields ()
  "Test that extra fields between sources are ignored."
  (let* ((output (concat "Source #65\n\tState: IDLE\n\tName: dev-a\n\tDescription: Dev A\n\tMute: no\n"
                         "\tDriver: PipeWire\n\tSample Specification: s16le 2ch 48000Hz\n"
                         "\tChannel Map: front-left,front-right\n"
                         "Source #66\n\tState: SUSPENDED\n\tName: dev-b\n\tDescription: Dev B\n\tMute: no\n"))
         (result (cj/recording--parse-pactl-verbose output "Source")))
    (should (= 2 (length result)))
    (should (equal "dev-a" (nth 0 (car result))))
    (should (equal "dev-b" (nth 0 (cadr result))))))

(ert-deftest test-video-audio-recording--parse-pactl-sources-verbose-boundary-description-with-parens ()
  "Test descriptions containing parentheses are captured fully."
  (let* ((output "Source #91\n\tState: SUSPENDED\n\tName: hdmi.monitor\n\tDescription: Monitor of Radeon (HDMI 2)\n\tMute: no\n")
         (result (cj/recording--parse-pactl-verbose output "Source")))
    (should (equal "Monitor of Radeon (HDMI 2)" (nth 1 (car result))))))

;;; Error Cases

(ert-deftest test-video-audio-recording--parse-pactl-sources-verbose-error-malformed-no-name ()
  "Test that source entries without Name field are skipped."
  (let* ((output "Source #65\n\tState: SUSPENDED\n\tDescription: Orphan\n\tMute: no\n")
         (result (cj/recording--parse-pactl-verbose output "Source")))
    (should (null result))))

(provide 'test-video-audio-recording--parse-pactl-sources-verbose)
;;; test-video-audio-recording--parse-pactl-sources-verbose.el ends here
