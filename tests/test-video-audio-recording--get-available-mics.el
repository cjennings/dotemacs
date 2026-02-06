;;; test-video-audio-recording--get-available-mics.el --- Tests for mic discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--get-available-mics.
;; Verifies that available microphones are discovered correctly:
;; - Monitor sources are excluded (they capture output, not input)
;; - Muted sources are excluded
;; - Friendly descriptions from PulseAudio are used

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Helper

(defun test-mics--make-pactl-output (sources)
  "Build fake `pactl list sources' output from SOURCES.
Each source is (name description mute state)."
  (mapconcat (lambda (src)
               (format "Source #1\n\tState: %s\n\tName: %s\n\tDescription: %s\n\tMute: %s\n"
                       (nth 3 src) (nth 0 src) (nth 1 src) (nth 2 src)))
             sources ""))

;;; Normal Cases

(ert-deftest test-video-audio-recording--get-available-mics-normal-filters-monitors ()
  "Test that monitor sources are excluded from mic list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("alsa_input.usb-Jabra.mono" "Jabra Mono" "no" "SUSPENDED")
                  ("alsa_output.usb-Jabra.monitor" "Monitor of Jabra" "no" "SUSPENDED"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (= 1 (length mics)))
      (should (equal "alsa_input.usb-Jabra.mono" (car (car mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-filters-muted ()
  "Test that muted sources are excluded from mic list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("active-mic" "Active Mic" "no" "SUSPENDED")
                  ("muted-mic" "Muted Mic" "yes" "SUSPENDED"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (= 1 (length mics)))
      (should (equal "active-mic" (car (car mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-uses-descriptions ()
  "Test that friendly descriptions are returned as cdr."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("raw-device-name" "Friendly Device Name" "no" "IDLE"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (equal "Friendly Device Name" (cdr (car mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-multiple-mics ()
  "Test that multiple non-muted, non-monitor mics are returned."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("mic-a" "Jabra Mono" "no" "SUSPENDED")
                  ("mic-b" "Built-in" "no" "IDLE")
                  ("alsa_output.jabra.monitor" "Monitor of Jabra" "no" "SUSPENDED")
                  ("muted-mic" "Muted Mic" "yes" "SUSPENDED"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (= 2 (length mics))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--get-available-mics-boundary-empty-output ()
  "Test that empty pactl output returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should (null (cj/recording--get-available-mics)))))

(ert-deftest test-video-audio-recording--get-available-mics-boundary-all-monitors ()
  "Test that if all sources are monitors, returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("sink-a.monitor" "Monitor A" "no" "SUSPENDED")
                  ("sink-b.monitor" "Monitor B" "no" "SUSPENDED"))))))
    (should (null (cj/recording--get-available-mics)))))

(ert-deftest test-video-audio-recording--get-available-mics-boundary-all-muted ()
  "Test that if all non-monitor sources are muted, returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("muted-a" "Mic A" "yes" "SUSPENDED")
                  ("muted-b" "Mic B" "yes" "SUSPENDED"))))))
    (should (null (cj/recording--get-available-mics)))))

(provide 'test-video-audio-recording--get-available-mics)
;;; test-video-audio-recording--get-available-mics.el ends here
