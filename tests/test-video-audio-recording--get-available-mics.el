;;; test-video-audio-recording--get-available-mics.el --- Tests for mic discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--get-available-mics.
;; Verifies that available microphones are discovered correctly:
;; - Monitor sources are excluded (they capture output, not input)
;; - Muted sources are included (shown with [muted] label in UI)
;; - Friendly descriptions from PulseAudio are used
;; - PulseAudio state and mute status are included

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
      (should (equal "alsa_input.usb-Jabra.mono" (nth 0 (car mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-includes-muted ()
  "Test that muted sources are included in mic list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("active-mic" "Active Mic" "no" "SUSPENDED")
                  ("muted-mic" "Muted Mic" "yes" "SUSPENDED"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (= 2 (length mics)))
      (should (equal "yes" (nth 3 (nth 1 mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-uses-descriptions ()
  "Test that friendly descriptions are returned as second element."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("raw-device-name" "Friendly Device Name" "no" "IDLE"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (equal "Friendly Device Name" (nth 1 (car mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-includes-state ()
  "Test that PulseAudio state is returned as third element."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("mic-a" "Mic A" "no" "RUNNING"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (equal "RUNNING" (nth 2 (car mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-includes-mute-status ()
  "Test that mute status is returned as fourth element."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("mic-a" "Mic A" "no" "RUNNING"))))))
    (let ((mics (cj/recording--get-available-mics)))
      (should (equal "no" (nth 3 (car mics)))))))

(ert-deftest test-video-audio-recording--get-available-mics-normal-multiple-mics ()
  "Test that multiple mics are returned including muted ones."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-mics--make-pactl-output
                '(("mic-a" "Jabra Mono" "no" "SUSPENDED")
                  ("mic-b" "Built-in" "no" "IDLE")
                  ("alsa_output.jabra.monitor" "Monitor of Jabra" "no" "SUSPENDED")
                  ("muted-mic" "Muted Mic" "yes" "SUSPENDED"))))))
    (let ((mics (cj/recording--get-available-mics)))
      ;; 3 non-monitor sources (including the muted one)
      (should (= 3 (length mics))))))

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

(provide 'test-video-audio-recording--get-available-mics)
;;; test-video-audio-recording--get-available-mics.el ends here
