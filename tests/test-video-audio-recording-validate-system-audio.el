;;; test-video-audio-recording-validate-system-audio.el --- Tests for cj/recording--validate-system-audio -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--validate-system-audio function.
;; Tests the pre-recording validation that catches stale/drifted system
;; audio devices before they cause silent recordings.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-validate-setup ()
  "Reset device variables before each test."
  (setq cj/recording-system-device nil))

(defun test-validate-teardown ()
  "Clean up device variables after each test."
  (setq cj/recording-system-device nil))

;;; Normal Cases

(ert-deftest test-validate-system-audio-normal-device-matches-default-no-change ()
  "Test that no change occurs when device matches current default and audio is active."
  (test-validate-setup)
  (unwind-protect
      (let ((cj/recording-system-device "alsa_output.usb-Jabra-00.analog-stereo.monitor"))
        (cl-letf (((symbol-function 'shell-command-to-string)
                   (lambda (cmd)
                     (cond
                      ((string-match-p "sources short" cmd)
                       "65\talsa_output.usb-Jabra-00.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ((string-match-p "sinks short" cmd)
                       "65\talsa_output.usb-Jabra-00.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ((string-match-p "sink-inputs" cmd) "Sink Input #1\n\tSink: 65\n")
                      ((string-match-p "get-default-sink" cmd) "alsa_output.usb-Jabra-00.analog-stereo")
                      (t "")))))
          (cj/recording--validate-system-audio)
          (should (equal "alsa_output.usb-Jabra-00.analog-stereo.monitor"
                         cj/recording-system-device))))
    (test-validate-teardown)))

(ert-deftest test-validate-system-audio-normal-stale-device-auto-updates ()
  "Test that a stale (non-existent) device is auto-updated to current default."
  (test-validate-setup)
  (unwind-protect
      (let ((cj/recording-system-device "old_disappeared_device.monitor")
            (messages nil))
        (cl-letf (((symbol-function 'shell-command-to-string)
                   (lambda (cmd)
                     (cond
                      ((string-match-p "sources short" cmd)
                       ;; Old device NOT in list
                       "65\talsa_output.usb-Jabra-00.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ((string-match-p "sinks short" cmd)
                       "65\talsa_output.usb-Jabra-00.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ((string-match-p "sink-inputs" cmd) "Sink Input #1\n\tSink: 65\n")
                      ((string-match-p "get-default-sink" cmd) "alsa_output.usb-Jabra-00.analog-stereo")
                      (t ""))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (cj/recording--validate-system-audio)
          (should (equal "alsa_output.usb-Jabra-00.analog-stereo.monitor"
                         cj/recording-system-device))
          (should (cl-some (lambda (m) (string-match-p "no longer exists" m)) messages))))
    (test-validate-teardown)))

(ert-deftest test-validate-system-audio-normal-respects-explicit-non-default-choice ()
  "Test that an existing non-default device is NOT overridden by drift detection."
  (test-validate-setup)
  (unwind-protect
      (let ((cj/recording-system-device "alsa_output.pci-0000.hdmi-stereo.monitor"))
        (cl-letf (((symbol-function 'shell-command-to-string)
                   (lambda (cmd)
                     (cond
                      ((string-match-p "sources short" cmd)
                       ;; Device still exists
                       (concat "65\talsa_output.pci-0000.hdmi-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"
                               "69\talsa_output.usb-Jabra-00.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n"))
                      ((string-match-p "sinks short" cmd)
                       "65\talsa_output.pci-0000.hdmi-stereo\tPipeWire\ts32le 2ch 48000Hz\tRUNNING\n")
                      ((string-match-p "sink-inputs" cmd) "Sink Input #1\n\tSink: 65\n")
                      ;; Default is Jabra, but user explicitly chose HDMI
                      ((string-match-p "get-default-sink" cmd) "alsa_output.usb-Jabra-00.analog-stereo")
                      (t ""))))
                  ((symbol-function 'message) (lambda (_fmt &rest _args) nil))
                  ((symbol-function 'cj/log-silently) (lambda (_fmt &rest _args) nil)))
          (cj/recording--validate-system-audio)
          ;; Should keep the user's explicit HDMI choice, not drift to Jabra
          (should (equal "alsa_output.pci-0000.hdmi-stereo.monitor"
                         cj/recording-system-device))))
    (test-validate-teardown)))

(ert-deftest test-validate-system-audio-normal-no-audio-warns ()
  "Test that no active audio shows warning message and logs diagnostic steps."
  (test-validate-setup)
  (unwind-protect
      (let ((cj/recording-system-device "alsa_output.usb-Jabra-00.analog-stereo.monitor")
            (messages nil)
            (logged nil))
        (cl-letf (((symbol-function 'shell-command-to-string)
                   (lambda (cmd)
                     (cond
                      ((string-match-p "sources short" cmd)
                       "65\talsa_output.usb-Jabra-00.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ((string-match-p "sinks short" cmd)
                       "65\talsa_output.usb-Jabra-00.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ;; No sink inputs — nothing playing
                      ((string-match-p "sink-inputs" cmd) "")
                      ((string-match-p "get-default-sink" cmd) "alsa_output.usb-Jabra-00.analog-stereo")
                      (t ""))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages)))
                  ((symbol-function 'cj/log-silently)
                   (lambda (fmt &rest args)
                     (setq logged (apply #'format fmt args)))))
          (cj/recording--validate-system-audio)
          ;; Echo area should show the warning
          (should (cl-some (lambda (m) (string-match-p "No audio connected" m)) messages))
          ;; Messages buffer should have diagnostic steps
          (should logged)
          (should (string-match-p "C-; r s" logged))))
    (test-validate-teardown)))

(ert-deftest test-validate-system-audio-normal-no-audio-does-not-block ()
  "Test that no active audio does not block — recording proceeds."
  (test-validate-setup)
  (unwind-protect
      (let ((cj/recording-system-device "alsa_output.usb-Jabra-00.analog-stereo.monitor"))
        (cl-letf (((symbol-function 'shell-command-to-string)
                   (lambda (cmd)
                     (cond
                      ((string-match-p "sources short" cmd)
                       "65\talsa_output.usb-Jabra-00.analog-stereo.monitor\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ((string-match-p "sinks short" cmd)
                       "65\talsa_output.usb-Jabra-00.analog-stereo\tPipeWire\ts32le 2ch 48000Hz\tSUSPENDED\n")
                      ((string-match-p "sink-inputs" cmd) "")
                      ((string-match-p "get-default-sink" cmd) "alsa_output.usb-Jabra-00.analog-stereo")
                      (t ""))))
                  ((symbol-function 'message) (lambda (_fmt &rest _args) nil))
                  ((symbol-function 'cj/log-silently) (lambda (_fmt &rest _args) nil)))
          ;; Should return normally, not signal an error or prompt
          (cj/recording--validate-system-audio)))
    (test-validate-teardown)))

;;; Boundary Cases

(ert-deftest test-validate-system-audio-boundary-nil-device-skips-validation ()
  "Test that nil system device skips all validation."
  (let ((cj/recording-system-device nil)
        (shell-called nil))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) (setq shell-called t) "")))
      (cj/recording--validate-system-audio)
      (should-not shell-called))))

(provide 'test-video-audio-recording-validate-system-audio)
;;; test-video-audio-recording-validate-system-audio.el ends here
