;;; test-video-audio-recording-quick-setup.el --- Tests for cj/recording-quick-setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-quick-setup function.
;; The quick setup shows available mics and auto-selects the default
;; sink's monitor for system audio capture.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-quick-setup-setup ()
  "Reset device variables before each test."
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

(defun test-quick-setup-teardown ()
  "Clean up device variables after each test."
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Normal Cases

(ert-deftest test-video-audio-recording-quick-setup-normal-sets-mic-device ()
  "Test that selecting a mic sets cj/recording-mic-device."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono")
                              ("builtin-input" . "Built-in Analog"))))
                ((symbol-function 'cj/recording--get-default-sink-monitor)
                 (lambda () "jds-labs.monitor"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _args)
                   (car (all-completions "" table)))))
        (cj/recording-quick-setup)
        (should (equal "jabra-input" cj/recording-mic-device)))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-sets-system-to-default-monitor ()
  "Test that system device is set to the default sink's monitor."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono"))))
                ((symbol-function 'cj/recording--get-default-sink-monitor)
                 (lambda () "alsa_output.usb-JDS_Labs-00.analog-stereo.monitor"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _args)
                   (car (all-completions "" table)))))
        (cj/recording-quick-setup)
        (should (equal "alsa_output.usb-JDS_Labs-00.analog-stereo.monitor"
                       cj/recording-system-device)))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-presents-descriptions ()
  "Test that completing-read receives friendly descriptions and Cancel option."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((presented-candidates nil))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("raw-device-1" . "Jabra SPEAK 510 Mono")
                                ("raw-device-2" . "Built-in Analog"))))
                  ((symbol-function 'cj/recording--get-default-sink-monitor)
                   (lambda () "default.monitor"))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq presented-candidates (all-completions "" table))
                     (car presented-candidates))))
          (cj/recording-quick-setup)
          ;; Candidates should have friendly descriptions
          (should (member "Jabra SPEAK 510 Mono" presented-candidates))
          (should (member "Built-in Analog" presented-candidates))
          ;; Cancel option should be present
          (should (member "Cancel" presented-candidates))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-confirmation-message ()
  "Test that confirmation message mentions the selected mic and monitor."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((message-text nil))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono"))))
                  ((symbol-function 'cj/recording--get-default-sink-monitor)
                   (lambda () "jds-labs.analog-stereo.monitor"))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (car (all-completions "" table))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-text (apply #'format fmt args)))))
          (cj/recording-quick-setup)
          (should (string-match-p "Recording ready" message-text))
          (should (string-match-p "Jabra SPEAK 510 Mono" message-text))
          (should (string-match-p "default output monitor" message-text))))
    (test-quick-setup-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-quick-setup-boundary-single-mic ()
  "Test that with only one mic, it still presents selection."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((read-called nil))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("sole-mic" . "Only Mic Available"))))
                  ((symbol-function 'cj/recording--get-default-sink-monitor)
                   (lambda () "default.monitor"))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq read-called t)
                     (car (all-completions "" table)))))
          (cj/recording-quick-setup)
          (should read-called)
          (should (equal "sole-mic" cj/recording-mic-device))))
    (test-quick-setup-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-quick-setup-error-cancel-selected ()
  "Test that selecting Cancel signals user-error and does not set devices."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono"))))
                ((symbol-function 'cj/recording--get-default-sink-monitor)
                 (lambda () "default.monitor"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _choices &rest _args)
                   "Cancel")))
        (should-error (cj/recording-quick-setup) :type 'user-error)
        (should (null cj/recording-mic-device))
        (should (null cj/recording-system-device)))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-error-no-mics ()
  "Test that function signals error when no mics are found."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () nil))
                ((symbol-function 'cj/recording--get-default-sink-monitor)
                 (lambda () "default.monitor")))
        (should-error (cj/recording-quick-setup) :type 'user-error))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-error-no-mics-message ()
  "Test that error message mentions mic and unmuted."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () nil))
                ((symbol-function 'cj/recording--get-default-sink-monitor)
                 (lambda () "default.monitor")))
        (condition-case err
            (cj/recording-quick-setup)
          (user-error
           (should (string-match-p "mic" (error-message-string err))))))
    (test-quick-setup-teardown)))

(provide 'test-video-audio-recording-quick-setup)
;;; test-video-audio-recording-quick-setup.el ends here
