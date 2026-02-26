;;; test-video-audio-recording-quick-setup.el --- Tests for cj/recording-quick-setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-quick-setup function.
;; The quick setup is a two-step flow:
;;   Step 1: Pick a microphone
;;   Step 2: Pick an audio output (sink) with active/inactive indicators
;; The chosen sink's .monitor source is set as the system audio device.

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
      (let ((call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono")
                                ("builtin-input" . "Built-in Analog"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("jds-labs" . "JDS Labs Element IV"))))
                  ((symbol-function 'cj/recording--sink-active-p)
                   (lambda (_name) nil))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (car (all-completions "" table)))))
          (cj/recording-quick-setup)
          (should (equal "jabra-input" cj/recording-mic-device))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-sets-system-to-sink-monitor ()
  "Test that system device is set to the chosen sink's .monitor."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono"))))
                ((symbol-function 'cj/recording--get-available-sinks)
                 (lambda () '(("alsa_output.usb-JDS_Labs-00.analog-stereo" . "JDS Labs Element IV"))))
                ((symbol-function 'cj/recording--sink-active-p)
                 (lambda (_name) nil))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _args)
                   (car (all-completions "" table)))))
        (cj/recording-quick-setup)
        (should (equal "alsa_output.usb-JDS_Labs-00.analog-stereo.monitor"
                       cj/recording-system-device)))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-two-completing-reads ()
  "Test that completing-read is called twice (mic + sink)."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("mic-1" . "Mic One"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" . "Sink One"))))
                  ((symbol-function 'cj/recording--sink-active-p)
                   (lambda (_name) nil))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (car (all-completions "" table)))))
          (cj/recording-quick-setup)
          (should (= 2 call-count))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-active-sink-indicator ()
  "Test that active sinks get the active icon in their label."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((sink-candidates nil)
            (call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("mic-1" . "Mic One"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("active-sink" . "Active Sink")
                                ("inactive-sink" . "Inactive Sink"))))
                  ((symbol-function 'cj/recording--sink-active-p)
                   (lambda (name) (equal name "active-sink")))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (let ((candidates (all-completions "" table)))
                       (when (= call-count 2)
                         (setq sink-candidates candidates))
                       (car candidates)))))
          (cj/recording-quick-setup)
          ;; Active sink should have 󰕾 icon (substring-no-properties strips faces but keeps text)
          (should (cl-some (lambda (c) (and (string-match-p "Active Sink" c)
                                            (string-match-p "󰕾" c)))
                           sink-candidates))
          ;; Inactive sink should have 󰖁 icon
          (should (cl-some (lambda (c) (and (string-match-p "Inactive Sink" c)
                                            (string-match-p "󰖁" c)))
                           sink-candidates))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-active-sorted-first ()
  "Test that active sinks are sorted to the top of the list."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((sink-candidates nil))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("mic-1" . "Mic One"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("inactive-sink" . "Inactive Sink")
                                ("active-sink" . "Active Sink"))))
                  ((symbol-function 'cj/recording--sink-active-p)
                   (lambda (name) (equal name "active-sink")))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (let ((candidates (all-completions "" table)))
                       (when (cl-some (lambda (c) (string-match-p "Sink" c)) candidates)
                         (setq sink-candidates candidates))
                       (car candidates)))))
          (cj/recording-quick-setup)
          ;; First non-Cancel candidate should be the active sink
          (let ((first-sink (car sink-candidates)))
            (should (string-match-p "Active Sink" first-sink)))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-confirmation-message ()
  "Test that confirmation message mentions the selected mic and monitor."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((message-text nil))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("jds-labs.analog-stereo" . "JDS Labs Element IV"))))
                  ((symbol-function 'cj/recording--sink-active-p)
                   (lambda (_name) nil))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (car (all-completions "" table))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-text (apply #'format fmt args)))))
          (cj/recording-quick-setup)
          (should (string-match-p "Recording ready" message-text))
          (should (string-match-p ".monitor" message-text))))
    (test-quick-setup-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-quick-setup-boundary-single-mic ()
  "Test that with only one mic, it still presents selection."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((read-called 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("sole-mic" . "Only Mic Available"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sole-sink" . "Only Sink"))))
                  ((symbol-function 'cj/recording--sink-active-p)
                   (lambda (_name) nil))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq read-called (1+ read-called))
                     (car (all-completions "" table)))))
          (cj/recording-quick-setup)
          (should (= 2 read-called))
          (should (equal "sole-mic" cj/recording-mic-device))))
    (test-quick-setup-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-quick-setup-error-cancel-mic ()
  "Test that cancelling mic selection signals user-error and does not set devices."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono"))))
                ((symbol-function 'cj/recording--get-available-sinks)
                 (lambda () '(("sink-1" . "Sink One"))))
                ((symbol-function 'cj/recording--sink-active-p)
                 (lambda (_name) nil))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _choices &rest _args)
                   "Cancel")))
        (should-error (cj/recording-quick-setup) :type 'user-error)
        (should (null cj/recording-mic-device))
        (should (null cj/recording-system-device)))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-error-cancel-sink ()
  "Test that cancelling sink selection signals user-error."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("jabra-input" . "Jabra SPEAK 510 Mono"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" . "Sink One"))))
                  ((symbol-function 'cj/recording--sink-active-p)
                   (lambda (_name) nil))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (if (= call-count 1)
                         ;; First call: select mic
                         (car (all-completions "" table))
                       ;; Second call: cancel sink
                       "Cancel"))))
          (should-error (cj/recording-quick-setup) :type 'user-error)
          (should (null cj/recording-system-device))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-error-no-mics ()
  "Test that function signals error when no mics are found."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () nil)))
        (should-error (cj/recording-quick-setup) :type 'user-error))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-error-no-mics-message ()
  "Test that error message mentions mic and unmuted."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () nil)))
        (condition-case err
            (cj/recording-quick-setup)
          (user-error
           (should (string-match-p "mic" (error-message-string err))))))
    (test-quick-setup-teardown)))

(provide 'test-video-audio-recording-quick-setup)
;;; test-video-audio-recording-quick-setup.el ends here
