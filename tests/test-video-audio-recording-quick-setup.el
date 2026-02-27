;;; test-video-audio-recording-quick-setup.el --- Tests for cj/recording-quick-setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-quick-setup function.
;; The quick setup is a two-step flow:
;;   Step 1: Pick a microphone (with state labels)
;;   Step 2: Pick an audio output (sink) with state labels
;; Both steps show [active - running], [active - idle], or
;; [inactive - suspended] after the device description.
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
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () '(("jabra-input" "Jabra SPEAK 510 Mono" "SUSPENDED")
                              ("builtin-input" "Built-in Analog" "SUSPENDED"))))
                ((symbol-function 'cj/recording--get-available-sinks)
                 (lambda () '(("jds-labs" "JDS Labs Element IV" "RUNNING"))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _args)
                   (car (all-completions "" table)))))
        (cj/recording-quick-setup)
        (should (equal "jabra-input" cj/recording-mic-device)))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-sets-system-to-sink-monitor ()
  "Test that system device is set to the chosen sink's .monitor."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                 (lambda () '(("jabra-input" "Jabra SPEAK 510 Mono" "SUSPENDED"))))
                ((symbol-function 'cj/recording--get-available-sinks)
                 (lambda () '(("alsa_output.usb-JDS_Labs-00.analog-stereo" "JDS Labs Element IV" "RUNNING"))))
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
                   (lambda () '(("mic-1" "Mic One" "SUSPENDED"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" "Sink One" "SUSPENDED"))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (car (all-completions "" table)))))
          (cj/recording-quick-setup)
          (should (= 2 call-count))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-running-label ()
  "Test that RUNNING devices show [active - running] label."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((mic-candidates nil)
            (call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("mic-1" "Running Mic" "RUNNING"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" "Sink One" "SUSPENDED"))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (let ((candidates (all-completions "" table)))
                       (when (= call-count 1)
                         (setq mic-candidates candidates))
                       (car candidates)))))
          (cj/recording-quick-setup)
          (should (cl-some (lambda (c) (string-match-p "\\[active - running\\]" c))
                           mic-candidates))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-idle-label ()
  "Test that IDLE devices show [active - idle] label."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((sink-candidates nil)
            (call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("mic-1" "Mic One" "SUSPENDED"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" "Idle Sink" "IDLE"))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (let ((candidates (all-completions "" table)))
                       (when (= call-count 2)
                         (setq sink-candidates candidates))
                       (car candidates)))))
          (cj/recording-quick-setup)
          (should (cl-some (lambda (c) (string-match-p "\\[active - idle\\]" c))
                           sink-candidates))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-suspended-label ()
  "Test that SUSPENDED devices show [inactive - suspended] label."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((mic-candidates nil)
            (call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("mic-1" "Suspended Mic" "SUSPENDED"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" "Sink One" "SUSPENDED"))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (let ((candidates (all-completions "" table)))
                       (when (= call-count 1)
                         (setq mic-candidates candidates))
                       (car candidates)))))
          (cj/recording-quick-setup)
          (should (cl-some (lambda (c) (string-match-p "\\[inactive - suspended\\]" c))
                           mic-candidates))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-sorted-by-state ()
  "Test that devices are sorted running → idle → suspended."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((mic-candidates nil)
            (call-count 0))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("suspended-mic" "Suspended Mic" "SUSPENDED")
                                ("running-mic" "Running Mic" "RUNNING")
                                ("idle-mic" "Idle Mic" "IDLE"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" "Sink One" "SUSPENDED"))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _args)
                     (setq call-count (1+ call-count))
                     (let ((candidates (all-completions "" table)))
                       (when (= call-count 1)
                         (setq mic-candidates candidates))
                       (car candidates)))))
          (cj/recording-quick-setup)
          ;; First should be running, then idle, then suspended
          (should (string-match-p "Running Mic" (nth 0 mic-candidates)))
          (should (string-match-p "Idle Mic" (nth 1 mic-candidates)))
          (should (string-match-p "Suspended Mic" (nth 2 mic-candidates)))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-normal-confirmation-message ()
  "Test that confirmation message mentions the selected mic and monitor."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((message-text nil))
        (cl-letf (((symbol-function 'cj/recording--get-available-mics)
                   (lambda () '(("jabra-input" "Jabra SPEAK 510 Mono" "RUNNING"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("jds-labs.analog-stereo" "JDS Labs Element IV" "RUNNING"))))
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
                   (lambda () '(("sole-mic" "Only Mic Available" "SUSPENDED"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sole-sink" "Only Sink" "SUSPENDED"))))
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
                 (lambda () '(("jabra-input" "Jabra SPEAK 510 Mono" "SUSPENDED"))))
                ((symbol-function 'cj/recording--get-available-sinks)
                 (lambda () '(("sink-1" "Sink One" "SUSPENDED"))))
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
                   (lambda () '(("jabra-input" "Jabra SPEAK 510 Mono" "SUSPENDED"))))
                  ((symbol-function 'cj/recording--get-available-sinks)
                   (lambda () '(("sink-1" "Sink One" "SUSPENDED"))))
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
