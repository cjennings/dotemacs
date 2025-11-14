;;; test-video-audio-recording-quick-setup-for-calls.el --- Tests for cj/recording-quick-setup-for-calls -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-quick-setup-for-calls function.
;; Tests quick device setup workflow for call recording.

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

(ert-deftest test-video-audio-recording-quick-setup-for-calls-normal-sets-both-devices ()
  "Test that function sets both mic and system device variables."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((grouped-devices '(("Bluetooth Headset" . ("bluez_input.00:1B:66" . "bluez_output.00_1B_66.monitor")))))
        (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                   (lambda () grouped-devices))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _choices &rest _args) "Bluetooth Headset")))
          (cj/recording-quick-setup-for-calls)
          (should (equal "bluez_input.00:1B:66" cj/recording-mic-device))
          (should (equal "bluez_output.00_1B_66.monitor" cj/recording-system-device))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-for-calls-normal-presents-friendly-names ()
  "Test that function presents friendly device names to user."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((grouped-devices '(("Jabra SPEAK 510 USB" . ("usb-input" . "usb-monitor"))
                              ("Built-in Laptop Audio" . ("pci-input" . "pci-monitor"))))
            (presented-choices nil))
        (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                   (lambda () grouped-devices))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _args)
                     (setq presented-choices choices)
                     (car choices))))
          (cj/recording-quick-setup-for-calls)
          (should (member "Jabra SPEAK 510 USB" presented-choices))
          (should (member "Built-in Laptop Audio" presented-choices))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-for-calls-normal-displays-confirmation ()
  "Test that function displays confirmation message with device details."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((grouped-devices '(("Bluetooth Headset" . ("bluez_input.00:1B:66" . "bluez_output.00_1B_66.monitor"))))
            (message-text nil))
        (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                   (lambda () grouped-devices))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _choices &rest _args) "Bluetooth Headset"))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq message-text (apply #'format fmt args)))))
          (cj/recording-quick-setup-for-calls)
          (should (string-match-p "Call recording ready" message-text))
          (should (string-match-p "Bluetooth Headset" message-text))))
    (test-quick-setup-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-quick-setup-for-calls-boundary-single-device-no-prompt ()
  "Test that with single device, selection still happens."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((grouped-devices '(("Built-in Laptop Audio" . ("pci-input" . "pci-monitor")))))
        (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                   (lambda () grouped-devices))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _choices &rest _args) "Built-in Laptop Audio")))
          (cj/recording-quick-setup-for-calls)
          (should (equal "pci-input" cj/recording-mic-device))
          (should (equal "pci-monitor" cj/recording-system-device))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-for-calls-boundary-device-name-with-special-chars ()
  "Test that device names with special characters are handled correctly."
  (test-quick-setup-setup)
  (unwind-protect
      (let ((grouped-devices '(("Device (USB-C)" . ("special-input" . "special-monitor")))))
        (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                   (lambda () grouped-devices))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _choices &rest _args) "Device (USB-C)")))
          (cj/recording-quick-setup-for-calls)
          (should (equal "special-input" cj/recording-mic-device))
          (should (equal "special-monitor" cj/recording-system-device))))
    (test-quick-setup-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-quick-setup-for-calls-error-no-devices-signals-error ()
  "Test that function signals user-error when no complete devices are found."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                 (lambda () nil)))
        (should-error (cj/recording-quick-setup-for-calls) :type 'user-error))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-for-calls-error-message-mentions-both-devices ()
  "Test that error message mentions need for both mic and monitor."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                 (lambda () nil)))
        (condition-case err
            (cj/recording-quick-setup-for-calls)
          (user-error
           (should (string-match-p "both mic and monitor" (error-message-string err))))))
    (test-quick-setup-teardown)))

(ert-deftest test-video-audio-recording-quick-setup-for-calls-error-empty-device-list ()
  "Test that empty device list from grouping is handled gracefully."
  (test-quick-setup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-group-devices-by-hardware)
                 (lambda () '())))
        (should-error (cj/recording-quick-setup-for-calls) :type 'user-error))
    (test-quick-setup-teardown)))

(provide 'test-video-audio-recording-quick-setup-for-calls)
;;; test-video-audio-recording-quick-setup-for-calls.el ends here
