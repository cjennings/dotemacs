;;; test-video-audio-recording-select-device.el --- Tests for cj/recording-select-device -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-select-device function.
;; Tests interactive device selection with filtering.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording-select-device-normal-returns-selected-mic ()
  "Test that function returns selected microphone device."
  (let ((sources '(("alsa_input.pci-device" "PipeWire" "SUSPENDED")
                   ("alsa_output.pci-device.monitor" "PipeWire" "SUSPENDED"))))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 ;; Select the first choice
                 (caar choices))))
      (let ((result (cj/recording-select-device "Select mic: " 'mic)))
        (should (stringp result))
        (should (equal "alsa_input.pci-device" result))))))

(ert-deftest test-video-audio-recording-select-device-normal-returns-selected-monitor ()
  "Test that function returns selected monitor device."
  (let ((sources '(("alsa_input.pci-device" "PipeWire" "SUSPENDED")
                   ("alsa_output.pci-device.monitor" "PipeWire" "SUSPENDED"))))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 (caar choices))))
      (let ((result (cj/recording-select-device "Select monitor: " 'monitor)))
        (should (stringp result))
        (should (equal "alsa_output.pci-device.monitor" result))))))

(ert-deftest test-video-audio-recording-select-device-normal-filters-monitors-for-mic ()
  "Test that function filters out monitor devices when selecting mic."
  (let ((sources '(("alsa_input.pci-device" "PipeWire" "SUSPENDED")
                   ("alsa_output.pci-device.monitor" "PipeWire" "SUSPENDED")
                   ("bluez_input.00:1B:66" "PipeWire" "RUNNING")))
        (presented-choices nil))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 (setq presented-choices choices)
                 (caar choices))))
      (cj/recording-select-device "Select mic: " 'mic)
      ;; Should have 2 mic devices (not the monitor)
      (should (= 2 (length presented-choices)))
      (should-not (cl-some (lambda (choice) (string-match-p "\\.monitor" (car choice)))
                           presented-choices)))))

(ert-deftest test-video-audio-recording-select-device-normal-filters-non-monitors-for-monitor ()
  "Test that function filters out non-monitor devices when selecting monitor."
  (let ((sources '(("alsa_input.pci-device" "PipeWire" "SUSPENDED")
                   ("alsa_output.pci-device.monitor" "PipeWire" "SUSPENDED")
                   ("bluez_output.00_1B_66.1.monitor" "PipeWire" "RUNNING")))
        (presented-choices nil))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 (setq presented-choices choices)
                 (caar choices))))
      (cj/recording-select-device "Select monitor: " 'monitor)
      ;; Should have 2 monitor devices (not the input)
      (should (= 2 (length presented-choices)))
      (should (cl-every (lambda (choice) (string-match-p "\\.monitor" (car choice)))
                        presented-choices)))))

(ert-deftest test-video-audio-recording-select-device-normal-shows-friendly-state ()
  "Test that function shows friendly state in choices."
  (let ((sources '(("alsa_input.pci-device" "PipeWire" "SUSPENDED")))
        (presented-choices nil))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 (setq presented-choices choices)
                 (caar choices))))
      (cj/recording-select-device "Select mic: " 'mic)
      ;; Choice should contain "Ready" (friendly for SUSPENDED)
      (should (string-match-p "Ready" (caar presented-choices))))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-select-device-boundary-single-device ()
  "Test that function works with single device."
  (let ((sources '(("alsa_input.pci-device" "PipeWire" "SUSPENDED"))))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 (caar choices))))
      (let ((result (cj/recording-select-device "Select mic: " 'mic)))
        (should (equal "alsa_input.pci-device" result))))))

(ert-deftest test-video-audio-recording-select-device-boundary-multiple-states ()
  "Test that function handles devices in different states."
  (let ((sources '(("alsa_input.device1" "PipeWire" "SUSPENDED")
                   ("alsa_input.device2" "PipeWire" "RUNNING")
                   ("alsa_input.device3" "PipeWire" "IDLE")))
        (presented-choices nil))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 (setq presented-choices choices)
                 (caar choices))))
      (cj/recording-select-device "Select mic: " 'mic)
      ;; All three should be presented
      (should (= 3 (length presented-choices)))
      ;; Check that friendly states appear
      (let ((choice-text (mapconcat #'car presented-choices " ")))
        (should (string-match-p "Ready\\|Active" choice-text))))))

;;; Error Cases

(ert-deftest test-video-audio-recording-select-device-error-no-mic-devices-signals-error ()
  "Test that function signals user-error when no mic devices found."
  (let ((sources '(("alsa_output.pci-device.monitor" "PipeWire" "SUSPENDED"))))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources)))
      (should-error (cj/recording-select-device "Select mic: " 'mic) :type 'user-error))))

(ert-deftest test-video-audio-recording-select-device-error-no-monitor-devices-signals-error ()
  "Test that function signals user-error when no monitor devices found."
  (let ((sources '(("alsa_input.pci-device" "PipeWire" "SUSPENDED"))))
    (cl-letf (((symbol-function 'cj/recording-parse-sources)
               (lambda () sources)))
      (should-error (cj/recording-select-device "Select monitor: " 'monitor) :type 'user-error))))

(ert-deftest test-video-audio-recording-select-device-error-empty-source-list ()
  "Test that function signals user-error when source list is empty."
  (cl-letf (((symbol-function 'cj/recording-parse-sources)
             (lambda () nil)))
    (should-error (cj/recording-select-device "Select mic: " 'mic) :type 'user-error)))

(ert-deftest test-video-audio-recording-select-device-error-message-mentions-device-type ()
  "Test that error message mentions the device type being searched for."
  (cl-letf (((symbol-function 'cj/recording-parse-sources)
             (lambda () nil)))
    (condition-case err
        (cj/recording-select-device "Select mic: " 'mic)
      (user-error
       (should (string-match-p "input" (error-message-string err)))))
    (condition-case err
        (cj/recording-select-device "Select monitor: " 'monitor)
      (user-error
       (should (string-match-p "monitor" (error-message-string err)))))))

(provide 'test-video-audio-recording-select-device)
;;; test-video-audio-recording-select-device.el ends here
