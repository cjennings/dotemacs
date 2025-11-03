;;; test-video-audio-recording-get-devices.el --- Tests for cj/recording-get-devices -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-get-devices function.
;; Tests device auto-detection fallback logic.
;;
;; Note: This function has interactive prompts, but we test the core logic paths
;; without mocking y-or-n-p. We focus on testing:
;; - Already-set devices (no auto-detection needed)
;; - Successful auto-detection
;; - Failed auto-detection → error

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-get-devices-setup ()
  "Reset device variables before each test."
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

(defun test-get-devices-teardown ()
  "Clean up device variables after each test."
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Normal Cases

(ert-deftest test-video-audio-recording-get-devices-normal-already-set-returns-devices ()
  "Test that already-set devices are returned without auto-detection."
  (test-get-devices-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "test-mic")
        (setq cj/recording-system-device "test-monitor")
        (let ((result (cj/recording-get-devices)))
          (should (consp result))
          (should (equal "test-mic" (car result)))
          (should (equal "test-monitor" (cdr result)))))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-auto-detect-success ()
  "Test that auto-detection succeeds and returns devices."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-detect-mic-device)
                 (lambda () "auto-detected-mic"))
                ((symbol-function 'cj/recording-detect-system-device)
                 (lambda () "auto-detected-monitor")))
        (let ((result (cj/recording-get-devices)))
          (should (consp result))
          (should (equal "auto-detected-mic" (car result)))
          (should (equal "auto-detected-monitor" (cdr result)))
          ;; Verify variables were set
          (should (equal "auto-detected-mic" cj/recording-mic-device))
          (should (equal "auto-detected-monitor" cj/recording-system-device))))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-partial-auto-detect ()
  "Test when only one device is already set, only the other is auto-detected."
  (test-get-devices-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "preset-mic")
        (cl-letf (((symbol-function 'cj/recording-detect-system-device)
                   (lambda () "auto-detected-monitor")))
          (let ((result (cj/recording-get-devices)))
            (should (consp result))
            (should (equal "preset-mic" (car result)))
            (should (equal "auto-detected-monitor" (cdr result))))))
    (test-get-devices-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-get-devices-error-auto-detect-fails-signals-error ()
  "Test that failed auto-detection signals user-error.
When auto-detection fails and user doesn't manually select, function errors."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-detect-mic-device)
                 (lambda () nil))
                ((symbol-function 'cj/recording-detect-system-device)
                 (lambda () nil))
                ;; Mock y-or-n-p to say no to manual selection
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) nil)))
        (should-error (cj/recording-get-devices) :type 'user-error))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-error-only-mic-detected-signals-error ()
  "Test that detecting only mic (no monitor) signals error."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-detect-mic-device)
                 (lambda () "detected-mic"))
                ((symbol-function 'cj/recording-detect-system-device)
                 (lambda () nil))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) nil)))
        (should-error (cj/recording-get-devices) :type 'user-error))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-error-only-monitor-detected-signals-error ()
  "Test that detecting only monitor (no mic) signals error."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-detect-mic-device)
                 (lambda () nil))
                ((symbol-function 'cj/recording-detect-system-device)
                 (lambda () "detected-monitor"))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) nil)))
        (should-error (cj/recording-get-devices) :type 'user-error))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-error-message-mentions-select-devices ()
  "Test that error message guides user to manual selection command."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-detect-mic-device)
                 (lambda () nil))
                ((symbol-function 'cj/recording-detect-system-device)
                 (lambda () nil))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) nil)))
        (condition-case err
            (cj/recording-get-devices)
          (user-error
           (should (string-match-p "cj/recording-select-devices" (error-message-string err))))))
    (test-get-devices-teardown)))

(provide 'test-video-audio-recording-get-devices)
;;; test-video-audio-recording-get-devices.el ends here
