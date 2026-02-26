;;; test-video-audio-recording-get-devices.el --- Tests for cj/recording-get-devices -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-get-devices function.
;; Tests device prompting and validation workflow.
;;
;; NOTE: This function goes straight into quick setup (mic selection)
;; when devices aren't configured — no confirmation dialog.

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

(ert-deftest test-video-audio-recording-get-devices-normal-returns-preset-devices ()
  "Test that already-configured devices are returned without prompting."
  (test-get-devices-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "preset-mic")
        (setq cj/recording-system-device "preset-monitor")
        (cl-letf (((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (let ((result (cj/recording-get-devices)))
            (should (consp result))
            (should (equal "preset-mic" (car result)))
            (should (equal "preset-monitor" (cdr result))))))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-calls-quick-setup ()
  "Test that function calls quick setup when devices not configured."
  (test-get-devices-setup)
  (unwind-protect
      (let ((quick-setup-called nil))
        (cl-letf (((symbol-function 'cj/recording-quick-setup)
                   (lambda ()
                     (setq quick-setup-called t)
                     (setq cj/recording-mic-device "quick-mic")
                     (setq cj/recording-system-device "quick-monitor")))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/recording-get-devices)
          (should quick-setup-called)))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-returns-cons-cell ()
  "Test that function returns (mic . monitor) cons cell."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-quick-setup)
                 (lambda ()
                   (setq cj/recording-mic-device "test-mic")
                   (setq cj/recording-system-device "test-monitor")))
                ((symbol-function 'cj/recording--validate-system-audio)
                 (lambda () nil)))
        (let ((result (cj/recording-get-devices)))
          (should (consp result))
          (should (equal "test-mic" (car result)))
          (should (equal "test-monitor" (cdr result)))))
    (test-get-devices-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-get-devices-boundary-only-mic-set-calls-setup ()
  "Test that function calls setup even when only mic is set."
  (test-get-devices-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "preset-mic")
        (setq cj/recording-system-device nil)
        (let ((quick-setup-called nil))
          (cl-letf (((symbol-function 'cj/recording-quick-setup)
                     (lambda ()
                       (setq quick-setup-called t)
                       (setq cj/recording-mic-device "new-mic")
                       (setq cj/recording-system-device "new-monitor")))
                    ((symbol-function 'cj/recording--validate-system-audio)
                     (lambda () nil)))
            (cj/recording-get-devices)
            (should quick-setup-called))))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-boundary-only-monitor-set-calls-setup ()
  "Test that function calls setup even when only monitor is set."
  (test-get-devices-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device nil)
        (setq cj/recording-system-device "preset-monitor")
        (let ((quick-setup-called nil))
          (cl-letf (((symbol-function 'cj/recording-quick-setup)
                     (lambda ()
                       (setq quick-setup-called t)
                       (setq cj/recording-mic-device "new-mic")
                       (setq cj/recording-system-device "new-monitor")))
                    ((symbol-function 'cj/recording--validate-system-audio)
                     (lambda () nil)))
            (cj/recording-get-devices)
            (should quick-setup-called))))
    (test-get-devices-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-get-devices-error-setup-fails-signals-error ()
  "Test that function signals error when setup fails to set devices."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-quick-setup)
                 (lambda () nil)))  ;; Setup fails - doesn't set devices
        (should-error (cj/recording-get-devices) :type 'user-error))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-error-message-mentions-setup-commands ()
  "Test that error message guides user to setup commands."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording-quick-setup)
                 (lambda () nil)))
        (condition-case err
            (cj/recording-get-devices)
          (user-error
           (should (string-match-p "C-; r s" (error-message-string err)))
           (should (string-match-p "C-; r s" (error-message-string err))))))
    (test-get-devices-teardown)))

(provide 'test-video-audio-recording-get-devices)
;;; test-video-audio-recording-get-devices.el ends here
