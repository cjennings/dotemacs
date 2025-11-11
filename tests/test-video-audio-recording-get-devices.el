;;; test-video-audio-recording-get-devices.el --- Tests for cj/recording-get-devices -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-get-devices function.
;; Tests device prompting and validation workflow.
;;
;; NOTE: This function was refactored to use interactive prompts instead of
;; auto-detection. It now prompts the user with y-or-n-p and calls either
;; cj/recording-quick-setup-for-calls or cj/recording-select-devices.

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
        (let ((result (cj/recording-get-devices)))
          (should (consp result))
          (should (equal "preset-mic" (car result)))
          (should (equal "preset-monitor" (cdr result)))))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-prompts-when-not-configured ()
  "Test that function prompts user when devices not configured."
  (test-get-devices-setup)
  (unwind-protect
      (let ((prompt-called nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) (setq prompt-called t) t))
                  ((symbol-function 'cj/recording-quick-setup-for-calls)
                   (lambda ()
                     (setq cj/recording-mic-device "quick-mic")
                     (setq cj/recording-system-device "quick-monitor"))))
          (cj/recording-get-devices)
          (should prompt-called)))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-calls-quick-setup-on-yes ()
  "Test that function calls quick setup when user answers yes."
  (test-get-devices-setup)
  (unwind-protect
      (let ((quick-setup-called nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) t))
                  ((symbol-function 'cj/recording-quick-setup-for-calls)
                   (lambda ()
                     (setq quick-setup-called t)
                     (setq cj/recording-mic-device "quick-mic")
                     (setq cj/recording-system-device "quick-monitor"))))
          (cj/recording-get-devices)
          (should quick-setup-called)))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-calls-select-devices-on-no ()
  "Test that function calls manual selection when user answers no."
  (test-get-devices-setup)
  (unwind-protect
      (let ((select-called nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_prompt) nil))
                  ((symbol-function 'cj/recording-select-devices)
                   (lambda ()
                     (setq select-called t)
                     (setq cj/recording-mic-device "manual-mic")
                     (setq cj/recording-system-device "manual-monitor"))))
          (cj/recording-get-devices)
          (should select-called)))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-normal-returns-cons-cell ()
  "Test that function returns (mic . monitor) cons cell."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) t))
                ((symbol-function 'cj/recording-quick-setup-for-calls)
                 (lambda ()
                   (setq cj/recording-mic-device "test-mic")
                   (setq cj/recording-system-device "test-monitor"))))
        (let ((result (cj/recording-get-devices)))
          (should (consp result))
          (should (equal "test-mic" (car result)))
          (should (equal "test-monitor" (cdr result)))))
    (test-get-devices-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-get-devices-boundary-only-mic-set-prompts ()
  "Test that function prompts even when only mic is set."
  (test-get-devices-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "preset-mic")
        (setq cj/recording-system-device nil)
        (let ((prompt-called nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt) (setq prompt-called t) t))
                    ((symbol-function 'cj/recording-quick-setup-for-calls)
                     (lambda ()
                       (setq cj/recording-mic-device "new-mic")
                       (setq cj/recording-system-device "new-monitor"))))
            (cj/recording-get-devices)
            (should prompt-called))))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-boundary-only-monitor-set-prompts ()
  "Test that function prompts even when only monitor is set."
  (test-get-devices-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device nil)
        (setq cj/recording-system-device "preset-monitor")
        (let ((prompt-called nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt) (setq prompt-called t) t))
                    ((symbol-function 'cj/recording-quick-setup-for-calls)
                     (lambda ()
                       (setq cj/recording-mic-device "new-mic")
                       (setq cj/recording-system-device "new-monitor"))))
            (cj/recording-get-devices)
            (should prompt-called))))
    (test-get-devices-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-get-devices-error-setup-fails-signals-error ()
  "Test that function signals error when setup fails to set devices."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) t))
                ((symbol-function 'cj/recording-quick-setup-for-calls)
                 (lambda () nil)))  ;; Setup fails - doesn't set devices
        (should-error (cj/recording-get-devices) :type 'user-error))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-error-message-mentions-setup-commands ()
  "Test that error message guides user to setup commands."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) t))
                ((symbol-function 'cj/recording-quick-setup-for-calls)
                 (lambda () nil)))
        (condition-case err
            (cj/recording-get-devices)
          (user-error
           (should (string-match-p "C-; r c" (error-message-string err)))
           (should (string-match-p "C-; r s" (error-message-string err))))))
    (test-get-devices-teardown)))

(ert-deftest test-video-audio-recording-get-devices-error-select-devices-fails ()
  "Test that function signals error when manual selection fails."
  (test-get-devices-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) nil))
                ((symbol-function 'cj/recording-select-devices)
                 (lambda () nil)))  ;; Manual selection fails
        (should-error (cj/recording-get-devices) :type 'user-error))
    (test-get-devices-teardown)))

(provide 'test-video-audio-recording-get-devices)
;;; test-video-audio-recording-get-devices.el ends here
