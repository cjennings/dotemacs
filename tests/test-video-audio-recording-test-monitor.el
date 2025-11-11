;;; test-video-audio-recording-test-monitor.el --- Tests for cj/recording-test-monitor -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-test-monitor function.
;; Tests system audio monitor testing functionality.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-monitor-setup ()
  "Reset device variables before each test."
  (setq cj/recording-system-device nil))

(defun test-monitor-teardown ()
  "Clean up device variables after each test."
  (setq cj/recording-system-device nil))

;;; Normal Cases

(ert-deftest test-video-audio-recording-test-monitor-normal-creates-temp-wav-file ()
  "Test that function creates temp file with .wav extension."
  (test-monitor-setup)
  (unwind-protect
      (progn
        (setq cj/recording-system-device "test-monitor-device")
        (let ((temp-file nil))
          ;; Mock make-temp-file to capture filename
          (cl-letf (((symbol-function 'make-temp-file)
                     (lambda (prefix _dir-flag suffix)
                       (setq temp-file (concat prefix "12345" suffix))
                       temp-file))
                    ((symbol-function 'shell-command)
                     (lambda (_cmd) 0)))
            (cj/recording-test-monitor)
            (should (string-match-p "monitor-test-" temp-file))
            (should (string-match-p "\\.wav$" temp-file)))))
    (test-monitor-teardown)))

(ert-deftest test-video-audio-recording-test-monitor-normal-runs-ffmpeg-command ()
  "Test that function runs ffmpeg command with configured monitor device."
  (test-monitor-setup)
  (unwind-protect
      (progn
        (setq cj/recording-system-device "test-monitor-device")
        (let ((commands nil))
          ;; Mock shell-command to capture all commands
          (cl-letf (((symbol-function 'shell-command)
                     (lambda (cmd) (push cmd commands) 0)))
            (cj/recording-test-monitor)
            (should (= 2 (length commands)))
            ;; First command should be ffmpeg (stored last in list due to push)
            (let ((ffmpeg-cmd (cadr commands)))
              (should (stringp ffmpeg-cmd))
              (should (string-match-p "ffmpeg" ffmpeg-cmd))
              (should (string-match-p "test-monitor-device" ffmpeg-cmd))
              (should (string-match-p "-t 5" ffmpeg-cmd))))))
    (test-monitor-teardown)))

(ert-deftest test-video-audio-recording-test-monitor-normal-runs-ffplay-for-playback ()
  "Test that function runs ffplay for playback."
  (test-monitor-setup)
  (unwind-protect
      (progn
        (setq cj/recording-system-device "test-monitor-device")
        (let ((commands nil))
          ;; Capture all shell commands
          (cl-letf (((symbol-function 'shell-command)
                     (lambda (cmd) (push cmd commands) 0)))
            (cj/recording-test-monitor)
            (should (= 2 (length commands)))
            ;; Second command should be ffplay
            (should (string-match-p "ffplay" (car commands)))
            (should (string-match-p "-autoexit" (car commands))))))
    (test-monitor-teardown)))

(ert-deftest test-video-audio-recording-test-monitor-normal-displays-messages ()
  "Test that function displays appropriate messages to user."
  (test-monitor-setup)
  (unwind-protect
      (progn
        (setq cj/recording-system-device "test-monitor-device")
        (let ((messages nil))
          ;; Capture messages
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                    ((symbol-function 'shell-command)
                     (lambda (_cmd) 0)))
            (cj/recording-test-monitor)
            (should (>= (length messages) 3))
            ;; Check for recording message
            (should (cl-some (lambda (msg) (string-match-p "Recording.*PLAY SOMETHING" msg)) messages))
            ;; Check for playback message
            (should (cl-some (lambda (msg) (string-match-p "Playing back" msg)) messages))
            ;; Check for complete message
            (should (cl-some (lambda (msg) (string-match-p "complete" msg)) messages)))))
    (test-monitor-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-test-monitor-error-no-monitor-configured-signals-error ()
  "Test that function signals user-error when monitor device is not configured."
  (test-monitor-setup)
  (unwind-protect
      (progn
        (setq cj/recording-system-device nil)
        (should-error (cj/recording-test-monitor) :type 'user-error))
    (test-monitor-teardown)))

(ert-deftest test-video-audio-recording-test-monitor-error-message-mentions-setup ()
  "Test that error message guides user to run setup."
  (test-monitor-setup)
  (unwind-protect
      (progn
        (setq cj/recording-system-device nil)
        (condition-case err
            (cj/recording-test-monitor)
          (user-error
           (should (string-match-p "C-; r c" (error-message-string err))))))
    (test-monitor-teardown)))

(ert-deftest test-video-audio-recording-test-monitor-error-ffmpeg-failure-handled ()
  "Test that ffmpeg command failure is handled gracefully."
  (test-monitor-setup)
  (unwind-protect
      (progn
        (setq cj/recording-system-device "test-monitor-device")
        ;; Mock shell-command to fail
        (cl-letf (((symbol-function 'shell-command)
                   (lambda (_cmd) 1)))  ;; Non-zero exit code
          ;; Should complete without crashing (ffmpeg errors are ignored)
          ;; No error is raised - function just completes
          (cj/recording-test-monitor)
          ;; Test passes if we get here
          (should t)))
    (test-monitor-teardown)))

(provide 'test-video-audio-recording-test-monitor)
;;; test-video-audio-recording-test-monitor.el ends here
