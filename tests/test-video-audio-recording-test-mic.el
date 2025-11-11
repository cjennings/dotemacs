;;; test-video-audio-recording-test-mic.el --- Tests for cj/recording-test-mic -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-test-mic function.
;; Tests microphone testing functionality.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-mic-setup ()
  "Reset device variables before each test."
  (setq cj/recording-mic-device nil))

(defun test-mic-teardown ()
  "Clean up device variables after each test."
  (setq cj/recording-mic-device nil))

;;; Normal Cases

(ert-deftest test-video-audio-recording-test-mic-normal-creates-temp-wav-file ()
  "Test that function creates temp file with .wav extension."
  (test-mic-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "test-mic-device")
        (let ((temp-file nil))
          ;; Mock make-temp-file to capture filename
          (cl-letf (((symbol-function 'make-temp-file)
                     (lambda (prefix _dir-flag suffix)
                       (setq temp-file (concat prefix "12345" suffix))
                       temp-file))
                    ((symbol-function 'shell-command)
                     (lambda (_cmd) 0)))
            (cj/recording-test-mic)
            (should (string-match-p "\\.wav$" temp-file)))))
    (test-mic-teardown)))

(ert-deftest test-video-audio-recording-test-mic-normal-runs-ffmpeg-command ()
  "Test that function runs ffmpeg command with configured mic device."
  (test-mic-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "test-mic-device")
        (let ((commands nil))
          ;; Mock shell-command to capture all commands
          (cl-letf (((symbol-function 'shell-command)
                     (lambda (cmd) (push cmd commands) 0)))
            (cj/recording-test-mic)
            (should (= 2 (length commands)))
            ;; First command should be ffmpeg (stored last in list due to push)
            (let ((ffmpeg-cmd (cadr commands)))
              (should (stringp ffmpeg-cmd))
              (should (string-match-p "ffmpeg" ffmpeg-cmd))
              (should (string-match-p "test-mic-device" ffmpeg-cmd))
              (should (string-match-p "-t 5" ffmpeg-cmd))))))
    (test-mic-teardown)))

(ert-deftest test-video-audio-recording-test-mic-normal-runs-ffplay-for-playback ()
  "Test that function runs ffplay for playback."
  (test-mic-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "test-mic-device")
        (let ((commands nil))
          ;; Capture all shell commands
          (cl-letf (((symbol-function 'shell-command)
                     (lambda (cmd) (push cmd commands) 0)))
            (cj/recording-test-mic)
            (should (= 2 (length commands)))
            ;; Second command should be ffplay
            (should (string-match-p "ffplay" (car commands)))
            (should (string-match-p "-autoexit" (car commands))))))
    (test-mic-teardown)))

(ert-deftest test-video-audio-recording-test-mic-normal-displays-messages ()
  "Test that function displays appropriate messages to user."
  (test-mic-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "test-mic-device")
        (let ((messages nil))
          ;; Capture messages
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                    ((symbol-function 'shell-command)
                     (lambda (_cmd) 0)))
            (cj/recording-test-mic)
            (should (>= (length messages) 3))
            ;; Check for recording message
            (should (cl-some (lambda (msg) (string-match-p "Recording.*SPEAK NOW" msg)) messages))
            ;; Check for playback message
            (should (cl-some (lambda (msg) (string-match-p "Playing back" msg)) messages))
            ;; Check for complete message
            (should (cl-some (lambda (msg) (string-match-p "complete" msg)) messages)))))
    (test-mic-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-test-mic-error-no-mic-configured-signals-error ()
  "Test that function signals user-error when mic device is not configured."
  (test-mic-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device nil)
        (should-error (cj/recording-test-mic) :type 'user-error))
    (test-mic-teardown)))

(ert-deftest test-video-audio-recording-test-mic-error-message-mentions-setup ()
  "Test that error message guides user to run setup."
  (test-mic-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device nil)
        (condition-case err
            (cj/recording-test-mic)
          (user-error
           (should (string-match-p "C-; r c" (error-message-string err))))))
    (test-mic-teardown)))

(ert-deftest test-video-audio-recording-test-mic-error-ffmpeg-failure-handled ()
  "Test that ffmpeg command failure is handled gracefully."
  (test-mic-setup)
  (unwind-protect
      (progn
        (setq cj/recording-mic-device "test-mic-device")
        ;; Mock shell-command to fail
        (cl-letf (((symbol-function 'shell-command)
                   (lambda (_cmd) 1)))  ;; Non-zero exit code
          ;; Should complete without crashing (ffmpeg errors are ignored)
          ;; No error is raised - function just completes
          (cj/recording-test-mic)
          ;; Test passes if we get here
          (should t)))
    (test-mic-teardown)))

(provide 'test-video-audio-recording-test-mic)
;;; test-video-audio-recording-test-mic.el ends here
