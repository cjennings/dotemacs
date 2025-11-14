;;; test-video-audio-recording-modeline-indicator.el --- Tests for cj/recording-modeline-indicator -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-modeline-indicator function.
;; Tests modeline indicator display based on active recording processes.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-modeline-indicator-setup ()
  "Reset process variables before each test."
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/video-recording-ffmpeg-process nil))

(defun test-modeline-indicator-teardown ()
  "Clean up process variables after each test."
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/video-recording-ffmpeg-process nil))

;;; Normal Cases

(ert-deftest test-video-audio-recording-modeline-indicator-normal-no-processes-returns-empty ()
  "Test that indicator returns empty string when no processes are active."
  (test-modeline-indicator-setup)
  (unwind-protect
      (let ((result (cj/recording-modeline-indicator)))
        (should (stringp result))
        (should (equal "" result)))
    (test-modeline-indicator-teardown)))

(ert-deftest test-video-audio-recording-modeline-indicator-normal-audio-only-shows-audio ()
  "Test that indicator shows audio when only audio process is active."
  (test-modeline-indicator-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (let ((result (cj/recording-modeline-indicator)))
          (should (equal " 🔴Audio " result)))
        (delete-process fake-process))
    (test-modeline-indicator-teardown)))

(ert-deftest test-video-audio-recording-modeline-indicator-normal-video-only-shows-video ()
  "Test that indicator shows video when only video process is active."
  (test-modeline-indicator-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000"))))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (let ((result (cj/recording-modeline-indicator)))
          (should (equal " 🔴Video " result)))
        (delete-process fake-process))
    (test-modeline-indicator-teardown)))

(ert-deftest test-video-audio-recording-modeline-indicator-normal-both-shows-combined ()
  "Test that indicator shows A+V when both processes are active."
  (test-modeline-indicator-setup)
  (unwind-protect
      (let ((audio-proc (make-process :name "test-audio" :command '("sleep" "1000")))
            (video-proc (make-process :name "test-video" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process audio-proc)
        (setq cj/video-recording-ffmpeg-process video-proc)
        (let ((result (cj/recording-modeline-indicator)))
          (should (equal " 🔴A+V " result)))
        (delete-process audio-proc)
        (delete-process video-proc))
    (test-modeline-indicator-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-modeline-indicator-boundary-dead-audio-process-returns-empty ()
  "Test that indicator returns empty string when audio process variable is set but process is dead."
  (test-modeline-indicator-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Kill the process
        (delete-process fake-process)
        ;; Wait for process to be fully dead
        (sit-for 0.1)
        (let ((result (cj/recording-modeline-indicator)))
          (should (equal "" result))))
    (test-modeline-indicator-teardown)))

(ert-deftest test-video-audio-recording-modeline-indicator-boundary-dead-video-process-returns-empty ()
  "Test that indicator returns empty string when video process variable is set but process is dead."
  (test-modeline-indicator-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000"))))
        (setq cj/video-recording-ffmpeg-process fake-process)
        ;; Kill the process
        (delete-process fake-process)
        ;; Wait for process to be fully dead
        (sit-for 0.1)
        (let ((result (cj/recording-modeline-indicator)))
          (should (equal "" result))))
    (test-modeline-indicator-teardown)))

(ert-deftest test-video-audio-recording-modeline-indicator-boundary-one-dead-one-alive-shows-alive ()
  "Test that only the alive process shows when one is dead and one is alive."
  (test-modeline-indicator-setup)
  (unwind-protect
      (let ((dead-proc (make-process :name "test-dead" :command '("sleep" "1000")))
            (alive-proc (make-process :name "test-alive" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process dead-proc)
        (setq cj/video-recording-ffmpeg-process alive-proc)
        (delete-process dead-proc)
        (sit-for 0.1)
        (let ((result (cj/recording-modeline-indicator)))
          (should (equal " 🔴Video " result)))
        (delete-process alive-proc))
    (test-modeline-indicator-teardown)))

(ert-deftest test-video-audio-recording-modeline-indicator-boundary-nil-process-variables ()
  "Test that nil process variables are handled gracefully."
  (test-modeline-indicator-setup)
  (unwind-protect
      (progn
        (setq cj/audio-recording-ffmpeg-process nil)
        (setq cj/video-recording-ffmpeg-process nil)
        (let ((result (cj/recording-modeline-indicator)))
          (should (equal "" result))))
    (test-modeline-indicator-teardown)))

(provide 'test-video-audio-recording-modeline-indicator)
;;; test-video-audio-recording-modeline-indicator.el ends here
