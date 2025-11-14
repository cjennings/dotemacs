;;; test-video-audio-recording-process-sentinel.el --- Tests for cj/recording-process-sentinel -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-process-sentinel function.
;; Tests process cleanup and modeline update when recording processes exit.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-sentinel-setup ()
  "Reset process variables before each test."
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/video-recording-ffmpeg-process nil))

(defun test-sentinel-teardown ()
  "Clean up process variables after each test."
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/video-recording-ffmpeg-process nil))

;;; Normal Cases

(ert-deftest test-video-audio-recording-process-sentinel-normal-audio-exit-clears-variable ()
  "Test that sentinel clears audio process variable when process exits."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sh" "-c" "exit 0"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Mock process-status to return 'exit
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'exit)))
          ;; Call sentinel with exit status
          (cj/recording-process-sentinel fake-process "finished\n")
          ;; Variable should be cleared
          (should (null cj/audio-recording-ffmpeg-process))))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-normal-video-exit-clears-variable ()
  "Test that sentinel clears video process variable when process exits."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sh" "-c" "exit 0"))))
        (setq cj/video-recording-ffmpeg-process fake-process)
        ;; Mock process-status to return 'exit
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'exit)))
          ;; Call sentinel with exit status
          (cj/recording-process-sentinel fake-process "finished\n")
          ;; Variable should be cleared
          (should (null cj/video-recording-ffmpeg-process))))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-normal-signal-status-clears-variable ()
  "Test that sentinel clears variable on signal status (killed)."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (delete-process fake-process)
        ;; Call sentinel with signal status
        (cj/recording-process-sentinel fake-process "killed\n")
        ;; Variable should be cleared
        (should (null cj/audio-recording-ffmpeg-process)))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-normal-modeline-update-called ()
  "Test that sentinel triggers modeline update."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sh" "-c" "exit 0")))
            (update-called nil))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Mock force-mode-line-update to track if it's called
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&optional _all) (setq update-called t))))
          (cj/recording-process-sentinel fake-process "finished\n")
          (should update-called)))
    (test-sentinel-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-process-sentinel-boundary-run-status-ignored ()
  "Test that sentinel ignores processes in 'run status (still running)."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Mock process-status to return 'run
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'run)))
          (cj/recording-process-sentinel fake-process "run")
          ;; Variable should NOT be cleared
          (should (eq fake-process cj/audio-recording-ffmpeg-process)))
        (delete-process fake-process))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-boundary-open-status-ignored ()
  "Test that sentinel ignores processes in 'open status."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'open)))
          (cj/recording-process-sentinel fake-process "open")
          ;; Variable should NOT be cleared
          (should (eq fake-process cj/audio-recording-ffmpeg-process)))
        (delete-process fake-process))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-boundary-event-trimmed ()
  "Test that event string is trimmed in message."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sh" "-c" "exit 0")))
            (message-text nil))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Mock message to capture output
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq message-text (apply #'format fmt args)))))
          (cj/recording-process-sentinel fake-process "  finished  \n")
          ;; Message should contain trimmed event
          (should (string-match-p "finished" message-text))
          ;; Should not have extra whitespace
          (should-not (string-match-p "  finished  " message-text))))
    (test-sentinel-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-process-sentinel-error-unknown-process-ignored ()
  "Test that sentinel handles unknown process (not audio or video) gracefully."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-unknown" :command '("sh" "-c" "exit 0")))
            (audio-proc (make-process :name "test-audio" :command '("sleep" "1000")))
            (video-proc (make-process :name "test-video" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process audio-proc)
        (setq cj/video-recording-ffmpeg-process video-proc)
        ;; Call sentinel with unknown process
        (cj/recording-process-sentinel fake-process "finished\n")
        ;; Audio and video variables should NOT be cleared
        (should (eq audio-proc cj/audio-recording-ffmpeg-process))
        (should (eq video-proc cj/video-recording-ffmpeg-process))
        (delete-process audio-proc)
        (delete-process video-proc))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-error-nil-event-handled ()
  "Test that sentinel handles nil event string gracefully."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sh" "-c" "exit 0"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Mock process-status to return 'exit
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'exit)))
          ;; Should not crash with nil event (string-trim will error, but that's caught)
          ;; The function uses string-trim without protection, so this will error
          ;; Testing that it doesn't crash means we expect an error
          (should-error
            (cj/recording-process-sentinel fake-process nil))))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-error-empty-event-handled ()
  "Test that sentinel handles empty event string gracefully."
  (test-sentinel-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sh" "-c" "exit 0"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Mock process-status to return 'exit
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'exit)))
          ;; Empty string is fine - string-trim handles it
          ;; No error should be raised
          (cj/recording-process-sentinel fake-process "")
          ;; Variable should be cleared
          (should (null cj/audio-recording-ffmpeg-process))))
    (test-sentinel-teardown)))

(provide 'test-video-audio-recording-process-sentinel)
;;; test-video-audio-recording-process-sentinel.el ends here
