;;; test-video-audio-recording-ffmpeg-functions.el --- Tests for ffmpeg recording functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/ffmpeg-record-video, cj/ffmpeg-record-audio,
;; cj/video-recording-stop, and cj/audio-recording-stop functions.
;; Tests process creation, sentinel attachment, and cleanup.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Stub directory variables
(defvar video-recordings-dir "/tmp/video-recordings/")
(defvar audio-recordings-dir "/tmp/audio-recordings/")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-ffmpeg-setup ()
  "Reset all variables before each test."
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device "test-mic-device")
  (setq cj/recording-system-device "test-monitor-device")
  (setq cj/recording-mic-boost 2.0)
  (setq cj/recording-system-volume 0.5))

(defun test-ffmpeg-teardown ()
  "Clean up after each test."
  (when cj/video-recording-ffmpeg-process
    (ignore-errors (delete-process cj/video-recording-ffmpeg-process)))
  (when cj/audio-recording-ffmpeg-process
    (ignore-errors (delete-process cj/audio-recording-ffmpeg-process)))
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/audio-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Video Recording - Normal Cases

(ert-deftest test-video-audio-recording-ffmpeg-record-video-normal-creates-process ()
  "Test that video recording creates a process."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((process-created nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (setq process-created t)
                     (make-process :name "fake-video" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should process-created)
          (should cj/video-recording-ffmpeg-process)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-video-normal-attaches-sentinel ()
  "Test that video recording attaches sentinel to process."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((sentinel-attached nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (_proc sentinel)
                     (should (eq sentinel #'cj/recording-process-sentinel))
                     (setq sentinel-attached t))))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should sentinel-attached)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-video-normal-updates-modeline ()
  "Test that video recording triggers modeline update."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((update-called nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'force-mode-line-update)
                   (lambda (&optional _all) (setq update-called t))))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should update-called)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-video-normal-uses-device-settings ()
  "Test that video recording uses configured devices and volume settings."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-video" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should (string-match-p "test-mic-device" command))
          (should (string-match-p "test-monitor-device" command))
          (should (string-match-p "2\\.0" command))  ; mic boost
          (should (string-match-p "0\\.5" command))))  ; system volume
    (test-ffmpeg-teardown)))

;;; Audio Recording - Normal Cases

(ert-deftest test-video-audio-recording-ffmpeg-record-audio-normal-creates-process ()
  "Test that audio recording creates a process."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((process-created nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (setq process-created t)
                     (make-process :name "fake-audio" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          (should process-created)
          (should cj/audio-recording-ffmpeg-process)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-audio-normal-attaches-sentinel ()
  "Test that audio recording attaches sentinel to process."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((sentinel-attached nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (make-process :name "fake-audio" :command '("sleep" "1000"))))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (_proc sentinel)
                     (should (eq sentinel #'cj/recording-process-sentinel))
                     (setq sentinel-attached t))))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          (should sentinel-attached)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-audio-normal-updates-modeline ()
  "Test that audio recording triggers modeline update."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((update-called nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (make-process :name "fake-audio" :command '("sleep" "1000"))))
                  ((symbol-function 'force-mode-line-update)
                   (lambda (&optional _all) (setq update-called t))))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          (should update-called)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-audio-normal-creates-m4a-file ()
  "Test that audio recording creates .m4a file."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((command nil))
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer cmd)
                     (setq command cmd)
                     (make-process :name "fake-audio" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          (should (string-match-p "\\.m4a" command))))
    (test-ffmpeg-teardown)))

;;; Stop Functions - Normal Cases

(ert-deftest test-video-audio-recording-video-stop-normal-interrupts-process ()
  "Test that stopping video recording interrupts the process."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (interrupt-called nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'interrupt-process)
                   (lambda (_proc) (setq interrupt-called t))))
          (cj/video-recording-stop)
          (should interrupt-called))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-video-stop-normal-clears-variable ()
  "Test that stopping video recording clears the process variable."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000"))))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cj/video-recording-stop)
        (should (null cj/video-recording-ffmpeg-process))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-video-stop-normal-updates-modeline ()
  "Test that stopping video recording updates modeline."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (update-called nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&optional _all) (setq update-called t))))
          (cj/video-recording-stop)
          (should update-called))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-audio-stop-normal-interrupts-process ()
  "Test that stopping audio recording interrupts the process."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000")))
            (interrupt-called nil))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'interrupt-process)
                   (lambda (_proc) (setq interrupt-called t))))
          (cj/audio-recording-stop)
          (should interrupt-called))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-audio-stop-normal-clears-variable ()
  "Test that stopping audio recording clears the process variable."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cj/audio-recording-stop)
        (should (null cj/audio-recording-ffmpeg-process))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-video-stop-boundary-no-process-displays-message ()
  "Test that stopping when no video recording shows message."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((message-text nil))
        (setq cj/video-recording-ffmpeg-process nil)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq message-text (apply #'format fmt args)))))
          (cj/video-recording-stop)
          (should (string-match-p "No video recording" message-text))))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-audio-stop-boundary-no-process-displays-message ()
  "Test that stopping when no audio recording shows message."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((message-text nil))
        (setq cj/audio-recording-ffmpeg-process nil)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq message-text (apply #'format fmt args)))))
          (cj/audio-recording-stop)
          (should (string-match-p "No audio recording" message-text))))
    (test-ffmpeg-teardown)))

;;; Error Cases

(ert-deftest test-video-audio-recording-video-stop-error-interrupt-process-fails ()
  "Test that video stop handles interrupt-process failure gracefully."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (error-raised nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'interrupt-process)
                   (lambda (_proc) (error "Interrupt failed"))))
          ;; Should handle the error without crashing
          (condition-case err
              (cj/video-recording-stop)
            (error (setq error-raised t)))
          ;; Error should propagate (function doesn't catch it)
          (should error-raised))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-audio-stop-error-interrupt-process-fails ()
  "Test that audio stop handles interrupt-process failure gracefully."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000")))
            (error-raised nil))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'interrupt-process)
                   (lambda (_proc) (error "Interrupt failed"))))
          ;; Should handle the error without crashing
          (condition-case err
              (cj/audio-recording-stop)
            (error (setq error-raised t)))
          ;; Error should propagate (function doesn't catch it)
          (should error-raised))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-video-stop-error-dead-process-raises-error ()
  "Test that video stop raises error if process is already dead.
This documents current behavior - interrupt-process on dead process errors.
The sentinel should clear the variable before this happens in practice."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000"))))
        (setq cj/video-recording-ffmpeg-process fake-process)
        ;; Kill process before calling stop
        (delete-process fake-process)
        (sit-for 0.1)
        ;; Calling stop on dead process raises error
        (should-error (cj/video-recording-stop)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-audio-stop-error-dead-process-raises-error ()
  "Test that audio stop raises error if process is already dead.
This documents current behavior - interrupt-process on dead process errors.
The sentinel should clear the variable before this happens in practice."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000"))))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        ;; Kill process before calling stop
        (delete-process fake-process)
        (sit-for 0.1)
        ;; Calling stop on dead process raises error
        (should-error (cj/audio-recording-stop)))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-video-boundary-skips-if-already-recording ()
  "Test that video recording skips if already in progress."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (start-called nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (setq start-called t)
                     (make-process :name "fake-video2" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-video video-recordings-dir)
          ;; Should NOT start a new process
          (should-not start-called))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(ert-deftest test-video-audio-recording-ffmpeg-record-audio-boundary-skips-if-already-recording ()
  "Test that audio recording skips if already in progress."
  (test-ffmpeg-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000")))
            (start-called nil))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (setq start-called t)
                     (make-process :name "fake-audio2" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-audio audio-recordings-dir)
          ;; Should NOT start a new process
          (should-not start-called))
        (delete-process fake-process))
    (test-ffmpeg-teardown)))

(provide 'test-video-audio-recording-ffmpeg-functions)
;;; test-video-audio-recording-ffmpeg-functions.el ends here
