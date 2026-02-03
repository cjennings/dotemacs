;;; test-video-audio-recording-process-cleanup.el --- Tests for recording process cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that verify proper cleanup of recording processes, especially
;; wf-recorder on Wayland which can become orphaned if not explicitly killed.
;;
;; Unit tests verify pkill is called at the right times.
;; Integration tests verify no orphan processes remain after stop.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(defvar video-recordings-dir "/tmp/video-recordings/")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Setup and Teardown

(defun test-cleanup-setup ()
  "Reset all variables before each test."
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device "test-mic-device")
  (setq cj/recording-system-device "test-monitor-device")
  (setq cj/recording-mic-boost 2.0)
  (setq cj/recording-system-volume 1.0))

(defun test-cleanup-teardown ()
  "Clean up after each test."
  (when cj/video-recording-ffmpeg-process
    (ignore-errors (delete-process cj/video-recording-ffmpeg-process)))
  (setq cj/video-recording-ffmpeg-process nil)
  (setq cj/recording-mic-device nil)
  (setq cj/recording-system-device nil))

;;; Unit Tests - Verify pkill is called

(ert-deftest test-video-recording-stop-wayland-calls-pkill-wf-recorder ()
  "Test that stopping video on Wayland calls pkill to kill wf-recorder."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (pkill-called nil)
            (pkill-args nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'call-process)
                   (lambda (program &rest args)
                     (when (equal program "pkill")
                       (setq pkill-called t)
                       (setq pkill-args args))
                     0))
                  ((symbol-function 'interrupt-process) (lambda (_proc) nil)))
          (cj/video-recording-stop)
          (should pkill-called)
          ;; Should call: pkill -INT wf-recorder
          (should (member "-INT" pkill-args))
          (should (member "wf-recorder" pkill-args)))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

(ert-deftest test-video-recording-stop-x11-does-not-call-pkill ()
  "Test that stopping video on X11 does NOT call pkill wf-recorder."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (pkill-called nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'call-process)
                   (lambda (program &rest _args)
                     (when (equal program "pkill")
                       (setq pkill-called t))
                     0))
                  ((symbol-function 'interrupt-process) (lambda (_proc) nil)))
          (cj/video-recording-stop)
          ;; Should NOT call pkill on X11
          (should-not pkill-called))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

(ert-deftest test-video-recording-start-wayland-kills-orphans ()
  "Test that starting video on Wayland kills orphan wf-recorder processes."
  (test-cleanup-setup)
  (unwind-protect
      (let ((pkill-called nil)
            (pkill-args nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'cj/recording--check-wf-recorder) (lambda () t))
                  ((symbol-function 'call-process)
                   (lambda (program &rest args)
                     (when (equal program "pkill")
                       (setq pkill-called t)
                       (setq pkill-args args))
                     0))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (make-process :name "fake-video" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-video video-recordings-dir)
          ;; Should call pkill to clean up orphans before starting
          (should pkill-called)
          (should (member "-INT" pkill-args))
          (should (member "wf-recorder" pkill-args))))
    (test-cleanup-teardown)))

(ert-deftest test-video-recording-start-x11-does-not-kill-orphans ()
  "Test that starting video on X11 does NOT call pkill."
  (test-cleanup-setup)
  (unwind-protect
      (let ((pkill-called nil))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'call-process)
                   (lambda (program &rest _args)
                     (when (equal program "pkill")
                       (setq pkill-called t))
                     0))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (make-process :name "fake-video" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-video video-recordings-dir)
          ;; Should NOT call pkill on X11
          (should-not pkill-called)))
    (test-cleanup-teardown)))

;;; Integration Tests - Verify actual process cleanup
;;; These tests require wf-recorder to be installed and a Wayland session

(defun test-cleanup--count-wf-recorder-processes ()
  "Count running wf-recorder processes."
  (let ((output (shell-command-to-string "pgrep -c wf-recorder 2>/dev/null || echo 0")))
    (string-to-number (string-trim output))))

(defun test-cleanup--get-wf-recorder-pids ()
  "Get list of wf-recorder PIDs."
  (let ((output (shell-command-to-string "pgrep wf-recorder 2>/dev/null")))
    (when (and output (not (string-empty-p (string-trim output))))
      (mapcar #'string-to-number (split-string (string-trim output) "\n")))))

(ert-deftest test-integration-video-recording-no-orphan-wf-recorder-after-stop ()
  "Test that no wf-recorder processes remain after stopping recording.
This is an integration test that requires wf-recorder and Wayland."
  :tags '(:integration :wayland)
  (skip-unless (executable-find "wf-recorder"))
  (skip-unless (cj/recording--wayland-p))
  (test-cleanup-setup)
  (unwind-protect
      (let ((initial-count (test-cleanup--count-wf-recorder-processes)))
        ;; Start recording
        (cj/ffmpeg-record-video video-recordings-dir)
        (sit-for 1.0)  ; Let it start

        ;; Should have at least one wf-recorder running
        (should (> (test-cleanup--count-wf-recorder-processes) initial-count))

        ;; Stop recording
        (cj/video-recording-stop)
        (sit-for 1.0)  ; Let cleanup complete

        ;; Should be back to initial count (no orphans)
        (should (= (test-cleanup--count-wf-recorder-processes) initial-count)))
    (test-cleanup-teardown)
    ;; Extra cleanup in case test failed mid-way
    (ignore-errors (call-process "pkill" nil nil nil "-INT" "wf-recorder"))))

(ert-deftest test-integration-video-recording-start-cleans-orphans ()
  "Test that starting a recording cleans up any orphan wf-recorder processes.
This is an integration test that requires wf-recorder and Wayland."
  :tags '(:integration :wayland)
  (skip-unless (executable-find "wf-recorder"))
  (skip-unless (cj/recording--wayland-p))
  (test-cleanup-setup)
  (unwind-protect
      (progn
        ;; Create an orphan wf-recorder (simulating a crash)
        (start-process "orphan-wf" nil "wf-recorder" "-c" "libx264" "-m" "matroska" "-f" "/dev/null")
        (sit-for 0.5)

        (let ((orphan-pids (test-cleanup--get-wf-recorder-pids)))
          (should orphan-pids)  ; Verify orphan was created

          ;; Start recording - should clean up orphan
          (cj/ffmpeg-record-video video-recordings-dir)
          (sit-for 1.0)

          ;; The orphan PIDs should no longer exist
          (let ((current-pids (test-cleanup--get-wf-recorder-pids)))
            ;; Old orphan should be gone (new recording process may exist)
            (dolist (orphan-pid orphan-pids)
              ;; Check if old orphan is still in current pids
              ;; It's OK if there's a NEW wf-recorder, just not the orphan
              (let ((still-running (member orphan-pid current-pids)))
                (should-not still-running))))

          ;; Clean up the recording we started
          (cj/video-recording-stop)
          (sit-for 0.5)))
    (test-cleanup-teardown)
    ;; Extra cleanup
    (ignore-errors (call-process "pkill" nil nil nil "-INT" "wf-recorder"))))

(ert-deftest test-integration-video-recording-multiple-start-stop-cycles ()
  "Test that multiple recording cycles don't accumulate orphan processes.
This is an integration test that requires wf-recorder and Wayland."
  :tags '(:integration :wayland)
  (skip-unless (executable-find "wf-recorder"))
  (skip-unless (cj/recording--wayland-p))
  (test-cleanup-setup)
  (unwind-protect
      (let ((initial-count (test-cleanup--count-wf-recorder-processes)))
        ;; Do 3 start/stop cycles
        (dotimes (_ 3)
          (cj/ffmpeg-record-video video-recordings-dir)
          (sit-for 0.5)
          (cj/video-recording-stop)
          (sit-for 0.5))

        ;; Should be back to initial count - no accumulated orphans
        (should (= (test-cleanup--count-wf-recorder-processes) initial-count)))
    (test-cleanup-teardown)
    ;; Extra cleanup
    (ignore-errors (call-process "pkill" nil nil nil "-INT" "wf-recorder"))))

(provide 'test-video-audio-recording-process-cleanup)
;;; test-video-audio-recording-process-cleanup.el ends here
