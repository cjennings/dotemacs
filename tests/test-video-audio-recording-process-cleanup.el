;;; test-video-audio-recording-process-cleanup.el --- Tests for recording process cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that verify proper cleanup of recording processes.
;;
;; Key behaviors tested:
;; - SIGINT is sent to the process group (negative PID), not just the shell
;; - Video stop waits for the process to actually exit before declaring done
;; - On Wayland, wf-recorder is killed AFTER ffmpeg exits (safety net)
;; - On X11, pkill is not called (no wf-recorder involved)
;; - Starting a recording cleans up orphan wf-recorder from previous crashes

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

;;; Unit Tests - Signal delivery

(ert-deftest test-video-recording-stop-sends-sigint-to-process-group ()
  "Test that stopping sends SIGINT to the process group via negative PID."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (signaled-pid nil)
            (signaled-sig nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'signal-process)
                   (lambda (pid sig)
                     (setq signaled-pid pid)
                     (setq signaled-sig sig)
                     0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc _timeout) t)))
          (cj/video-recording-stop)
          ;; Should send signal 2 (SIGINT) to negative PID (process group)
          (should signaled-pid)
          (should (< signaled-pid 0))
          (should (= signaled-sig 2)))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

;;; Unit Tests - Wayland wf-recorder cleanup

(ert-deftest test-video-recording-stop-wayland-kills-wf-recorder-before-signal ()
  "Test that on Wayland, pkill wf-recorder is called BEFORE signal-process.
Orderly pipeline shutdown requires killing the producer (wf-recorder) first
so ffmpeg sees EOF on its video input pipe and starts finalizing the file."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (call-order nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'call-process)
                   (lambda (program &rest args)
                     (when (equal program "pkill")
                       (push (cons 'pkill args) call-order))
                     0))
                  ((symbol-function 'signal-process)
                   (lambda (_pid _sig)
                     (push 'signal call-order)
                     0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc _timeout) t)))
          (cj/video-recording-stop)
          ;; call-order is reversed (push adds to front), so last element is first call
          (let ((order (reverse call-order)))
            ;; First call should be pkill (kill producer first)
            (should (consp (car order)))
            (should (eq 'pkill (caar order)))
            ;; signal-process should come after pkill
            (should (eq 'signal (cadr order)))))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

(ert-deftest test-video-recording-stop-wayland-calls-pkill-with-correct-args ()
  "Test that Wayland stop calls pkill -INT wf-recorder."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (pkill-args-list nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () t))
                  ((symbol-function 'call-process)
                   (lambda (program &rest args)
                     (when (equal program "pkill")
                       (push args pkill-args-list))
                     0))
                  ((symbol-function 'signal-process) (lambda (_pid _sig) 0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc _timeout) t)))
          (cj/video-recording-stop)
          ;; Should call pkill at least once with -INT wf-recorder
          (should pkill-args-list)
          (should (cl-some (lambda (args)
                             (and (member "-INT" args)
                                  (member "wf-recorder" args)))
                           pkill-args-list)))
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
                  ((symbol-function 'signal-process) (lambda (_pid _sig) 0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc _timeout) t)))
          (cj/video-recording-stop)
          (should-not pkill-called))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

;;; Unit Tests - Start cleans orphans

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
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
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
                     (make-process :name "fake-video" :command '("sleep" "1000"))))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (should-not pkill-called)))
    (test-cleanup-teardown)))

;;; Unit Tests - Wait for exit

(ert-deftest test-video-recording-stop-waits-for-process-exit ()
  "Test that stop waits for the process to exit instead of blind sit-for."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (wait-called nil)
            (wait-timeout nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'signal-process) (lambda (_pid _sig) 0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc timeout)
                     (setq wait-called t)
                     (setq wait-timeout timeout)
                     t)))
          (cj/video-recording-stop)
          (should wait-called)
          ;; Video uses 5 second timeout for MKV finalization
          (should (= wait-timeout 5)))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

(ert-deftest test-video-recording-stop-timeout-shows-warning ()
  "Test that video stop shows warning when process doesn't exit in time."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-video" :command '("sleep" "1000")))
            (warning-shown nil))
        (setq cj/video-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'signal-process) (lambda (_pid _sig) 0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc _timeout) nil))  ; Simulate timeout
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (let ((msg (apply #'format fmt args)))
                       (when (string-match-p "did not exit" msg)
                         (setq warning-shown t))))))
          (cj/video-recording-stop)
          (should warning-shown))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

(ert-deftest test-audio-recording-stop-timeout-shows-warning ()
  "Test that audio stop shows warning when process doesn't exit in time."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000")))
            (warning-shown nil))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'signal-process) (lambda (_pid _sig) 0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc _timeout) nil))  ; Simulate timeout
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (let ((msg (apply #'format fmt args)))
                       (when (string-match-p "did not exit" msg)
                         (setq warning-shown t))))))
          (cj/audio-recording-stop)
          (should warning-shown))
        (ignore-errors (delete-process fake-process)))
    (test-cleanup-teardown)))

(ert-deftest test-audio-recording-stop-waits-for-process-exit ()
  "Test that audio stop waits for the process to exit."
  (test-cleanup-setup)
  (unwind-protect
      (let ((fake-process (make-process :name "test-audio" :command '("sleep" "1000")))
            (wait-called nil)
            (wait-timeout nil))
        (setq cj/audio-recording-ffmpeg-process fake-process)
        (cl-letf (((symbol-function 'signal-process) (lambda (_pid _sig) 0))
                  ((symbol-function 'cj/recording--wait-for-exit)
                   (lambda (_proc timeout)
                     (setq wait-called t)
                     (setq wait-timeout timeout)
                     t)))
          (cj/audio-recording-stop)
          (should wait-called)
          ;; Audio uses 3 second timeout for M4A finalization
          (should (= wait-timeout 3)))
        (ignore-errors (delete-process fake-process)))
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
      (cl-letf (((symbol-function 'cj/recording--validate-system-audio)
                 (lambda () nil)))
        (let ((initial-count (test-cleanup--count-wf-recorder-processes)))
          (cj/ffmpeg-record-video video-recordings-dir)
          (sit-for 1.0)
          (should (> (test-cleanup--count-wf-recorder-processes) initial-count))
          (cj/video-recording-stop)
          (sit-for 1.0)
          (should (= (test-cleanup--count-wf-recorder-processes) initial-count))))
    (test-cleanup-teardown)
    (ignore-errors (call-process "pkill" nil nil nil "-INT" "wf-recorder"))))

(ert-deftest test-integration-video-recording-start-cleans-orphans ()
  "Test that starting a recording cleans up any orphan wf-recorder processes.
This is an integration test that requires wf-recorder and Wayland."
  :tags '(:integration :wayland)
  (skip-unless (executable-find "wf-recorder"))
  (skip-unless (cj/recording--wayland-p))
  (test-cleanup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--validate-system-audio)
                 (lambda () nil)))
        ;; Create an orphan wf-recorder (simulating a crash)
        (start-process "orphan-wf" nil "wf-recorder" "-c" "libx264" "-m" "matroska" "-f" "/dev/null")
        (sit-for 0.5)
        (let ((orphan-pids (test-cleanup--get-wf-recorder-pids)))
          (should orphan-pids)
          (cj/ffmpeg-record-video video-recordings-dir)
          (sit-for 1.0)
          (let ((current-pids (test-cleanup--get-wf-recorder-pids)))
            (dolist (orphan-pid orphan-pids)
              (let ((still-running (member orphan-pid current-pids)))
                (should-not still-running))))
          (cj/video-recording-stop)
          (sit-for 0.5)))
    (test-cleanup-teardown)
    (ignore-errors (call-process "pkill" nil nil nil "-INT" "wf-recorder"))))

(ert-deftest test-integration-video-recording-multiple-start-stop-cycles ()
  "Test that multiple recording cycles don't accumulate orphan processes.
This is an integration test that requires wf-recorder and Wayland."
  :tags '(:integration :wayland)
  (skip-unless (executable-find "wf-recorder"))
  (skip-unless (cj/recording--wayland-p))
  (test-cleanup-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'cj/recording--validate-system-audio)
                 (lambda () nil)))
        (let ((initial-count (test-cleanup--count-wf-recorder-processes)))
          (dotimes (_ 3)
            (cj/ffmpeg-record-video video-recordings-dir)
            (sit-for 0.5)
            (cj/video-recording-stop)
            (sit-for 0.5))
          (should (= (test-cleanup--count-wf-recorder-processes) initial-count))))
    (test-cleanup-teardown)
    (ignore-errors (call-process "pkill" nil nil nil "-INT" "wf-recorder"))))

(provide 'test-video-audio-recording-process-cleanup)
;;; test-video-audio-recording-process-cleanup.el ends here
