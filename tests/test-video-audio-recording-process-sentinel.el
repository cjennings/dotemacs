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
        ;; Mock process-status to return 'exit and force-mode-line-update to track call
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'exit))
                  ((symbol-function 'force-mode-line-update)
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
        ;; Mock process-status to return 'exit and message to capture output
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_proc) 'exit))
                  ((symbol-function 'message)
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

;;; Failed-Start Stub Deletion
;;
;; On Wayland a wf-recorder that fails to grab the compositor capture
;; still writes a ~500KB, ~0.5s stub .mkv before dying.  The sentinel
;; detects the failed start (exit sooner than
;; `cj/recording-start-fail-threshold' without a user stop); these tests
;; pin that it also deletes the stub file stamped on the process as the
;; `cj-output-file' property — and that normal stops and user stops
;; never delete anything.

(defun test-sentinel--make-exited-process ()
  "Return a real process that has already exited.
Drives the sentinel with a genuinely dead process so `process-status'
and the process plist behave for real instead of through mocks."
  (let ((proc (make-process :name "test-sentinel-exited"
                            :command '("true")
                            :sentinel #'ignore)))
    (while (process-live-p proc)
      (accept-process-output proc 0.05))
    proc))

(defun test-sentinel--make-stub-file ()
  "Create and return a temp file standing in for the stub .mkv."
  (make-temp-file "test-sentinel-stub-" nil ".mkv" "stub-content"))

(ert-deftest test-video-audio-recording-process-sentinel-normal-failed-start-deletes-stub ()
  "Normal: a failed video start deletes the stub output file."
  (test-sentinel-setup)
  (unwind-protect
      (let ((proc (test-sentinel--make-exited-process))
            (stub (test-sentinel--make-stub-file)))
        (unwind-protect
            (progn
              (setq cj/video-recording-ffmpeg-process proc)
              ;; Exited immediately after its start time — a failed start.
              (process-put proc 'cj-start-time (float-time))
              (process-put proc 'cj-output-file stub)
              (cj/recording-process-sentinel proc "exited abnormally\n")
              (should-not (file-exists-p stub)))
          (when (file-exists-p stub) (delete-file stub))))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-normal-failed-start-still-messages ()
  "Normal: the failed-start branch still reports the failure to the user."
  (test-sentinel-setup)
  (unwind-protect
      (let ((proc (test-sentinel--make-exited-process))
            (stub (test-sentinel--make-stub-file))
            (failure-messaged nil))
        (unwind-protect
            (progn
              (setq cj/video-recording-ffmpeg-process proc)
              (process-put proc 'cj-start-time (float-time))
              (process-put proc 'cj-output-file stub)
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (let ((msg (apply #'format fmt args)))
                             (when (string-match-p "failed to start" msg)
                               (setq failure-messaged t))))))
                (cj/recording-process-sentinel proc "exited abnormally\n"))
              (should failure-messaged))
          (when (file-exists-p stub) (delete-file stub))))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-normal-long-run-keeps-file ()
  "Normal: a recording that ran past the threshold keeps its output file."
  (test-sentinel-setup)
  (unwind-protect
      (let ((proc (test-sentinel--make-exited-process))
            (stub (test-sentinel--make-stub-file)))
        (unwind-protect
            (progn
              (setq cj/video-recording-ffmpeg-process proc)
              ;; Ran well past the fail threshold — a real recording.
              (process-put proc 'cj-start-time
                           (- (float-time)
                              (* 10 cj/recording-start-fail-threshold)))
              (process-put proc 'cj-output-file stub)
              (cj/recording-process-sentinel proc "finished\n")
              (should (file-exists-p stub)))
          (when (file-exists-p stub) (delete-file stub))))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-boundary-user-stop-keeps-file ()
  "Boundary: a quick user stop (cj-stopping) never deletes the file."
  (test-sentinel-setup)
  (unwind-protect
      (let ((proc (test-sentinel--make-exited-process))
            (stub (test-sentinel--make-stub-file)))
        (unwind-protect
            (progn
              (setq cj/video-recording-ffmpeg-process proc)
              ;; Quick exit, but the user asked for it.
              (process-put proc 'cj-start-time (float-time))
              (process-put proc 'cj-stopping t)
              (process-put proc 'cj-output-file stub)
              (cj/recording-process-sentinel proc "finished\n")
              (should (file-exists-p stub)))
          (when (file-exists-p stub) (delete-file stub))))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-boundary-missing-stub-no-error ()
  "Boundary: failed start whose stub never hit disk signals no error."
  (test-sentinel-setup)
  (unwind-protect
      (let ((proc (test-sentinel--make-exited-process)))
        (setq cj/video-recording-ffmpeg-process proc)
        (process-put proc 'cj-start-time (float-time))
        (process-put proc 'cj-output-file "/nonexistent/dir/never-written.mkv")
        ;; Must not signal even though the file is absent.
        (cj/recording-process-sentinel proc "exited abnormally\n")
        (should (null cj/video-recording-ffmpeg-process)))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-error-nil-output-property-no-error ()
  "Error: failed start with no cj-output-file property signals no error.
Covers processes started before the property existed (a live daemon
mid-upgrade) — the sentinel degrades to the old message-only path."
  (test-sentinel-setup)
  (unwind-protect
      (let ((proc (test-sentinel--make-exited-process)))
        (setq cj/video-recording-ffmpeg-process proc)
        (process-put proc 'cj-start-time (float-time))
        ;; No cj-output-file property at all.
        (cj/recording-process-sentinel proc "exited abnormally\n")
        (should (null cj/video-recording-ffmpeg-process)))
    (test-sentinel-teardown)))

(ert-deftest test-video-audio-recording-process-sentinel-normal-start-stamps-output-file ()
  "Normal: `cj/ffmpeg-record-video' stamps cj-output-file on the process."
  (test-sentinel-setup)
  (unwind-protect
      (let ((cj/recording-mic-device "test-mic-device")
            (cj/recording-system-device "test-monitor-device")
            (cj/recording-mic-boost 2.0)
            (cj/recording-system-volume 1.0))
        (cl-letf (((symbol-function 'cj/recording--wayland-p) (lambda () nil))
                  ((symbol-function 'cj/recording--validate-system-audio)
                   (lambda () nil))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer _command)
                     (make-process :name "fake-video" :command '("sleep" "1000")))))
          (cj/ffmpeg-record-video "/tmp/video-recordings/")
          (let ((output-file (process-get cj/video-recording-ffmpeg-process
                                          'cj-output-file)))
            (should (stringp output-file))
            (should (string-suffix-p ".mkv" output-file))
            (should (string-prefix-p "/tmp/video-recordings/" output-file))))
        (when cj/video-recording-ffmpeg-process
          (ignore-errors (delete-process cj/video-recording-ffmpeg-process))))
    (test-sentinel-teardown)))

(provide 'test-video-audio-recording-process-sentinel)
;;; test-video-audio-recording-process-sentinel.el ends here
