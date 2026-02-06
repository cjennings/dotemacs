;;; test-video-audio-recording--wait-for-exit.el --- Tests for recording wait-for-exit -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `cj/recording--wait-for-exit', the function that polls
;; for process exit instead of using a blind `sit-for' delay.
;;
;; This function is critical to the recording fix: without proper waiting,
;; ffmpeg cannot finalize container metadata, resulting in zero-byte files.
;;
;; Tests use real short-lived processes (`true', `sleep') to exercise the
;; actual polling logic rather than mocking `process-live-p'.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--wait-for-exit-normal-process-exits-returns-t ()
  "Test that wait-for-exit returns t when process exits within timeout.
Uses `true' which exits immediately."
  (let ((proc (make-process :name "test-exit" :command '("true"))))
    (sit-for 0.2)  ; Let it exit
    (should (eq t (cj/recording--wait-for-exit proc 2)))))

(ert-deftest test-video-audio-recording--wait-for-exit-normal-polls-until-exit ()
  "Test that wait-for-exit polls and returns t after process exits.
Uses a short sleep to verify polling works across multiple iterations."
  (let ((proc (make-process :name "test-short" :command '("sleep" "0.2"))))
    (should (eq t (cj/recording--wait-for-exit proc 3)))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--wait-for-exit-boundary-already-dead-returns-t ()
  "Test that wait-for-exit returns t immediately for already-dead process."
  (let ((proc (make-process :name "test-dead" :command '("true"))))
    (sit-for 0.2)  ; Ensure it's dead
    (should-not (process-live-p proc))
    (should (eq t (cj/recording--wait-for-exit proc 1)))))

(ert-deftest test-video-audio-recording--wait-for-exit-boundary-zero-timeout-dead-process ()
  "Test that zero timeout with dead process returns t."
  (let ((proc (make-process :name "test-zero-dead" :command '("true"))))
    (sit-for 0.2)
    (should (eq t (cj/recording--wait-for-exit proc 0)))))

(ert-deftest test-video-audio-recording--wait-for-exit-boundary-zero-timeout-live-process ()
  "Test that zero timeout with live process returns nil (timed out)."
  (let ((proc (make-process :name "test-zero-live" :command '("sleep" "1000"))))
    (unwind-protect
        (should (eq nil (cj/recording--wait-for-exit proc 0)))
      (delete-process proc))))

(ert-deftest test-video-audio-recording--wait-for-exit-boundary-timeout-exceeded-returns-nil ()
  "Test that wait-for-exit returns nil when timeout is exceeded.
Uses a long-running process with a very short timeout."
  (let ((proc (make-process :name "test-timeout" :command '("sleep" "1000"))))
    (unwind-protect
        (should (eq nil (cj/recording--wait-for-exit proc 0.2)))
      (delete-process proc))))

;;; Error Cases

(ert-deftest test-video-audio-recording--wait-for-exit-error-deleted-process-returns-t ()
  "Test that wait-for-exit handles a deleted (not just exited) process.
A process killed via `delete-process' should be detected as not live."
  (let ((proc (make-process :name "test-deleted" :command '("sleep" "1000"))))
    (delete-process proc)
    (sit-for 0.1)
    (should (eq t (cj/recording--wait-for-exit proc 1)))))

(provide 'test-video-audio-recording--wait-for-exit)
;;; test-video-audio-recording--wait-for-exit.el ends here
