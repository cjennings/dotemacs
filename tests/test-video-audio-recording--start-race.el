;;; test-video-audio-recording--start-race.el --- start-race fix tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the wf-recorder start-race fix: the poll that waits for a dying
;; wf-recorder to release the compositor capture before launching a new one, and
;; the fail-fast timing predicate that tells a 0.5s failed start from a real
;; recording.  The pgrep wrapper and the live start/stop wiring are exercised in
;; the daemon, not here; the poll is tested with an injected predicate so no real
;; process is needed.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module.
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

(declare-function cj/recording--start-failed-p "video-audio-recording-capture" (elapsed threshold))
(declare-function cj/recording--wait-for-no-wf-recorder "video-audio-recording-capture" (timeout-secs &optional running-p))

;;; ------------------------- cj/recording--start-failed-p ---------------------

(ert-deftest test-recording-start-failed-p-short-exit-is-failure ()
  "Normal: an exit well before the threshold is a failed start."
  (should (cj/recording--start-failed-p 0.5 1.5)))

(ert-deftest test-recording-start-failed-p-long-run-is-not-failure ()
  "Normal: a long-running recording that ends is not a failed start."
  (should-not (cj/recording--start-failed-p 30.0 1.5)))

(ert-deftest test-recording-start-failed-p-at-threshold-is-not-failure ()
  "Boundary: an exit exactly at the threshold is not counted as failed."
  (should-not (cj/recording--start-failed-p 1.5 1.5)))

;;; -------------------- cj/recording--wait-for-no-wf-recorder ------------------

(ert-deftest test-recording-wait-for-no-wf-recorder-clears ()
  "Normal: returns t once the injected predicate reports wf-recorder gone."
  (let ((n 0))
    (should (cj/recording--wait-for-no-wf-recorder
             2.0
             (lambda () (setq n (1+ n)) (< n 3))))))

(ert-deftest test-recording-wait-for-no-wf-recorder-already-clear ()
  "Boundary: an already-clear predicate returns t immediately."
  (should (cj/recording--wait-for-no-wf-recorder 2.0 (lambda () nil))))

(ert-deftest test-recording-wait-for-no-wf-recorder-times-out ()
  "Error: a predicate that never clears returns nil at the timeout."
  (should-not (cj/recording--wait-for-no-wf-recorder 0.15 (lambda () t))))

(provide 'test-video-audio-recording--start-race)
;;; test-video-audio-recording--start-race.el ends here
