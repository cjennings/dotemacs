;;; test-video-audio-recording--device-sort-key.el --- Tests for device sort key -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--device-sort-key.
;; Verifies numeric sort key assignment: RUNNING=0, IDLE=1, SUSPENDED=2, muted=3.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording--device-sort-key-normal-running ()
  "RUNNING unmuted device returns 0 (highest priority)."
  (should (= 0 (cj/recording--device-sort-key "RUNNING" "no"))))

(ert-deftest test-video-audio-recording--device-sort-key-normal-idle ()
  "IDLE unmuted device returns 1."
  (should (= 1 (cj/recording--device-sort-key "IDLE" "no"))))

(ert-deftest test-video-audio-recording--device-sort-key-normal-suspended ()
  "SUSPENDED unmuted device returns 2."
  (should (= 2 (cj/recording--device-sort-key "SUSPENDED" "no"))))

(ert-deftest test-video-audio-recording--device-sort-key-normal-muted-overrides-state ()
  "Muted device returns 3 regardless of state."
  (should (= 3 (cj/recording--device-sort-key "RUNNING" "yes")))
  (should (= 3 (cj/recording--device-sort-key "IDLE" "yes")))
  (should (= 3 (cj/recording--device-sort-key "SUSPENDED" "yes"))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording--device-sort-key-boundary-nil-state ()
  "Nil state with unmuted returns 2 (default/available)."
  (should (= 2 (cj/recording--device-sort-key nil "no"))))

(ert-deftest test-video-audio-recording--device-sort-key-boundary-lowercase-state ()
  "Lowercase state is handled via upcase."
  (should (= 0 (cj/recording--device-sort-key "running" "no")))
  (should (= 1 (cj/recording--device-sort-key "idle" "no"))))

(ert-deftest test-video-audio-recording--device-sort-key-boundary-empty-state ()
  "Empty string state returns 2 (default)."
  (should (= 2 (cj/recording--device-sort-key "" "no"))))

;;; Error Cases

(ert-deftest test-video-audio-recording--device-sort-key-error-unknown-state ()
  "Unknown state string returns 2 (falls through to default)."
  (should (= 2 (cj/recording--device-sort-key "BOGUS" "no"))))

(ert-deftest test-video-audio-recording--device-sort-key-error-muted-nil-state ()
  "Muted with nil state still returns 3 (muted check is first)."
  (should (= 3 (cj/recording--device-sort-key nil "yes"))))

(provide 'test-video-audio-recording--device-sort-key)
;;; test-video-audio-recording--device-sort-key.el ends here
