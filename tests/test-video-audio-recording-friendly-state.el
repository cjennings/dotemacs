;;; test-video-audio-recording-friendly-state.el --- Tests for cj/recording-friendly-state -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording-friendly-state function.
;; Tests conversion of technical pactl state names to user-friendly labels.

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

;; Now load the actual production module
(require 'video-audio-recording)

;;; Normal Cases

(ert-deftest test-video-audio-recording-friendly-state-normal-suspended-returns-ready ()
  "Test that SUSPENDED state converts to Ready."
  (should (string= "Ready" (cj/recording-friendly-state "SUSPENDED"))))

(ert-deftest test-video-audio-recording-friendly-state-normal-running-returns-active ()
  "Test that RUNNING state converts to Active."
  (should (string= "Active" (cj/recording-friendly-state "RUNNING"))))

(ert-deftest test-video-audio-recording-friendly-state-normal-idle-returns-ready ()
  "Test that IDLE state converts to Ready."
  (should (string= "Ready" (cj/recording-friendly-state "IDLE"))))

;;; Boundary Cases

(ert-deftest test-video-audio-recording-friendly-state-boundary-empty-string-returns-empty ()
  "Test that empty string passes through unchanged."
  (should (string= "" (cj/recording-friendly-state ""))))

(ert-deftest test-video-audio-recording-friendly-state-boundary-lowercase-suspended-returns-unchanged ()
  "Test that lowercase 'suspended' is not converted (case-sensitive)."
  (should (string= "suspended" (cj/recording-friendly-state "suspended"))))

(ert-deftest test-video-audio-recording-friendly-state-boundary-mixed-case-returns-unchanged ()
  "Test that mixed case 'Running' passes through unchanged."
  (should (string= "Running" (cj/recording-friendly-state "Running"))))

;;; Error Cases

(ert-deftest test-video-audio-recording-friendly-state-error-unknown-state-returns-unchanged ()
  "Test that unknown state passes through unchanged."
  (should (string= "UNKNOWN" (cj/recording-friendly-state "UNKNOWN"))))

(ert-deftest test-video-audio-recording-friendly-state-error-random-string-returns-unchanged ()
  "Test that random string passes through unchanged."
  (should (string= "foobar" (cj/recording-friendly-state "foobar"))))

(ert-deftest test-video-audio-recording-friendly-state-error-numeric-string-returns-unchanged ()
  "Test that numeric string passes through unchanged."
  (should (string= "12345" (cj/recording-friendly-state "12345"))))

(ert-deftest test-video-audio-recording-friendly-state-error-special-chars-returns-unchanged ()
  "Test that string with special characters passes through unchanged."
  (should (string= "!@#$%" (cj/recording-friendly-state "!@#$%"))))

(provide 'test-video-audio-recording-friendly-state)
;;; test-video-audio-recording-friendly-state.el ends here
