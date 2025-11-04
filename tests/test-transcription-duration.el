;;; test-transcription-duration.el --- Tests for duration calculation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--transcription-duration function
;; Categories: Normal cases, Boundary cases

;;; Code:

(require 'ert)
(require 'transcription-config)

;; ----------------------------- Normal Cases ----------------------------------

(ert-deftest test-cj/--transcription-duration-zero-seconds ()
  "Test duration calculation for current time (should be 00:00)."
  (let ((now (current-time)))
    (should (string= (cj/--transcription-duration now) "00:00"))))

(ert-deftest test-cj/--transcription-duration-30-seconds ()
  "Test duration calculation for 30 seconds ago."
  (let ((start-time (time-subtract (current-time) (seconds-to-time 30))))
    (should (string= (cj/--transcription-duration start-time) "00:30"))))

(ert-deftest test-cj/--transcription-duration-1-minute ()
  "Test duration calculation for 1 minute ago."
  (let ((start-time (time-subtract (current-time) (seconds-to-time 60))))
    (should (string= (cj/--transcription-duration start-time) "01:00"))))

(ert-deftest test-cj/--transcription-duration-2-minutes-30-seconds ()
  "Test duration calculation for 2:30 ago."
  (let ((start-time (time-subtract (current-time) (seconds-to-time 150))))
    (should (string= (cj/--transcription-duration start-time) "02:30"))))

(ert-deftest test-cj/--transcription-duration-10-minutes ()
  "Test duration calculation for 10 minutes ago."
  (let ((start-time (time-subtract (current-time) (seconds-to-time 600))))
    (should (string= (cj/--transcription-duration start-time) "10:00"))))

;; ----------------------------- Boundary Cases --------------------------------

(ert-deftest test-cj/--transcription-duration-59-seconds ()
  "Test duration just before 1 minute."
  (let ((start-time (time-subtract (current-time) (seconds-to-time 59))))
    (should (string= (cj/--transcription-duration start-time) "00:59"))))

(ert-deftest test-cj/--transcription-duration-1-hour ()
  "Test duration for 1 hour (60 minutes)."
  (let ((start-time (time-subtract (current-time) (seconds-to-time 3600))))
    (should (string= (cj/--transcription-duration start-time) "60:00"))))

(ert-deftest test-cj/--transcription-duration-format ()
  "Test that duration is always in MM:SS format with zero-padding."
  (let ((start-time (time-subtract (current-time) (seconds-to-time 65))))
    (let ((result (cj/--transcription-duration start-time)))
      (should (string-match-p "^[0-9][0-9]:[0-9][0-9]$" result)))))

(provide 'test-transcription-duration)
;;; test-transcription-duration.el ends here
