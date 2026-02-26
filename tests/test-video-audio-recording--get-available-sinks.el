;;; test-video-audio-recording--get-available-sinks.el --- Tests for sink discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--get-available-sinks.
;; Verifies that available sinks are discovered correctly:
;; - Muted sinks are excluded
;; - Friendly descriptions from PulseAudio are used

;;; Code:

(require 'ert)

;; Stub dependencies before loading the module
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'video-audio-recording)

;;; Helper

(defun test-sinks--make-pactl-output (sinks)
  "Build fake `pactl list sinks' output from SINKS.
Each sink is (name description mute state)."
  (mapconcat (lambda (s)
               (format "Sink #1\n\tState: %s\n\tName: %s\n\tDescription: %s\n\tMute: %s\n"
                       (nth 3 s) (nth 0 s) (nth 1 s) (nth 2 s)))
             sinks ""))

;;; Normal Cases

(ert-deftest test-get-available-sinks-normal-filters-muted ()
  "Test that muted sinks are excluded from sink list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("active-sink" "Active Sink" "no" "RUNNING")
                  ("muted-sink" "Muted Sink" "yes" "SUSPENDED"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (= 1 (length sinks)))
      (should (equal "active-sink" (car (car sinks)))))))

(ert-deftest test-get-available-sinks-normal-uses-descriptions ()
  "Test that friendly descriptions are returned as cdr."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("raw-sink-name" "Friendly Sink Name" "no" "IDLE"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (equal "Friendly Sink Name" (cdr (car sinks)))))))

(ert-deftest test-get-available-sinks-normal-multiple-sinks ()
  "Test that multiple non-muted sinks are returned."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("sink-a" "JDS Labs" "no" "RUNNING")
                  ("sink-b" "Shure MV7+" "no" "SUSPENDED")
                  ("muted-sink" "Muted" "yes" "SUSPENDED"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (= 2 (length sinks))))))

;;; Boundary Cases

(ert-deftest test-get-available-sinks-boundary-empty-output ()
  "Test that empty pactl output returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should (null (cj/recording--get-available-sinks)))))

(ert-deftest test-get-available-sinks-boundary-all-muted ()
  "Test that if all sinks are muted, returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("muted-a" "Sink A" "yes" "SUSPENDED")
                  ("muted-b" "Sink B" "yes" "SUSPENDED"))))))
    (should (null (cj/recording--get-available-sinks)))))

(provide 'test-video-audio-recording--get-available-sinks)
;;; test-video-audio-recording--get-available-sinks.el ends here
