;;; test-video-audio-recording--get-available-sinks.el --- Tests for sink discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for cj/recording--get-available-sinks.
;; Verifies that available sinks are discovered correctly:
;; - Muted sinks are included (shown with [muted] label in UI)
;; - Friendly descriptions from PulseAudio are used
;; - PulseAudio state and mute status are included

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

(ert-deftest test-get-available-sinks-normal-includes-muted ()
  "Test that muted sinks are included in sink list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("active-sink" "Active Sink" "no" "RUNNING")
                  ("muted-sink" "Muted Sink" "yes" "SUSPENDED"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (= 2 (length sinks)))
      (should (equal "yes" (nth 3 (nth 1 sinks)))))))

(ert-deftest test-get-available-sinks-normal-uses-descriptions ()
  "Test that friendly descriptions are returned as second element."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("raw-sink-name" "Friendly Sink Name" "no" "IDLE"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (equal "Friendly Sink Name" (nth 1 (car sinks)))))))

(ert-deftest test-get-available-sinks-normal-includes-state ()
  "Test that PulseAudio state is returned as third element."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("sink-a" "Sink A" "no" "RUNNING"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (equal "RUNNING" (nth 2 (car sinks)))))))

(ert-deftest test-get-available-sinks-normal-includes-mute-status ()
  "Test that mute status is returned as fourth element."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("sink-a" "Sink A" "yes" "SUSPENDED"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (equal "yes" (nth 3 (car sinks)))))))

(ert-deftest test-get-available-sinks-normal-multiple-sinks ()
  "Test that multiple sinks are returned including muted."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (test-sinks--make-pactl-output
                '(("sink-a" "JDS Labs" "no" "RUNNING")
                  ("sink-b" "Shure MV7+" "no" "SUSPENDED")
                  ("muted-sink" "Muted" "yes" "SUSPENDED"))))))
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (= 3 (length sinks))))))

;;; Boundary Cases

(ert-deftest test-get-available-sinks-boundary-empty-output ()
  "Test that empty pactl output returns empty list."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should (null (cj/recording--get-available-sinks)))))

;;; Error Cases

(ert-deftest test-get-available-sinks-error-garbled-output ()
  "Test that garbled pactl output returns empty list, not an error."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "random garbage\nwith newlines\nbut no structure\n")))
    (should (null (cj/recording--get-available-sinks)))))

(ert-deftest test-get-available-sinks-error-missing-fields ()
  "Test that sink with partial fields does not crash."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "Sink #1\n\tState: RUNNING\n\tName: partial-sink\n")))
    ;; Missing Description and Mute — should not error
    (let ((sinks (cj/recording--get-available-sinks)))
      (should (listp sinks)))))

(provide 'test-video-audio-recording--get-available-sinks)
;;; test-video-audio-recording--get-available-sinks.el ends here
